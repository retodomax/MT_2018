###########################################################################
## Project:  MT_2018
## Content:  setup_experiment
## Date:     2018-03-06 17:49:40
## Author:   Reto Zihlmann <retozihlmann@outlook.com>
###########################################################################


# packages ----------------------------------------------------------------

library(tidyverse)
library(stringr)
library(magrittr)
library(lubridate)
library(hms)
library(forcats)
library(FunRZ)
Sys.setlocale("LC_ALL", "English")
source("script/00_functions/fun_MT.R")


# import data -------------------------------------------------------------


# problem with missing tabs => complicate method for import

folder <- "Mar-03-2017"
my_path <- dir(str_c("data/01_lenviro/", folder), pattern = "\\.txt$")
lenviro <- vector("list", length(my_path))
for (i in seq_along(my_path)) {
  # read in as single string
  file <- read_file(str_c("data/01_lenviro/", folder, "/", my_path[i])) %>%
    # match everything but digits, -, :, ., space, newline and replace with tab
    str_replace_all("[^[:digit:]-:. \\n\\r]+", "\t")
  # check if it is empty, otherways read_tsv
  lenviro[[i]] <- 
    if (file == ""){
      NULL
    } else {
      read_tsv(file, col_names = c("date_time", "device", "angle", "raw_angle",
                                   "temp", "hum", "lux"))
    }
}
lenviro <- reduce(lenviro, rbind)
lenviro$device <- as_factor(as.character(lenviro$device))
# save(lenviro, file = "data/01_lenviro/Mar-03-2017/lenviro_20170303.RData")
# load("data/01_lenviro/Mar-03-2017/lenviro_20170303.RData")


# one problem: if there are multiple devices (robots) with the same number but
# different letter (e.g. A7 B7 C7) they will all be reduced to device 7!
# I suggest to rename the devices to 1.7, 2.7, 3.7, etc.
# or we find a solution for the missing tabs

# stevens solution: take the name of the replicate out of the folder name





# LER ---------------------------------------------------------------------


# find inverse wheel cases
inverse <- lenviro %>% 
  group_by(device) %>% 
  arrange(date_time) %>% 
  mutate(angle_diff = angle - lag(angle)) %>% 
  filter(angle_diff != 0) %>%
  summarise(mean_angle_diff = mean(angle_diff, trim = 0.05, na.rm = T),
            inverse_wheel = mean_angle_diff < 0 ) %>% 
  filter(inverse_wheel)

# define new variables angle_diff, time_diff, l_diff, ler and id
# discard the first 20 values of each robot (influenced by the experimenter)
lenviro <- lenviro %>% 
  arrange(date_time) %>% 
  mutate(id = seq_along(date_time)) %>% 
  group_by(device) %>% 
  mutate(daytime = as.numeric(paste0(hour(date_time), str_pad(minute(date_time),2,pad = "0"))),
         daytime = between(daytime, 530, 2300),
         angle_diff = angle - lag(angle),
         angle_diff = ifelse(device %in% inverse$device, -angle_diff, angle_diff),
         angle_diff = ifelse(angle_diff < -270, 360+angle_diff, angle_diff),
         time_diff = as.numeric(difftime(date_time, lag(date_time), units = c("secs"))),
         l_diff = angle_diff*(8*pi/360),
         ler = l_diff/time_diff*60*60) %>% 
  slice(20:length(date_time)) %>%
  arrange(date_time) %>% 
  select(id, everything())


# VPD ---------------------------------------------------------------------

#https://physics.stackexchange.com/questions/4343/how-can-i-calculate-vapor-
# pressure-deficit-from-temperature-and-relative-humidit


lenviro <- lenviro %>% 
  mutate(es = 0.6108 * exp(17.27 * temp / (temp + 237.3)),
         vpd = (1- (hum / 100)) * es) # in kPa




# exclude outliers --------------------------------------------------------

lenviro <- lenviro %>% 
  group_by(device) %>% 
  mutate(lux = ifelse(abs(lux-mean(lux)) < 4*ifelse(sd(lux, na.rm = T) == 0,
                                                    Inf, sd(lux, na.rm = T)),
                      lux, NA),
         temp = ifelse(abs(temp-mean(temp)) < 4*sd(temp), temp, NA),
         hum = ifelse(abs(hum-mean(hum)) < 4*sd(hum), hum, NA),
         vpd = ifelse(abs(vpd-mean(vpd)) < 4*sd(vpd), vpd, NA),
         l_diff = ifelse(abs(l_diff-mean(l_diff)) < 2*sd(l_diff), l_diff, NA),
         ler = ifelse(abs(ler-mean(ler)) < 2*sd(ler), ler, NA))




# cumulative length -------------------------------------------------------

lenviro <- lenviro %>% 
  group_by(device) %>% 
  arrange(date_time) %>% 
  mutate(l_cum = cumsum(ifelse(is.na(l_diff), 0, l_diff)))



# plot var ----------------------------------------------------------------

# pdf("fig/setup_experiment_var_new_devices3.pdf", width = 4, height = 7, bg = "white")
fun_plot_var(df = lenviro)
# dev.off()



# leaf elongation rate per termal time ------------------------------------


# regression to see T0 and a
opar <- par(mfrow = c(3,3), mar = c(2,2,2,2), oma = c(4,4,0,0))
lm_night_ler <- vector("list", length = length(levels(lenviro$device)))
for(i in seq_along(levels(lenviro$device))){
  name <- levels(lenviro$device)[[i]]
  data <- lenviro %>%
    filter(!daytime) %>%
    filter(device == name)
  plot(ler ~ temp, data = data, xlim = c(22,27.2), ylim = c(0,7.2))
  lm_night_ler[[i]] <- FunRegressionPlot(x = data$temp, y = data$ler,
                                         edge = c(0.6,0.9), col = "darkgray")
  legend("topleft", legend = paste("dev =", name), bty = "n")
}
lm_night_ler <- setNames(lm_night_ler, levels(lenviro$device))
mtext("temp", side = 1, outer = T, line = 2)
mtext("ler", side = 2, outer = T, line = 2)
par(opar)

# extract T0 and a
night_ler <- tibble(device = names(lm_night_ler),
                    intercept = lm_night_ler %>% 
                      map("coefficients") %>% 
                      map("(Intercept)") %>% 
                      unlist(),
                    a = lm_night_ler %>% 
                      map("coefficients") %>% 
                      map("x") %>% 
                      unlist())
night_ler <- night_ler %>% 
  mutate(T0 = -intercept/a)

night_ler





# model -------------------------------------------------------------------


for(i in levels(lenviro$device)[2:9]){
  subset <- lenviro %>% 
    filter(device == i)
  
  lm_ler <- lm(ler ~ temp + vpd + lux + I(l_cum^2), data = subset)
  compl <- complete.cases(subset[c("ler", "temp","vpd", "lux", "l_cum")])
  subset$pred_ler <- NA
  subset$pred_ler[compl] <- predict(lm_ler)
  subset$residuals <- NA
  subset$residuals[compl] <- lm_ler$residuals
  
  subset <- subset %>% 
    mutate(pred_l_diff = subset$pred_ler*subset$time_diff/(60*60),
           pred_l_cum = cumsum(ifelse(is.na(pred_l_diff), 0, pred_l_diff)))
  
  ################
  Im <- format(summary(lm_ler)$coefficients[1,1],digits=2,nsmall=2)
  Tm <- format(summary(lm_ler)$coefficients[2,1],digits=2,nsmall=2)
  Vm <- format(summary(lm_ler)$coefficients[3,1],digits=2,nsmall=2)
  Lm <- format(summary(lm_ler)$coefficients[4,1],digits=2,nsmall=2)
  Em <- format(summary(lm_ler)$coefficients[5,1],digits=2,nsmall=2)
  
  
  Ip <- format(summary(lm_ler)$coefficients[1,4],digits=2,nsmall=2)
  Tp <- format(summary(lm_ler)$coefficients[2,4],digits=2,nsmall=2)
  Vp <- format(summary(lm_ler)$coefficients[3,4],digits=2,nsmall=2)
  Lp <- format(summary(lm_ler)$coefficients[4,4],digits=2,nsmall=2)
  Ep <- format(summary(lm_ler)$coefficients[5,4],digits=2,nsmall=2)
  
  R2 <-  format(summary(lm_ler)$r.squared,digits=2,nsmall=2)
  AR2 <- format(summary(lm_ler)$adj.r.squared,digits=2,nsmall=2)
  SLM <- summary(lm_ler)
  PV <- format(pf(SLM$fstatistic[1], SLM$fstatistic[2], SLM$fstatistic[3], lower.tail = FALSE) ,digits=2,nsmall=2)
  
  
  # coefficients
  COEF <- paste(c("Estimate",Im,Tm,Vm,Lm,Em,"",R2,AR2,"",PV),collapse="\n")
  COMPO <- paste(c("","Intercept","Temperature","VPD","Light","Length","","R2","adjusted.R2","","P"),collapse="\n")
  PVALS <- paste(c("P",Ip,Tp,Vp,Lp,Ep,"","","","",""),collapse="\n")
  ###############
  
  opar1 <- FunPar(yaxs = "r", xaxs = "r", family = "", mar = c(0,5,0,2), tcl = 0.2)
  opar2 <- par(oma = c(3.5,0,3,0), mfrow = c(3,1)) #; on.exit(par(c(opar1, opar2)))
  plot(subset$date_time ,subset$ler, col = "grey", type = "l", lwd = 1.5,
       axes = F, ann = F)
  FunAxisPOSIXct(xtitle = "", ytitle = "LER [mm]", xtimevec = subset$date_time, xlabels = F)
  lines(subset$date_time, subset$pred_ler,col="darkslategray2", lwd = 2)
  
  text(x=FunXPosition(0.3),y= FunYPosition(0.3),labels=COMPO,pos=4)
  text(x=FunXPosition(0.5),y= FunYPosition(0.3),labels=COEF,pos=4)
  text(x=FunXPosition(0.7),y= FunYPosition(0.3),labels=PVALS,pos=4)
  
  plot(subset$date_time, subset$l_cum, col = "grey", type = "l", lwd = 2,
       axes = F, ann = F)
  FunAxisPOSIXct(xtitle = "", ytitle = "length [mm]", xtimevec = subset$date_time, xlabels = F)
  lines(subset$date_time, subset$pred_l_cum, col = "darkslategray2", lwd = 2)
  
  plot(subset$date_time, subset$residuals, axes = F, ann = F, panel.first = abline(h = 0, col = "darkslategray2"),
       col = alpha("black", 0.1))
  FunAxisPOSIXct(xtitle = "Time", ytitle = "residuals", xtimevec = subset$date_time)
  mtext(str_c("device:", i), side = 3, outer = T, adj = 0.05, cex = 1.5, col = "darkgray", line = 0.5)
}

##################################





levels(lenviro$device)
subset <- lenviro %>% 
  filter(device == 13)

lm_ler <- lm(ler ~ temp + vpd + lux + I(l_cum^2), data = subset)
compl <- complete.cases(subset[c("ler", "temp","vpd", "lux", "l_cum")])
subset$pred_ler <- NA
subset$pred_ler[compl] <- predict(lm_ler)
subset$residuals <- NA
subset$residuals[compl] <- lm_ler$residuals

subset <- subset %>% 
  mutate(pred_l_diff = subset$pred_ler*subset$time_diff/(60*60),
         pred_l_cum = cumsum(ifelse(is.na(pred_l_diff), 0, pred_l_diff)))

################ make it more dense
Im <- format(summary(lm_ler)$coefficients[1,1],digits=2,nsmall=2)
Tm <- format(summary(lm_ler)$coefficients[2,1],digits=2,nsmall=2)
Vm <- format(summary(lm_ler)$coefficients[3,1],digits=2,nsmall=2)
Lm <- format(summary(lm_ler)$coefficients[4,1],digits=2,nsmall=2)
Em <- format(summary(lm_ler)$coefficients[5,1],digits=2,nsmall=2)


Ip <- format(summary(lm_ler)$coefficients[1,4],digits=2,nsmall=2)
Tp <- format(summary(lm_ler)$coefficients[2,4],digits=2,nsmall=2)
Vp <- format(summary(lm_ler)$coefficients[3,4],digits=2,nsmall=2)
Lp <- format(summary(lm_ler)$coefficients[4,4],digits=2,nsmall=2)
Ep <- format(summary(lm_ler)$coefficients[5,4],digits=2,nsmall=2)

R2 <-  format(summary(lm_ler)$r.squared,digits=2,nsmall=2)
AR2 <- format(summary(lm_ler)$adj.r.squared,digits=2,nsmall=2)
SLM <- summary(lm_ler)
PV <- format(pf(SLM$fstatistic[1], SLM$fstatistic[2], SLM$fstatistic[3], lower.tail = FALSE) ,digits=2,nsmall=2)


# coefficients
COEF <- paste(c("Estimate",Im,Tm,Vm,Lm,Em,"",R2,AR2,"",PV),collapse="\n")
COMPO <- paste(c("","Intercept","Temperature","VPD","Light","Length","","R2","adjusted.R2","","P"),collapse="\n")
PVALS <- paste(c("P",Ip,Tp,Vp,Lp,Ep,"","","","",""),collapse="\n")
###############

opar1 <- FunPar(yaxs = "r", xaxs = "r", family = "", mar = c(0,5,0,2), tcl = 0.2)
opar2 <- par(oma = c(3.5,0,3,0), mfrow = c(3,1)) #; on.exit(par(c(opar1, opar2)))
plot(subset$date_time ,subset$ler, col = "grey", type = "l", lwd = 1.5,
     axes = F, ann = F)
FunAxisPOSIXct(xtitle = "", ytitle = "LER [mm]", xtimevec = subset$date_time, xlabels = F)
lines(subset$date_time, subset$pred_ler,col="darkslategray2", lwd = 2)

text(x=FunXPosition(0.3),y= FunYPosition(0.3),labels=COMPO,pos=4)
text(x=FunXPosition(0.5),y= FunYPosition(0.3),labels=COEF,pos=4)
text(x=FunXPosition(0.7),y= FunYPosition(0.3),labels=PVALS,pos=4)

plot(subset$date_time, subset$l_cum, col = "grey", type = "l", lwd = 2,
     axes = F, ann = F)
FunAxisPOSIXct(xtitle = "", ytitle = "length [mm]", xtimevec = subset$date_time, xlabels = F)
lines(subset$date_time, subset$pred_l_cum, col = "darkslategray2", lwd = 2)

plot(subset$date_time, subset$residuals, axes = F, ann = F, panel.first = abline(h = 0, col = "darkslategray2"),
     col = alpha("black", 0.1))
FunAxisPOSIXct(xtitle = "Time", ytitle = "residuals", xtimevec = subset$date_time)
mtext(str_c("device:", "11"), side = 3, outer = T, adj = 0.05, cex = 1.5, col = "darkgray", line = 0.5)


## NEXT:
# 1) make code above more dense
# -- make plots nicer (reto axis)
# -- make it repeatable for other subsets (make a function out of it)
# the function should:
# - loop over all devices
# - return the output of lm() in a list
# - plot all important plots with text
# 2) loop over all devices
# 3) interpret output
# 4) apply the same plots to aggregated data
# 5) adapt last chapter in stevens code



# aggregate to 20 min -----------------------------------------------------



# visualisation -----------------------------------------------------------


library(gridExtra)
# pdf("fig/ler_setup3.pdf", width = 9, height = 7, bg = "white")
for (i in levels(lenviro$device)) {
  p1 <- lenviro %>%
    filter(device == i) %>%
    ggplot(aes(x = date_time, y = angle)) +
    geom_line() +
    labs(title = i) +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
  p2 <- lenviro %>%
    filter(device == i) %>%
    ggplot(aes(x = date_time, y = ler)) +
    geom_line() +
    labs(title = i) +
    coord_cartesian(ylim = c(-0.001, 0.0045)) +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
    geom_smooth()
  print(grid.arrange(p1, p2, nrow=2))
}
# dev.off()




# show steven:
# what could this difference between angle and raw_angle be?
lenviro %>%
  filter(device == 13) %>%
  filter(between(id, 965, 1100)) %>%
  View()



# moste differences between angle and raw_angle are due to missing numbers/points
lenviro %>%
  filter(abs(angle-raw_angle) > 4) %>%
  View()


# what could have happend here? probably dureing the time we were there
lenviro %>%
  filter(device == 12) %>%
  filter(between(id, 9746, 9836)) %>%
  View()




# plot all variables ggplot------------------------------------------------

## ggplot

# pdf("fig/ler_setup_var1.pdf", width = 9, height = 7, bg = "white")
lenviro_sel <- lenviro %>%
  filter(device == 7)
p1 <- lenviro_sel %>%
  ggplot(aes(date_time, y = temp)) +
  geom_line()
p2 <- lenviro_sel %>%
  ggplot(aes(date_time, y = hum)) +
  geom_line()
p3 <- lenviro_sel %>%
  ggplot(aes(date_time, y = lux)) +
  geom_line()
p4 <- lenviro_sel %>%
  ggplot(aes(date_time, y = angle)) +
  geom_line()
p5 <- lenviro_sel %>%
  ggplot(aes(date_time, y = l_cum)) +
  geom_line()
p6 <- lenviro_sel %>%
  ggplot(aes(date_time, y = ler)) +
  geom_line()
grid.arrange(p1, p2, p3, p4, p5, p6, nrow=6)
# dev.off()


# plot all variables in base graphics -------------------------------------


# pdf("fig/ler_setup_var_ag_1.pdf", width = 4, height = 7, bg = "white")
fun_plot_var(df = ag_lenviro)
# dev.off()

# pdf("fig/ler_setup_var_outliers2.pdf", width = 4, height = 7, bg = "white")
fun_plot_var(df = lenviro)
# dev.off()





