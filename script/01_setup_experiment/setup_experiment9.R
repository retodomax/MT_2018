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
source("script/00_functions/fun_MT_old.R")


# import data -------------------------------------------------------------


# problem with missing tabs => complicate method for import

folder <- "A/Mar-03-2018"
values_to_cut <- 20


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
  slice(values_to_cut:length(date_time)+1) %>%
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

# pdf("fig/setup_experiment_var.pdf", width = 4, height = 7, bg = "white")
fun_plot_var_old(df = lenviro)
# dev.off()




# model -------------------------------------------------------------------

# pdf("fig/setup_experiment_ler_model.pdf", width = 4, height = 7, bg = "white")
mod_summary <- fun_plot_lerModel_old(lenviro)
# dev.off()



# 4) apply the same plots to aggregated data
# - the fun_plot_var() function
# - the fun_plot_lerModel() function
# => send both outputs to Steven


# 6) go further with chapter "leaf elongation rate per termal time"
# 7) write R-script: create_desing_file.R



# leaf elongation rate per termal time ------------------------------------


# regression to see T0 and a

# 1) make a function out of that
# 2) applie it to new data

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





# aggregate to 20 min -----------------------------------------------------

lenviro <- lenviro %>%
  arrange(date_time) %>%
  group_by(device) %>%
  mutate(time_period = c(rep(seq_len(n() %/% 5), each=5), rep(NA,n() %% 5)))


ag_lenviro <- lenviro %>%
  group_by(device, time_period) %>%
  summarise(date_time = max(date_time, na.rm = T),
            time_diff = sum(time_diff, na.rm = T),
            lux = mean(lux, na.rm = T),
            temp = mean(temp, na.rm = T),
            vpd = mean(vpd, na.rm = T),
            hum = mean(hum, na.rm = T),
            angle = max(angle, na.rm = T),
            l_diff = sum(l_diff, na.rm = T),
            l_cum = max(l_cum, na.rm = T),
            ler = l_diff/time_diff*60*60) %>%
  mutate(daytime = as.numeric(paste0(hour(date_time), str_pad(minute(date_time),2,pad = "0"))),
         daytime = between(daytime, 530, 2300))



# pdf("fig/setup_experiment_var_ag.pdf", width = 4, height = 7, bg = "white")
fun_plot_var_old(df = lenviro)
# dev.off()

# pdf("fig/setup_experiment_ler_model_ag.pdf", width = 4, height = 7, bg = "white")
mod_summary <- fun_plot_lerModel_old(ag_lenviro)
# dev.off()



# Appendix  -----------------------------------------------------------













