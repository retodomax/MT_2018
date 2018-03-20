###########################################################################
## Project:  MT_2018
## Content:  old stuff
## Date:     2018-03-14 12:06:54
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



# plot angle and ler in ggplot --------------------------------------------

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




# variables plotting function old ---------------------------------------------

fun_plot_var <- function(df, format = "%d.%m") {
  opar1 <- FunPar(yaxs = "r", xaxs = "r", family = "", mar = c(0,5,0,2), tcl = 0.2)
  opar2 <- par(oma = c(3.5,0,3,0), mfrow = c(6,1)); on.exit(par(c(opar1, opar2)))
  for (i in levels(df[["device"]])) {
    lenviro_sel <- df %>%
      filter(device == i)
    var <- c("temp", "hum", "lux", "angle", "l_cum", "ler")
    var_name <- c("temp [\U00B0\U0043]", "hum [%]", "lux [lux]", "angle [\U00B0]", "length growth [mm]", "LER [mm/h]")
    for (j in seq_along(var)) {
      plot(lenviro_sel$date_time, lenviro_sel[[var[j]]], type = "l",
           col = "darkgray", axes = F, ann = F)
      FunAxisPOSIXct(xtimevec = lenviro_sel$date_time, format = format,
                     ytitle = var_name[j], xtitle = "", yline = 3.5, xlabels = F,
                     xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1)
    }
    lo <- loess(ler ~ as.numeric(date_time), data = lenviro_sel, na.action = na.exclude, span = 0.2)
    lines(as.numeric(lenviro_sel$date_time), predict(lo), col='red', lwd=2)
    FunAxisPOSIXct(xtimevec = lenviro_sel$date_time, format = format,
                   ytitle = var_name[i], xtitle = "Date", yline = 3.5, xlabels = T,
                   xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1)
    mtext(str_c("device:", i), side = 3, outer = T, adj = 0.05, cex = 1.5, col = "darkgray", line = 0.5)
  }  
}



# plot ler model old ------------------------------------------------------

fun_plot_lerModel <- function(df) {
  model_summaries <- vector("list", length = length(levels(df[["device"]])))
  for(i in seq_along(levels(df[["device"]]))){
    level_name <- levels(df[["device"]])[i]
    subset <- df %>% 
      filter(device == level_name)
    lm_ler <- lm(ler ~ temp + vpd + lux + I(l_cum^2), data = subset)
    compl <- complete.cases(subset[c("ler", "temp","vpd", "lux", "l_cum")])
    subset$pred_ler <- NA
    subset$pred_ler[compl] <- predict(lm_ler)
    subset$residuals <- NA
    subset$residuals[compl] <- lm_ler$residuals
    subset <- subset %>% 
      mutate(pred_l_diff = subset$pred_ler*subset$time_diff/(60*60),
             pred_l_cum = cumsum(ifelse(is.na(pred_l_diff), 0, pred_l_diff)))
    
    # extract model summary values 
    estimates <- summary(lm_ler)$coefficients[,1] %>% 
      map(format, digits = 2, nsmall = 2) %>% 
      unlist
    
    mod_labels <- names(estimates)
    mod_labels <- paste(c("",mod_labels, "", "R2", "adjusted.R2", "", "P"), collapse = "\n")
    
    R2 <-  format(summary(lm_ler)$r.squared,digits=2,nsmall=2)
    AR2 <- format(summary(lm_ler)$adj.r.squared,digits=2,nsmall=2)
    SLM <- summary(lm_ler)
    #what does this p value mean?
    PV <- format(pf(SLM$fstatistic[1], SLM$fstatistic[2], SLM$fstatistic[3], lower.tail = FALSE) ,digits=2,nsmall=2)
    estimates <- paste(c("Estimate",estimates, "", R2, AR2, "", PV), collapse = "\n")
    
    pval <- summary(lm_ler)$coefficients[,4] %>% 
      map(format, digits = 2, nsmall = 2) %>% 
      unlist
    pval <- paste(c("P", pval, "","","","",""),collapse = "\n")
    
    # plot model vs observed
    opar1 <- FunPar(yaxs = "r", xaxs = "r", family = "", mar = c(0,5,0,2), tcl = 0.2)
    opar2 <- par(oma = c(3.5,0,3,0), mfrow = c(3,1)); on.exit(par(c(opar1, opar2)))
    plot(subset$date_time ,subset$ler, col = "grey", type = "l", lwd = 1.5,
         axes = F, ann = F)
    FunAxisPOSIXct(xtitle = "", ytitle = "LER [mm]", xtimevec = subset$date_time, xlabels = F,
                   xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1)
    lines(subset$date_time, subset$pred_ler,col="#807dba", lwd = 2)
    
    text(x=FunXPosition(0.3),y= FunYPosition(0.3),labels=mod_labels,pos=4, cex = 0.8)
    text(x=FunXPosition(0.5),y= FunYPosition(0.3),labels=estimates,pos=4, cex = 0.8)
    text(x=FunXPosition(0.7),y= FunYPosition(0.3),labels=pval,pos=4, cex = 0.8)
    
    plot(subset$date_time, subset$l_cum, col = "grey", type = "l", lwd = 2,
         axes = F, ann = F)
    FunAxisPOSIXct(xtitle = "", ytitle = "length [mm]", xtimevec = subset$date_time, xlabels = F,
                   xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1)
    lines(subset$date_time, subset$pred_l_cum, col = "#807dba", lwd = 2)
    
    plot(subset$date_time, subset$residuals, axes = F, ann = F,
         panel.first = abline(h = 0, col = "#807dba"),
         col = alpha("black", 0.1))
    FunAxisPOSIXct(xtitle = "Time", ytitle = "residuals", xtimevec = subset$date_time,
                   xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1)
    mtext(str_c("device:", level_name),
          side = 3, outer = T, adj = 0.05, cex = 1.5, col = "darkgray", line = 0.5)
    mtext(text = str_c(as.character(lm_ler$call[1:2]), collapse = "(): "),
          side = 3, outer = T, adj = 0.95, cex = 0.6, col = "#4a1486", line = 0.5)
    # storing output
    model_summaries[[i]] <- SLM
  }
  # return model summary
  names(model_summaries) <- str_c("device",levels(df[["device"]]))
  invisible(model_summaries)
}






# interesting sections ----------------------------------------------------
# from setup_experiment9.R

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


