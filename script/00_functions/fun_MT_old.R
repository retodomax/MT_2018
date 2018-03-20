###########################################################################
## Project:  MT_2018
## Content:  old functions
## Date:     2018-03-16 17:13:08
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



# variables plotting function ---------------------------------------------

fun_plot_var_old <- function(df, format = "%d.%m") {
  opar1 <- FunPar(yaxs = "r", xaxs = "r", family = "", mar = c(0,5,0,2), tcl = 0.2)
  opar2 <- par(oma = c(3.5,0,3,0), mfrow = c(6,1)); on.exit(par(c(opar1, opar2)))
  for (i in levels(df[["device"]])) {
    lenviro_sel <- df %>%
      filter(device == i)
    var <- c("temp", "hum", "lux", "angle", "l_cum", "ler")
    var_name <- c("temp [\U00B0\U0043]", "hum [%]", "lux [lux]", "angle [\U00B0]", "length growth [mm]", "LER [mm/h]")
    for (j in seq_along(var)) {
      if(is_empty(lenviro_sel[[var[j]]])){
        plot.new()
      } else {
        plot(lenviro_sel$date_time, lenviro_sel[[var[j]]], type = "l",
             col = "darkgray", axes = F, ann = F)
      }
      FunAxisPOSIXct(xtimevec = lenviro_sel$date_time, format = format,
                     ytitle = var_name[j], xtitle = "", yline = 3.5, xlabels = F,
                     xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1)
    }
    if(!is_empty(lenviro_sel[["ler"]])){
      lo <- loess(ler ~ as.numeric(date_time), data = lenviro_sel, na.action = na.exclude, span = 0.2)
      lines(as.numeric(lenviro_sel$date_time), predict(lo), col='red', lwd=2)
    }
    FunAxisPOSIXct(xtimevec = lenviro_sel$date_time, format = format,
                   ytitle = var_name[i], xtitle = "Date", yline = 3.5, xlabels = T,
                   xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1)
    mtext(str_c("device:", i), side = 3, outer = T, adj = 0.05, cex = 1.5, col = "darkgray", line = 0.5)
  }
}





# plot ler model ----------------------------------------------------------

fun_plot_lerModel_old <- function(df) {
  model_summaries <- vector("list", length = length(levels(df[["device"]])))
  for(i in seq_along(levels(df[["device"]]))){
    level_name <- levels(df[["device"]])[i]
    subset <- df %>% 
      filter(device == level_name)
    # here:
    # test if there is at least one row with no missing data at ler, temp, vpd, lux and l_cum
    # if there is such a row: execute code below
    if(any(complete.cases(subset[c("ler","temp","vpd","lux","l_cum")]))){
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
    } else {
      opar1 <- FunPar(yaxs = "r", xaxs = "r", family = "", mar = c(0,5,0,2), tcl = 0.2)
      opar2 <- par(oma = c(3.5,0,3,0), mfrow = c(3,1)); on.exit(par(c(opar1, opar2)))
      plot.new()
      FunAxisPOSIXct(xtitle = "", ytitle = "LER [mm]", xtimevec = subset$date_time, xlabels = F,
                     xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1)
      plot.new()
      FunAxisPOSIXct(xtitle = "", ytitle = "length [mm]", xtimevec = subset$date_time, xlabels = F,
                     xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1)
      plot.new()
      FunAxisPOSIXct(xtitle = "Time", ytitle = "residuals", xtimevec = subset$date_time,
                     xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1)
      mtext(str_c("device:", level_name),
            side = 3, outer = T, adj = 0.05, cex = 1.5, col = "darkgray", line = 0.5)
      mtext(text = "no data available",
            side = 3, outer = T, adj = 0.95, cex = 0.6, col = "#4a1486", line = 0.5)
    }
  }
  # return model summary
  names(model_summaries) <- str_c("device",levels(df[["device"]]))
  invisible(model_summaries)
}