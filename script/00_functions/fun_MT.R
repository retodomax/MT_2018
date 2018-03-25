###########################################################################
## Project:  MT_2018
## Content:  functions for MT_2018
## Date:     2018-03-11 18:25:35
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




# variables plotting function separated by experiment ---------------------

fun_plot_var <- function(df, format = "%d.%m") {
  opar1 <- FunPar(yaxs = "r", xaxs = "r", family = "", mar = c(0,5,0,2), tcl = 0.2)
  opar2 <- par(oma = c(3.5,0,3,0), mfrow = c(6,1)); on.exit(par(c(opar1, opar2)))
  for (i in levels(df[["experiment"]])) {
    lenviro_sel <- df %>%
      filter(experiment == i)
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
                   ytitle = "", xtitle = "Date", yline = 3.5, xlabels = T,
                   xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1)
    mtext(str_c("exp: ", i), side = 3, outer = T, adj = 0.05, cex = 0.8, col = "darkgray", line = 0.5)
  }
}




# plot ler model separated by experiment ----------------------------------

fun_plot_lerModel <- function(df) {
  model_summaries <- vector("list", length = length(levels(df[["experiment"]])))
  for(i in seq_along(levels(df[["experiment"]]))){
    level_name <- levels(df[["experiment"]])[i]
    subset <- df %>% 
      filter(experiment == level_name)
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
      
      text(x=grconvertX(0.3, from = "npc"),y= grconvertY(0.3, from = "npc"),
           labels=mod_labels,pos=4, cex = 0.8)
      text(x=grconvertX(0.5, from = "npc"),y= grconvertY(0.3, from = "npc"),
           labels=estimates,pos=4, cex = 0.8)
      text(x=grconvertX(0.7, from = "npc"),y= grconvertY(0.3, from = "npc"),
           labels=pval,pos=4, cex = 0.8)
      
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
      mtext(str_c("exp: ", level_name),
            side = 3, outer = T, adj = 0.05, cex = 0.8, col = "darkgray", line = 0.5)
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
      mtext(str_c("exp: ", level_name),
            side = 3, outer = T, adj = 0.05, cex = 0.8, col = "darkgray", line = 0.5)
      mtext(text = "no data available",
            side = 3, outer = T, adj = 0.95, cex = 0.6, col = "#4a1486", line = 0.5)
    }
  }
  # return model summary
  names(model_summaries) <- levels(df[["experiment"]])
  invisible(model_summaries)
}






# combined function -------------------------------------------------------

fun_plot_overview <- function(df, format_date = "") {
  opar1 <- FunPar(yaxs = "r", xaxs = "r", family = "", mar = c(0,5,0,2), tcl = 0.2)
  opar2 <- par(oma = c(3.5,0,3,0)); on.exit(par(c(opar1, opar2)))
  layout(matrix(c(1:6, rep(7:9, each = 2)), ncol = 2))
  # preallocate output
  model_summaries <- vector("list", length = length(levels(df[["experiment"]])))
  # loop
  for (i in seq_along(levels(df[["experiment"]]))) {
    level_name <- levels(df[["experiment"]])[i]
    subset <- df %>%
      filter(experiment == level_name)
    
    var <- c("temp", "hum", "lux", "angle", "l_cum", "ler")
    var_name <- c("temp [\U00B0\U0043]", "hum [%]", "lux [lux]", "angle [\U00B0]", "length growth [mm]", "LER [mm/h]")
    for (j in seq_along(var)) {
      if(is_empty(subset[[var[j]]])){
        plot.new()
      } else {
        plot(subset$date_time, subset[[var[j]]], type = "l",
             col = "darkgray", axes = F, ann = F)
      }
      FunAxisPOSIXct(xtimevec = subset$date_time, format = format_date,
                     ytitle = var_name[j], xtitle = "", yline = 3.5, xlabels = F,
                     xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1)
    }
    if(!is_empty(subset[["ler"]])){
      lo <- loess(ler ~ as.numeric(date_time), data = subset, na.action = na.exclude, span = 0.2)
      lines(as.numeric(subset$date_time), predict(lo), col='red', lwd=2)
    }
    FunAxisPOSIXct(xtimevec = subset$date_time, format = format_date,
                   ytitle = "", xtitle = "Time", yline = 3.5, xlabels = T,
                   xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1)
    mtext(str_c("exp: ", level_name), side = 3, outer = T, adj = 0.05, cex = 0.8, col = "darkgray", line = 0.5)
    
    #### model part
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
      plot(subset$date_time ,subset$ler, col = "grey", type = "l", lwd = 1.5,
           axes = F, ann = F)
      FunAxisPOSIXct(xtitle = "", ytitle = "LER [mm]", xtimevec = subset$date_time, xlabels = F,
                     xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1, format = format_date)
      lines(subset$date_time, subset$pred_ler,col="#807dba", lwd = 2)
      
      text(x=grconvertX(0.3, from = "npc"),y= grconvertY(0.3, from = "npc"),
           labels=mod_labels,pos=4, cex = 0.8)
      text(x=grconvertX(0.5, from = "npc"),y= grconvertY(0.3, from = "npc"),
           labels=estimates,pos=4, cex = 0.8)
      text(x=grconvertX(0.7, from = "npc"),y= grconvertY(0.3, from = "npc"),
           labels=pval,pos=4, cex = 0.8)
      
      plot(subset$date_time, subset$l_cum, col = "grey", type = "l", lwd = 2,
           axes = F, ann = F)
      FunAxisPOSIXct(xtitle = "", ytitle = "length [mm]", xtimevec = subset$date_time, xlabels = F,
                     xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1, format = format_date)
      lines(subset$date_time, subset$pred_l_cum, col = "#807dba", lwd = 2)
      
      plot(subset$date_time, subset$residuals, axes = F, ann = F,
           panel.first = abline(h = 0, col = "#807dba"),
           col = alpha("black", 0.1))
      FunAxisPOSIXct(xtitle = "Time", ytitle = "residuals", xtimevec = subset$date_time,
                     xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1,  format = format_date)
      mtext(text = str_c(as.character(lm_ler$call[1:2]), collapse = "(): "),
            side = 3, outer = T, adj = 0.95, cex = 0.6, col = "#4a1486", line = 0.5)
      # storing output
      model_summaries[[i]] <- SLM
    } else {
      plot.new()
      FunAxisPOSIXct(xtitle = "", ytitle = "LER [mm]", xtimevec = subset$date_time, xlabels = F,
                     xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1, format = format_date)
      plot.new()
      FunAxisPOSIXct(xtitle = "", ytitle = "length [mm]", xtimevec = subset$date_time, xlabels = F,
                     xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1, format = format_date)
      plot.new()
      FunAxisPOSIXct(xtitle = "Time", ytitle = "residuals", xtimevec = subset$date_time,
                     xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1, format = format_date)
      mtext(text = "no data available",
            side = 3, outer = T, adj = 0.95, cex = 0.6, col = "#4a1486", line = 0.5)
    }
  }
  # return model summary
  names(model_summaries) <- levels(df[["experiment"]])
  invisible(model_summaries)
}




# fun plot design ---------------------------------------------------------

fun_plot_design <- function(df = design, date = "2018-04-06", suffix = "") {
  opar1 <- FunPar(tcl = 0, mgp = c(0.5,0.2,0), family = "")
  opar2 <- par(mfrow = c(3,2), mar = c(3,4.5,2.5,0.5), oma = c(1,5,6,1), pty = "s", xpd=NA); on.exit(c(opar1, opar2))
  mycol <- c("#08519c", "#252525")
  a <- 1
  for (i in levels(df[["dframe"]])) {
    subset <- df %>% 
      filter(start_date == date) %>% 
      filter(dframe == i)
    image(x = 1:4, y = 1:4, z = matrix(subset$variety, nrow = 4),
          zlim = c(1,330), col = heat.colors(330), xlab = "dcol", ylab = "drow", axes = F, ann = F)
    mtext(text = str_c("dframe: ", i), side = 3, line = 0.5, adj = 0, col = "gray")
    FunAxis(xtitle = "dcol", ytitle = "drow", xline = 1, yline = 1)
    text(x = subset$dcol, y = subset$drow, labels = subset$variety)
    rect(xleft = as.numeric(as.character(subset$dcol))-0.5,
         xright = as.numeric(as.character(subset$dcol))+0.5,
         ybottom = as.numeric(as.character(subset$drow))-0.5,
         ytop = as.numeric(as.character(subset$drow))-0.25,
         col = mycol[as.numeric(as.character(subset$dreplicate)) %% 2 + 1])
    text(x = as.numeric(as.character(subset$dcol)),
         y = as.numeric(as.character(subset$drow))-0.375,
         labels = subset$dreplicate, col = "white", cex = 0.9)
    box()
    rect(grconvertX(0.02, from='ndc'), grconvertY(0.02, from='nfc'),
         grconvertX(0.98, from='ndc'), grconvertY(1, from='nfc'), border = "gray")
    text(x = grconvertX(0.05, from='ndc'), y = grconvertY(0.5, from='nfc'), labels = str_c("dchamber: ", floor(a)),
         srt = 90, col = "gray", cex = 2)
    a <- a + 0.5
  }
  mtext(text = str_c("start_date: ", date, suffix), side = 3, line = 3, outer = T, cex = 1.5, col = "gray", adj = 0)
  rect(grconvertX(0.90, from = "ndc"), grconvertY(0.92, from = "ndc"),
       grconvertX(0.98, from = "ndc"), grconvertY(0.98, from = "ndc"), col = "orange")
  text(grconvertX(0.94, from = "ndc"), grconvertY(0.95, from = "ndc"), labels = "variety")
  rect(grconvertX(0.90, from = "ndc"), grconvertY(0.92, from = "ndc"),
       grconvertX(0.98, from = "ndc"), grconvertY(0.935, from = "ndc"), col = mycol[2])
  text(grconvertX(0.94, from = "ndc"), grconvertY(0.9275, from = "ndc"), labels = "dreplicate", col = "white", cex = 0.8)
}



# fun flip two var in design ----------------------------------------------

fun_flip <- function(df = design, selec1 = selection1, selec2 = selection2, flip_var = "variety") {
  if(any(selection1 != selection2)) {
    df[[flip_var]][selec1] <- df[[flip_var]][selec1] + df[[flip_var]][selec2]
    df[[flip_var]][selec2] <- df[[flip_var]][selec1] - df[[flip_var]][selec2]
    df[[flip_var]][selec1] <- df[[flip_var]][selec1] - df[[flip_var]][selec2]
  }
  df
}



