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


# import data -------------------------------------------------------------


# problem with missing tabs and empty files => complicate method for import

my_path <- dir("data/01_lenviro/Mar-03-2017")
lenviro <- vector("list", length(my_path))
for (i in seq_along(my_path)) {
  # read in as single string
  file <- read_file(str_c("data/01_lenviro/Mar-03-2017/", my_path[i])) %>%
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
# discard the first 20 values of each robot
lenviro <- lenviro %>% 
  group_by(device) %>% 
  arrange(date_time) %>% 
  mutate(angle_diff = angle - lag(angle),
         angle_diff = ifelse(device %in% inverse$device, -angle_diff, angle_diff),
         angle_diff = ifelse(angle_diff < -270, 360+angle_diff, angle_diff),
         time_diff = as.numeric(difftime(date_time, lag(date_time), units = c("secs"))),
         l_diff = angle_diff*(8*pi/360),
         ler = l_diff/time_diff) %>% 
  ungroup() %>% 
  mutate(id = seq_along(date_time)) %>% 
  group_by(device) %>% 
  slice(20:length(date_time)) %>%
  mutate(l_cum = cumsum(l_diff)) %>% 
  arrange(id) %>% 
  select(id, everything())




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


# what could have happend here?
lenviro %>% 
  filter(device == 12) %>% 
  filter(between(id, 9746, 9836)) %>% 
  View()




# plot all variables ------------------------------------------------------

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

## base graphics

fun_plot_var <- function(df, format = "%d.%m") {
  opar1 <- FunPar(yaxs = "r", xaxs = "r", family = "", mar = c(0,5,0,2), tcl = 0.2)
  opar2 <- par(oma = c(3.5,0,3,0), mfrow = c(6,1)); on.exit(par(c(opar1, opar2)))
  for (i in levels(df[["device"]])) {
    lenviro_sel <- df %>%
      filter(device == i)
    
    lenviro_sel$temp
    var <- c("temp", "hum", "lux", "angle", "l_cum", "ler")
    var_name <- c("temp [°C]", "hum [%]", "lux [?]", "angle [°]", "length growth [mm]", "LER [mm/s]")
    for (j in seq_along(var)) {
      plot(lenviro_sel$date_time, lenviro_sel[[var[j]]], type = "l",
           col = "darkgray", axes = F, ann = F)
      FunAxisPOSIXct(xtimevec = lenviro_sel$date_time, format = format,
                     ytitle = var_name[j], xtitle = "", yline = 3.5, xlabels = F,
                     xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1)
    }
    FunAxisPOSIXct(xtimevec = lenviro_sel$date_time, format = "%d.%m",
                   ytitle = var_name[i], xtitle = "Date", yline = 3.5, xlabels = T,
                   xtick_lwd = 1, ytick_lwd = 1, box_lwd = 1)
    mtext(str_c("device:", i), side = 3, outer = T, adj = 0.05, cex = 1.5, col = "darkgray", line = 0.5)
  }  
}


# pdf("fig/ler_setup_var4.pdf", width = 4, height = 7, bg = "white")
fun_plot_var(df = lenviro)
# dev.off()








# discard outliers --------------------------------------------------------





# compute LERth -----------------------------------------------------------


