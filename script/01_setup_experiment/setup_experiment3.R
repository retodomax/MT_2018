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

my_path <- dir("data/01_lenviro/Mar-03-2017", pattern = "\\.txt$")
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



# leaf elongation rate per termal time ------------------------------------

# 0) daytime qualifier
lenviro %>% 
  filter(device == 7) %>% 
  View()

# 1) find T0
# plot all nighttime data with 
  

# 2) make formula




# model -------------------------------------------------------------------






# aggregate to 20 min -----------------------------------------------------


lenviro <- lenviro %>% 
  arrange(date_time) %>% 
  group_by(device) %>% 
  mutate(time_period = c(rep(seq_len(n() %/% 5), each=5), rep(NA,n() %% 5)))


ag_lenviro <- lenviro %>% 
  group_by(device, time_period) %>% 
  summarise(date_time = max(date_time, na.rm = T),
            duration = sum(time_diff, na.rm = T),
            lux = mean(lux, na.rm = T),
            temp = mean(temp, na.rm = T),
            vpd = mean(vpd, na.rm = T),
            hum = mean(hum, na.rm = T),
            angle = max(angle, na.rm = T),
            l_diff = sum(l_diff, na.rm = T),
            l_cum = max(l_cum, na.rm = T),
            ler = l_diff/duration)
  




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








