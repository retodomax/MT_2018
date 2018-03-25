###########################################################################
## Project:  MT_2018
## Content:  arrange data
## Date:     2018-03-15 10:47:30
## Author:   Reto Zihlmann <retozihlmann@outlook.com>
###########################################################################


# packages ----------------------------------------------------------------

library(tidyverse)
library(stringr)
library(magrittr)
library(lubridate)
library(hms)
library(forcats)
# devtools::install_git("https://github.com/retodomax/FunRZ")
library(FunRZ)
Sys.setlocale("LC_ALL", "English")
source("script/00_functions/fun_MT.R")


# parameters --------------------------------------------------------------

values_to_cut <- 20
my_path <- "data/01_lenviro/A"
print_all <- F
print_all_suffix <- "_all"

print_ag <- F
print_ag_suffix <- "_all"



# import data -------------------------------------------------------------

my_dir <- list.files(path = my_path, recursive = T, pattern = "\\.txt$", full.names = T)
lenviro <- vector("list", length(my_dir))
for (i in seq_along(my_dir)) {
  # collect file info and write experiment_label
  file_info <- unlist(str_split(my_dir[i], pattern = "(/|-|_|\\.)"))
  experiment_label <- str_c(file_info[7],
                            file_info[5],
                            file_info[6], "_",
                            file_info[length(file_info)-1])
  # read in as single string
  file <- read_file(my_dir[i]) %>%
    # match everything but digits, -, :, ., space, newline and replace with tab
    str_replace_all("[^[:digit:]-:. \\n\\r]+", "\t")
  # check if it is empty, otherways read_tsv
    if (file != ""){
      lenviro[[i]] <- read_tsv(file, col_names = c("date_time", "device", "angle", "raw_angle",
                                   "temp", "hum", "lux")) %>% 
        mutate(experiment = experiment_label)
    }
}
lenviro <- reduce(lenviro, rbind)
lenviro$experiment <- as_factor(as.character(lenviro$experiment))
# lenviro$device <- as_factor(as.character(lenviro$device))
# save(lenviro, file = "data/01_lenviro/lenviro.RData")
# load("data/01_lenviro/lenviro.RData")



# LER ---------------------------------------------------------------------


# find inverse wheel cases
inverse <- lenviro %>% 
  group_by(experiment) %>% 
  arrange(date_time) %>% 
  mutate(angle_diff = angle - lag(angle)) %>% 
  filter(angle_diff != 0) %>%
  summarise(mean_angle_diff = mean(angle_diff, trim = 0.05, na.rm = T),
            inverse_wheel = mean_angle_diff < 0 ) %>% 
  filter(inverse_wheel)

# define new variables angle_diff, time_diff, l_diff, ler and id
# discard the first "values_to_cut" values of each robot (influenced by the experimenter)
lenviro <- lenviro %>% 
  arrange(date_time) %>% 
  mutate(id = seq_along(date_time)) %>% 
  group_by(experiment) %>% 
  mutate(daytime = as.numeric(paste0(hour(date_time), str_pad(minute(date_time),2,pad = "0"))),
         daytime = between(daytime, 530, 2300),
         angle_diff = angle - lag(angle),
         angle_diff = ifelse(experiment %in% inverse$experiment, -angle_diff, angle_diff),
         angle_diff = ifelse(angle_diff < -270, 360+angle_diff, angle_diff),
         time_diff = as.numeric(difftime(date_time, lag(date_time), units = c("secs"))),
         l_diff = angle_diff*(8*pi/360),
         ler = l_diff/time_diff*60*60) %>% 
  slice(values_to_cut:length(date_time)+1) %>%
  arrange(date_time) %>% 
  select(id, date_time, experiment, everything())


# VPD ---------------------------------------------------------------------

#https://physics.stackexchange.com/questions/4343/how-can-i-calculate-vapor-
# pressure-deficit-from-temperature-and-relative-humidit


lenviro <- lenviro %>% 
  mutate(es = 0.6108 * exp(17.27 * temp / (temp + 237.3)),
         vpd = (1- (hum / 100)) * es) # in kPa




# exclude outliers --------------------------------------------------------

lenviro <- lenviro %>% 
  group_by(experiment) %>% 
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
  group_by(experiment) %>% 
  arrange(date_time) %>% 
  mutate(l_cum = cumsum(ifelse(is.na(l_diff), 0, l_diff)))


# plot var and model ------------------------------------------------------

if (print_all){pdf(str_c("fig/var_mod", print_all_suffix, ".pdf"),
                   width = 8, height = 7, bg = "white")}
mod_summary <- fun_plot_overview(df = lenviro)
if (print_all){dev.off()}



# aggregate to 20 min -----------------------------------------------------

lenviro <- lenviro %>%
  arrange(date_time) %>%
  group_by(experiment) %>%
  mutate(time_period = c(rep(seq_len(n() %/% 5), each=5), rep(NA,n() %% 5)))


ag_lenviro <- lenviro %>%
  group_by(experiment, time_period) %>%
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




# plot aggregated var and model -------------------------------------------

if (print_ag){pdf(str_c("fig/ag_var_mod", print_ag_suffix, ".pdf"),
                  width = 8, height = 7, bg = "white")}
mod_summary <- fun_plot_overview(ag_lenviro)
if (print_ag){dev.off()}
