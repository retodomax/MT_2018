###########################################################################
## Project:  MT_2018
## Content:  create design file
## Date:     2018-03-15 13:52:02
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




# design file -------------------------------------------------------------


# thursday: last watering, take away desired 96 pods from automated watered table (nursery)
# friday: install 96 pods on phenotyping platform, start measuring
# no watering for 6 days
# thursday: rewatering, take away next 96 pods from nursery
# friday: stop measuring, disconect devices, conect next 96 pods


start_date <- c(ymd("2018-04-06") + seq(0,7*13,7))
str_c(year(start_date),month(start_date, label = T, abbr = T),day(start_date))


device <- expand.grid(let = LETTERS[1:6], num = 1:16) %>% 
  arrange(let) %>% 
  mutate(com = str_c(let, num)) %$% com

design <- expand.grid(device = device, start_date = start_date) %>% 
  mutate(experiment = str_c(str_c(year(start_date),month(start_date, label = T, abbr = T),day(start_date)), "_", device),
         dposition = as.numeric(str_extract(device, patter = "[:digit:]+")),
         dframe = str_extract(device, patter = "[:alpha:]"),
         drow = cut(dposition, breaks = c(0,4,8,12,16), labels = 1:4),
         dcol = dposition %% 4,
         dcol = ifelse(dcol == 0, 4, dcol),
         dchamber = ceiling(match(dframe, LETTERS[1:26])/2),
         variety = base::sample(variety, length(experiment), replace = T)) %>% 
  select(experiment, start_date, device, dchamber, dframe, drow, dcol, dposition, everything())
design






start_date <- c("2018Feb12",
           "2018Feb16",
           "2018Mar03",
           "2018Mar06",
           "2018Mar09")
device <- str_c("A", 1:16)
variety <- c("CH Claro", "Skagen", "Titlis")

design <- expand.grid(device = device, start_date = start_date) %>% 
  mutate(experiment = str_c(start_date, "_", device),
         position = as.numeric(str_extract(device, patter = "[:digit:]+")),
         block = str_extract(device, patter = "[:alpha:]"),
         iblock1 = cut(position, breaks = c(0,4,8,12,16), labels = 1:4),
         iblock2 = position %% 4,
         iblock2 = ifelse(iblock2 == 0, 4, iblock2),
         variety = base::sample(variety, length(experiment), replace = T)) %>% 
  select(experiment, start_date, everything())
# Better: make two columns for variety: variety_nr and variety_name
# assign a certain variety_nr to each experiment and join with a variety file (variety_nr, variety_name)


design
# Questions:
# how many devices and varieties are there finally? --> 96


# how to assign the varieties?
# you can use sample() or if you want you can use something more sofisticated
# have a look to ExDesing chap 1







