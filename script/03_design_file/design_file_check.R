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


nr_varieties <- 330
# frames = 6
# devices = 16
# rep = 4


start_date <- c(ymd("2018-04-06") + seq(0,7*13,7))
device <- expand.grid(let = LETTERS[1:6], num = 1:16) %>% 
  arrange(let) %>% 
  mutate(com = str_c(let, num)) %$% com
variety <- 1:nr_varieties

set.seed(222)
design <- expand.grid(device = device, start_date = start_date) %>% 
  mutate(experiment = str_c(str_c(year(start_date),month(start_date, label = T, abbr = T),day(start_date)), "_", device),
         dposition = as.numeric(str_extract(device, patter = "[:digit:]+")),
         dframe = str_extract(device, patter = "[:alpha:]"),
         drow = cut(dposition, breaks = c(0,4,8,12,16), labels = 1:4),
         dcol = dposition %% 4,
         dcol = ifelse(dcol == 0, 4, dcol),
         dchamber = ceiling(match(dframe, LETTERS[1:26])/2),
         dreplicate = "",
         variety = "")  %>% 
  select(experiment, start_date, device, dchamber, dframe, drow, dcol, dposition, everything()) %>% 
  as_tibble() %>% 
  FunFactorize(c("device", "dchamber", "dframe", "drow", "dcol", "dposition"))


# randomize varieties within dreplicate
design$variety[design$device != "F16"] <- c(as.vector(replicate(4,base::sample(variety, length(variety), replace = F))),
                                     rep(NA, 95*14-4*nr_varieties))
design$dreplicate[design$device != "F16"] <- c(rep(1:4, each = nr_varieties), rep(NA, 95*14-4*nr_varieties))
design$variety[design$device == "F16"] <- 999
design$dreplicate[design$device == "F16"] <- 0

design$variety <- as.numeric(design$variety)
design$dreplicate <- as.numeric(design$dreplicate)


# flip varieties across start_date ----------------------------------------

# exchange varieties accross start_date
# where two of the same varieties
# are within one start_date


# 1) search for start_date with two times the same variety
var_doublicates <- design %>% 
  group_by(start_date, variety) %>% 
  count() %>% 
  filter(n == 2) %>% 
  inner_join(design) %>% 
  select(start_date, variety, experiment, dreplicate) %>% 
  filter(row_number() %% 2 == 0)


while(nrow(var_doublicates) > 0){
  # 2) randomly select switch partners within the same replicate
  var_doublicates$experiment_switch <- apply(var_doublicates, 1, function(x){
    selection <- design$dreplicate == x[["dreplicate"]] &
      design$start_date != x[["start_date"]] &
      !is.na(design$variety)
    sample(design$experiment[selection], 1)
  })
  var_doublicates <- var_doublicates %>% 
    inner_join(design, by = c("experiment_switch" = "experiment"), suffix = c("", "_switch")) %>% 
    select(start_date, variety, experiment, dreplicate, experiment_switch, variety_switch)
  print(var_doublicates)
  
  #  3) flip the varieties of the switch partners
  for (i in seq_along(var_doublicates$experiment)) {
    selection1 <- design$experiment == var_doublicates$experiment[i]
    selection2 <- design$experiment == var_doublicates$experiment_switch[i]
    design <- fun_flip(df = design, selec1 = selection1, selec2 = selection2, flip_var = "variety")
  }
  
  # 4) search again for start_date with two times the same variety
  var_doublicates <- design %>% 
    group_by(start_date, variety) %>% 
    count() %>% 
    filter(n == 2) %>% 
    inner_join(design) %>% 
    select(start_date, variety, experiment, dreplicate) %>% 
    filter(row_number() %% 2 == 0)
}



# flip varieties within start_date ----------------------------------------

# exchange varieties within start_date
# where one varieties is measured by the
# same device several times


# 1) search for start_date with two times the same variety
dev_doublicates <- design %>% 
  group_by(variety, device) %>% 
  count() %>% 
  filter(n > 1) %>% 
  inner_join(design) %>% 
  select(experiment, start_date, device, variety) %>% 
  filter(row_number() %% 2 == 0)


while(nrow(dev_doublicates) > 0) {
  # 2) randomly select switch partners within the same start_date
  dev_doublicates$experiment_switch <- apply(dev_doublicates, 1, function(x){
    selection <- design$start_date == x[["start_date"]] &
      !is.na(design$variety)
    sample(design$experiment[selection], 1)
  })
  dev_doublicates <- dev_doublicates %>% 
    inner_join(design, by = c("experiment_switch" = "experiment"), suffix = c("", "_switch")) %>% 
    select(experiment, start_date, device, variety, experiment_switch, device_switch, variety_switch)
  print(dev_doublicates)
  
  #  3) flip the varieties (and dreplicate) of the switch partners
  for (i in seq_along(dev_doublicates$experiment)) {
    selection1 <- design$experiment == dev_doublicates$experiment[i]
    selection2 <- design$experiment == dev_doublicates$experiment_switch[i]
    design <- fun_flip(df = design, selec1 = selection1, selec2 = selection2, flip_var = "variety")
    design <- fun_flip(df = design, selec1 = selection1, selec2 = selection2, flip_var = "dreplicate")
  }
  
  # 4) search again for start_date with two times the same variety
  dev_doublicates <- design %>% 
    group_by(variety, device) %>% 
    count() %>% 
    filter(n > 1) %>% 
    inner_join(design) %>% 
    select(experiment, start_date, device, variety) %>% 
    filter(row_number() %% 2 == 0)
}


# pdf("fig/design_with_check.pdf", width = 6, height = 9, bg = "white")
for(i in seq_along(unique(design$start_date))){
  date_label <- unique(design$start_date)[i]
  fun_plot_design(date = date_label)
}
# dev.off()







# a single experiment is affected by:
# - starting date ****
# - chamber **
# - frame  *
# - row 
# - col
# - position
# - device ****

