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


# varieties = 330
# frames = 6
# devices = 16
# rep = 4


start_date <- c(ymd("2018-04-06") + seq(0,7*13,7))
str_c(year(start_date),month(start_date, label = T, abbr = T),day(start_date))


device <- expand.grid(let = LETTERS[1:6], num = 1:16) %>% 
  arrange(let) %>% 
  mutate(com = str_c(let, num)) %$% com

variety <- 1:330
variety

set.seed(222)
design <- expand.grid(device = device, start_date = start_date) %>% 
  mutate(experiment = str_c(str_c(year(start_date),month(start_date, label = T, abbr = T),day(start_date)), "_", device),
         dposition = as.numeric(str_extract(device, patter = "[:digit:]+")),
         dframe = str_extract(device, patter = "[:alpha:]"),
         drow = cut(dposition, breaks = c(0,4,8,12,16), labels = 1:4),
         dcol = dposition %% 4,
         dcol = ifelse(dcol == 0, 4, dcol),
         dchamber = ceiling(match(dframe, LETTERS[1:26])/2),
         dreplicate = c(rep(1:4, each = 330), rep(NA, 96*14-4*330)),
         variety = c(as.vector(replicate(4,base::sample(variety, length(variety), replace = F))), rep(NA, 96*14-4*330))) %>% 
  select(experiment, start_date, device, dchamber, dframe, drow, dcol, dposition, everything()) %>% 
  as_tibble() %>% 
  FunFactorize(c("device", "dchamber", "dframe", "drow", "dcol", "dposition", "dreplicate"))
design
View(design)





# Uriels technik ----------------------------------------------------------

# gehe von position zu position und sample immer aus einem engeschraenkten raum von moeglichen varieties
# (abhaengig von vorherigen werten)





library(DiGGer)
id.2d <- DiGGer(NumberOfTreatments = 32,
                RowsInDesign = 8,
                ColumnsInDesign = 8,
                RowsInReplicate = 4,
                ColumnsInReplicate = 8,
                RowsInBlock = NULL,
                ColumnsInBlock = NULL,
                BlockIn2D = c(2,2), #*
                Spatial = FALSE,
                RowColumn = FALSE)
d.2d <- run(id.2d)
d.2d$dlist
# write.table(d.2d$dlist, file="2dDesign.txt", sep="\t", row.names = FALSE)
m.2d <- getDesign(d.2d)
getConcurrence(d.2d)

detach("package:vegan", unload=TRUE)
detach("package:DiGGer", unload = T)
#*first number gives number of rows per IB of first blocking dimension
#second number gives number of columns per IB of second blocking dimension




# a single experiment is affected by:
# - starting date ****
# - chamber **
# - frame  *
# - row 
# - col
# - position
# - device ****

# there are no physical complete blocks in our experiment
# "Block what you can, randomize what you cannot."



# old version -------------------------------------------------------------




start_date <- c("2018Feb12",
           "2018Feb16",
           "2018Mar03",
           "2018Mar06")
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







