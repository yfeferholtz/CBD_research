#Sustainable Pathway Scenario

###Assumptions###
  # CO2 growth rate same as 2008-2018
  # no net land use change from 2018
  # GDP growth rate same as 2008-2018

#setup
rm(list=ls())
library(devtools)
P <- rprojroot::find_rstudio_root_file

#read data
ScenData <- read.csv(P("data/scenario_data.csv"))

#Find 2030 GDP (1% annual growth)
library(dplyr)
ScenData <- ScenData %>%
  mutate("2030 GDP" = GDP*(1.01^(12)))



