#Sustainable Pathway Scenario

###Assumptions###
  # effective climate change mitigation -> 0% CO2 change 
  # land use change effectively managed -> 0% land use change
  # slow GDP growth -> growth rate to 1%

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



