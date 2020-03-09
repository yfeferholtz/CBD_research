#Global Conservation Scenario

###Assumptions###
  # restore habitat -> 30% reduction in ag land
  # ambitious carbon mitigation -> 30% reduction in CO2 emissions
  # NO change from 2018 levels

#setup
rm(list=ls())
library(devtools)
P <- rprojroot::find_rstudio_root_file

#read data 
ScenData <- read.csv(P("data/scenario_data.csv"))






