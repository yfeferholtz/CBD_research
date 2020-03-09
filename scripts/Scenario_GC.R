#Global Conservation Scenario

###Assumptions###
  # 30% reduction in ag land
  # 30% reduction in CO2 emissions
  # No GDP change from 2018 levels

#setup
rm(list=ls())
library(devtools)
P <- rprojroot::find_rstudio_root_file

#read data 
BAUData<- read.csv(P("outputs/BAUdata.csv"))






