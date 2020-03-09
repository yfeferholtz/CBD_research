#Sustainable Pathway Scenario

###Assumptions###
  # CO2 growth rate same as 2008-2018
  # no net land use change from 2018
  # GDP growth rate same as 2008-2018

#setup
rm(list=ls())
library(devtools)
P <- rprojroot::find_rstudio_root_file

WaldronModel <- readRDS(P("outputs/WaldronModel5.RDS"))
RishmanModel <- readRDS(P("outputs/RishmanModel2.RDS"))
WiseModel <- readRDS(P("outputs/EmilyModel.RDS"))

#read data
BAUData<- read.csv(P("outputs/BAUdata.csv"))

#Extrapolate for Waldron
ln_waldronexp <-
  WaldronModel$coefficients[[1]]*BAUData$constant+
  WaldronModel$coefficients[[2]]*BAUData$birdspeciesthreatened+
  WaldronModel$coefficients[[3]]*BAUData$mammalspeciesthreatened+
  WaldronModel$coefficients[[4]]*BAUData$ln_landarea+
  WaldronModel$coefficients[[5]]*BAUData$Price_Index_yr2011+
  WaldronModel$coefficients[[6]]*BAUData$terrestrialandmarineprotectedare+
  WaldronModel$coefficients[[7]]*BAUData$futureGDP_sq+
  WaldronModel$coefficients[[8]]*BAUData$Gov
BAUData$ExpWaldron = exp(ln_waldronexp) 
#total sum
WaldronSum <- sum(BAUData$ExpWaldron, na.rm = TRUE)/1E9
#120.6 Billion, no change, ag land not affected

#Rishman Model

ln_rishman <-
  RishmanModel$coefficients[[1]]*BAUData$constant+
  RishmanModel$coefficients[[2]]*BAUData$birdspeciesthreatened+
  RishmanModel$coefficients[[3]]*BAUData$ln_landarea+
  RishmanModel$coefficients[[4]]*BAUData$Gov+
  RishmanModel$coefficients[[5]]*BAUData$average_population_density+
  RishmanModel$coefficients[[6]]*BAUData$aglandpercent+ #only change
  RishmanModel$coefficients[[7]]*BAUData$futureGDP_sq+
  RishmanModel$coefficients[[8]]*BAUData$futureco2levelppp
BAUData$ExpRishman = exp(ln_rishman)
#total sum of expenditures
RishmanSum <- sum(BAUData$ExpRishman, na.rm = TRUE)/1E9
#128.2 Billion, slightly higher than BAU

#Wise model

ln_wise <-
  WiseModel$coefficients[[1]]*BAUData$constant+
  WiseModel$coefficients[[2]]*BAUData$ln_futureGDP+
  WiseModel$coefficients[[3]]*BAUData$futureGDP_sq+
  WiseModel$coefficients[[4]]*BAUData$Gov+
  WiseModel$coefficients[[5]]*BAUData$AvgCO2ReductionPercent+
  WiseModel$coefficients[[6]]*BAUData$aglandpercent+
  WiseModel$coefficients[[7]]*BAUData$birdspeciesthreatened +
  WiseModel$coefficients[[8]]*BAUData$average_population_density
BAUData$ExpWise <- exp(ln_wise)
WiseSum <- sum(BAUData$ExpWise, na.rm = TRUE)/1E9
#426 Billion, slightly less than BAU

