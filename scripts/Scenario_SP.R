#Sustainable Pathway Scenario

###Assumptions###
  # CO2 levels remain at 2014 levels
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

SPData <- BAUData %>%
  mutate(futureco2levelppp=co2ppp)%>%
  mutate(futureco2level=co2emissions)

#Extrapolate for Waldron
ln_waldronexp <-
  WaldronModel$coefficients[[1]]*SPData$constant+
  WaldronModel$coefficients[[2]]*SPData$birdspeciesthreatened+
  WaldronModel$coefficients[[3]]*SPData$mammalspeciesthreatened+
  WaldronModel$coefficients[[4]]*SPData$ln_landarea+
  WaldronModel$coefficients[[5]]*SPData$Price_Index_yr2011+
  WaldronModel$coefficients[[6]]*SPData$terrestrialandmarineprotectedare+
  WaldronModel$coefficients[[7]]*SPData$futureGDP_sq+
  WaldronModel$coefficients[[8]]*SPData$Gov
SPData$ExpWaldron = exp(ln_waldronexp) 
#total sum
WaldronSum <- sum(SPData$ExpWaldron, na.rm = TRUE)/1E9
#120.6 Billion, no change, ag land not affected

#Rishman Model

ln_rishman <-
  RishmanModel$coefficients[[1]]*SPData$constant+
  RishmanModel$coefficients[[2]]*SPData$birdspeciesthreatened+
  RishmanModel$coefficients[[3]]*SPData$ln_landarea+
  RishmanModel$coefficients[[4]]*SPData$Gov+
  RishmanModel$coefficients[[5]]*SPData$average_population_density+
  RishmanModel$coefficients[[6]]*SPData$aglandpercent+ #change
  RishmanModel$coefficients[[7]]*SPData$futureGDP_sq+
  RishmanModel$coefficients[[8]]*SPData$futureco2levelppp #change
SPData$ExpRishman = exp(ln_rishman)
#total sum of expenditures
RishmanSum <- sum(SPData$ExpRishman, na.rm = TRUE)/1E9
#127.8 Billion

#Wise model

ln_wise <-
  WiseModel$coefficients[[1]]*SPData$constant+
  WiseModel$coefficients[[2]]*SPData$ln_futureGDP+
  WiseModel$coefficients[[3]]*SPData$futureGDP_sq+
  WiseModel$coefficients[[4]]*SPData$Gov+
  WiseModel$coefficients[[5]]*SPData$AvgCO2ReductionPercent+
  WiseModel$coefficients[[6]]*SPData$aglandpercent+ #only change
  WiseModel$coefficients[[7]]*SPData$birdspeciesthreatened +
  WiseModel$coefficients[[8]]*SPData$average_population_density
SPData$ExpWise <- exp(ln_wise)
WiseSum <- sum(SPData$ExpWise, na.rm = TRUE)/1E9
#426 Billion

SPData <- SPData %>%
  arrange(countries)
