#Global Conservation Scenario

###Assumptions###
  # 30% reduction in ag land
  # 30% reduction in CO2 emissions
  # No GDP change from 2018 levels

#setup
rm(list=ls())
library(devtools)
P <- rprojroot::find_rstudio_root_file

WaldronModel <- readRDS(P("outputs/WaldronModel5.RDS"))
RishmanModel <- readRDS(P("outputs/RishmanModel2.RDS"))
WiseModel <- readRDS(P("outputs/EmilyModel.RDS"))

#read data 
BAUData<- read.csv(P("outputs/BAUdata.csv"))

#Create GCData from BAU
library(dplyr)
GCData <- BAUData %>%
  mutate(futureCO2_EMS=co2ppp*.7)%>%
  mutate(futureagland=aglandpercent*.7) %>%
  mutate(futureGDP_sq=log(GDP)^2)%>%
  mutate(ln_futureGDP=log(GDP))

#Extrapolate for Waldron
ln_waldronexp <-
  WaldronModel$coefficients[[1]]*GCData$constant+
  WaldronModel$coefficients[[2]]*GCData$birdspeciesthreatened+
  WaldronModel$coefficients[[3]]*GCData$mammalspeciesthreatened+
  WaldronModel$coefficients[[4]]*GCData$ln_landarea+
  WaldronModel$coefficients[[5]]*GCData$Price_Index_yr2011+
  WaldronModel$coefficients[[6]]*GCData$terrestrialandmarineprotectedare+
  WaldronModel$coefficients[[7]]*GCData$futureGDP_sq+
  WaldronModel$coefficients[[8]]*GCData$Gov
GCData$ExpWaldron = exp(ln_waldronexp) 

#total sum
WaldronSum <- sum(GCData$ExpWaldron, na.rm = TRUE)/1E9
#107.899 Billion, quite a bit less than BAU

#Rishman Model

ln_rishman <-
  RishmanModel$coefficients[[1]]*GCData$constant+
  RishmanModel$coefficients[[2]]*GCData$birdspeciesthreatened+
  RishmanModel$coefficients[[3]]*GCData$ln_landarea+
  RishmanModel$coefficients[[4]]*GCData$Gov+
  RishmanModel$coefficients[[5]]*GCData$average_population_density+
  RishmanModel$coefficients[[6]]*GCData$futureagland+ 
  RishmanModel$coefficients[[7]]*GCData$futureGDP_sq+
  RishmanModel$coefficients[[8]]*GCData$futureCO2_EMS
GCData$ExpRishman = exp(ln_rishman)  
#total sum of expenditures
RishmanSum <- sum(GCData$ExpRishman, na.rm = TRUE)/1E9
#97 Billion, quite a bit lower than BAU

#Wise model

ln_wise <-
  WiseModel$coefficients[[1]]*GCData$constant+
  WiseModel$coefficients[[2]]*GCData$ln_futureGDP+
  WiseModel$coefficients[[3]]*GCData$futureGDP_sq+
  WiseModel$coefficients[[4]]*GCData$Gov+
  WiseModel$coefficients[[5]]*GCData$AvgCO2ReductionPercent+
  WiseModel$coefficients[[6]]*GCData$aglandpercent+
  WiseModel$coefficients[[7]]*GCData$birdspeciesthreatened +
  WiseModel$coefficients[[8]]*GCData$average_population_density
GCData$ExpWise <- exp(ln_wise)
WiseSum <- sum(GCData$ExpWise, na.rm = TRUE)/1E9
#256 Billion, quite a bit less than BAU


GCData <- GCData %>%
  arrange(countries)


