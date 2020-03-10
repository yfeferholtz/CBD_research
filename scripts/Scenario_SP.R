#Sustainable Pathway Scenario

###Assumptions###
<<<<<<< HEAD
  # CO2 levels remain at 2014 levels
=======
  # CO2 levels stay the same as 2014 - growth rate 0
>>>>>>> 60b09c8291af1a68c31962f06f1d48576a2846ab
  # no net land use change from 2018
  # GDP growth rate same as 2008-2018

#setup
rm(list=ls())
library(devtools)
P <- rprojroot::find_rstudio_root_file

WaldronModel <- readRDS(P("outputs/WaldronModel5.RDS"))
RishmanModel <- readRDS(P("outputs/RishmanModel2.RDS"))
WiseModel <- readRDS(P("outputs/EmilyModel.RDS"))

library(dplyr)
#read data
SPdata<- read.csv(P("outputs/BAUdata.csv"), stringsAsFactors = FALSE) %>% 
  mutate(SPCO2growth = 0)


SPData <- BAUData %>%
  mutate(futureco2levelppp=co2ppp)%>%
  mutate(futureco2level=co2emissions)

#Extrapolate for Waldron
ln_waldronexp <-
<<<<<<< HEAD
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
=======
  WaldronModel$coefficients[[1]]*SPdata$constant+
  WaldronModel$coefficients[[2]]*SPdata$birdspeciesthreatened+
  WaldronModel$coefficients[[3]]*SPdata$mammalspeciesthreatened+
  WaldronModel$coefficients[[4]]*SPdata$ln_landarea+
  WaldronModel$coefficients[[5]]*SPdata$Price_Index_yr2011+
  WaldronModel$coefficients[[6]]*SPdata$terrestrialandmarineprotectedare+
  WaldronModel$coefficients[[7]]*SPdata$futureGDP_sq+
  WaldronModel$coefficients[[8]]*SPdata$Gov
SPdata$ExpWaldronSP = exp(ln_waldronexp) 
#total sum
WaldronSum <- sum(SPdata$ExpWaldronSP, na.rm = TRUE)/1E9
>>>>>>> 60b09c8291af1a68c31962f06f1d48576a2846ab
#120.6 Billion, no change, ag land not affected

#Rishman Model

ln_rishman <-
<<<<<<< HEAD
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
=======
  RishmanModel$coefficients[[1]]*SPdata$constant+
  RishmanModel$coefficients[[2]]*SPdata$birdspeciesthreatened+
  RishmanModel$coefficients[[3]]*SPdata$ln_landarea+
  RishmanModel$coefficients[[4]]*SPdata$Gov+
  RishmanModel$coefficients[[5]]*SPdata$average_population_density+
  RishmanModel$coefficients[[6]]*SPdata$aglandpercent+ #only change
  RishmanModel$coefficients[[7]]*SPdata$futureGDP_sq+
  RishmanModel$coefficients[[8]]*SPdata$co2ppp
SPdata$ExpRishmanSP = exp(ln_rishman)
#total sum of expenditures
RishmanSum <- sum(SPdata$ExpRishmanSP, na.rm = TRUE)/1E9
#127.8 Billion, slightly higher than BAU
>>>>>>> 60b09c8291af1a68c31962f06f1d48576a2846ab

#Wise model

ln_wise <-
<<<<<<< HEAD
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
=======
  WiseModel$coefficients[[1]]*SPdata$constant+
  WiseModel$coefficients[[2]]*SPdata$ln_futureGDP+
  WiseModel$coefficients[[3]]*SPdata$futureGDP_sq+
  WiseModel$coefficients[[4]]*SPdata$Gov+
  WiseModel$coefficients[[5]]*SPdata$SPCO2growth+
  WiseModel$coefficients[[6]]*SPdata$aglandpercent+
  WiseModel$coefficients[[7]]*SPdata$birdspeciesthreatened +
  WiseModel$coefficients[[8]]*SPdata$average_population_density
SPdata$ExpWiseSP <- exp(ln_wise)
WiseSum <- sum(SPdata$ExpWiseSP, na.rm = TRUE)/1E9
#359 Billion, slightly less than BAU

SPdata <- SPdata %>%
>>>>>>> 60b09c8291af1a68c31962f06f1d48576a2846ab
  arrange(countries)

#Need to apply needs
WiseNeeds <- readRDS("outputs/EmilyNeeds.RDS")
RishmanNeeds <- readRDS("outputs/RishmanNeeds.RDS")
WaldronNeeds<-readRDS("outputs/AnthonyNeeds.RDS")


#Waldron Needs
ln_needs_waldron <-
  WaldronNeeds$coefficients[[1]]*SPdata$constant+
  WaldronNeeds$coefficients[[2]]*ln_waldronexp
SPdata$WaldronNeedsSP <- exp(ln_needs_waldron)
WaldronNeedsSum <- sum(SPdata$WaldronNeedsSP, na.rm = TRUE)/1E9 #156.39 no change

#Rishman Needs

ln_needs_rishman <- 
  RishmanNeeds$coefficients[[1]]*SPdata$constant+
  RishmanNeeds$coefficients[[2]]*ln_rishman+
  RishmanNeeds$coefficients[[3]]*SPdata$mammalspeciesthreatened+
  RishmanNeeds$coefficients[[4]]*SPdata$oilrentsofgdp+
  RishmanNeeds$coefficients[[5]]*SPdata$terrestrialandmarineprotectedare
SPdata$RishmanNeeds <- exp(ln_needs_rishman)
SumRishmanNeeds <- sum(SPdata$RishmanNeeds, na.rm = TRUE)/1E9 #184.8

# Wise Needs

ln_needs_wise <-
  WiseNeeds$coefficients[[1]]*SPdata$constant+
  WiseNeeds$coefficients[[2]]*ln_wise
SPdata$WiseNeeds <- exp(ln_needs_wise)
SumWiseNeeds <- sum(SPdata$WiseNeeds, na.rm = TRUE)/1E9 #463.1


write.csv(SPdata, "data/SPdata.csv")


