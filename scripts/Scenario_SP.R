#Sustainable Pathway Scenario

###Assumptions###
  # CO2 levels stay the same as 2014 - growth rate 0
  # no net land use change from 2018
  # GDP growth rate same as 2008-2018

#setup
rm(list=ls())
library(devtools)
P <- rprojroot::find_rstudio_root_file

#WaldronModel <- readRDS(P("outputs/WaldronModel5.RDS"))
RishmanModel <- readRDS(P("outputs/RishmanModel2.RDS"))
Mlr1Model <- readRDS(P("outputs/mlr1.RDS"))

library(dplyr)
#read data
SPdata<- read.csv(P("outputs/BAUdata.csv"), stringsAsFactors = FALSE) %>% 
  mutate(SPCO2growth = 0) %>% 
  mutate(SPAgGrowth = 0)


# #Extrapolate for Waldron
# ln_waldronexp <-
#   WaldronModel$coefficients[[1]]*SPdata$constant+
#   WaldronModel$coefficients[[2]]*SPdata$birdspeciesthreatened+
#   WaldronModel$coefficients[[3]]*SPdata$mammalspeciesthreatened+
#   WaldronModel$coefficients[[4]]*SPdata$ln_landarea+
#   WaldronModel$coefficients[[5]]*SPdata$Price_Index_yr2011+
#   WaldronModel$coefficients[[6]]*SPdata$terrestrialandmarineprotectedare+
#   WaldronModel$coefficients[[7]]*SPdata$futureGDP_sq+
#   WaldronModel$coefficients[[8]]*SPdata$Gov
# SPdata$ExpWaldronSP = exp(ln_waldronexp) 
# #total sum
# WaldronSum <- sum(SPdata$ExpWaldronSP, na.rm = TRUE)/1E9
# #120.6 Billion, no change, ag land not affected

#Rishman Model

ln_rishman <-
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

#Wise model

ln_mlr1 <-
  Mlr1$coefficients[[1]]*SPdata$constant+
  Mlr1$coefficients[[2]]*SPdata$ln_futureGDP+
  Mlr1$coefficients[[3]]*SPdata$futureGDP_sq+
  Mlr1$coefficients[[4]]*SPdata$Gov+
  Mlr1$coefficients[[5]]*SPdata$SPCO2growth+
  Mlr1$coefficients[[6]]*SPdata$aglandpercent+
  Mlr1$coefficients[[7]]*SPdata$birdspeciesthreatened+
  Mlr1$coefficients[[8]]*SPdata$average_population_density+
  Mlr1$coefficients[[9]]*SPdata$SPAgGrowth+
  Mlr1$coefficients[[10]]*SPdata$terrestrialprotectedareasoftotal+
  Mlr1$coefficients[[11]]*SPdata$ln_landarea
SPdata$Expmlr1SP <- exp(ln_mlr1)
mlr1Sum <- sum(SPdata$Expmlr1SP, na.rm = TRUE)/1E9
#543.8 billion, slightly less than BAU


SPdata <- SPdata %>%
  arrange(countries)

#Need to apply needs
Mlr1Needs <- readRDS("outputs/mlr1Needs.RDS")
RishmanNeeds <- readRDS("outputs/RishmanNeeds.RDS")
#WaldronNeeds<-readRDS("outputs/AnthonyNeeds.RDS")


# #Waldron Needs
# ln_needs_waldron <-
#   WaldronNeeds$coefficients[[1]]*SPdata$constant+
#   WaldronNeeds$coefficients[[2]]*ln_waldronexp
# SPdata$WaldronNeedsSP <- exp(ln_needs_waldron)
# WaldronNeedsSum <- sum(SPdata$WaldronNeedsSP, na.rm = TRUE)/1E9 #156.39 no change

#Rishman Needs
SPdata$terrestrialandmarineprotectedare <- FullData$terrestrialandmarineprotectedare

ln_needs_rishman <- 
  RishmanNeeds$coefficients[[1]]*SPdata$constant+
  RishmanNeeds$coefficients[[2]]*ln_rishman+
  RishmanNeeds$coefficients[[3]]*SPdata$mammalspeciesthreatened+
  RishmanNeeds$coefficients[[4]]*SPdata$oilrentsofgdp+
  RishmanNeeds$coefficients[[5]]*SPdata$terrestrialandmarineprotectedare
SPdata$RishmanNeedsSP <- exp(ln_needs_rishman)
SumRishmanNeeds <- sum(SPdata$RishmanNeedsSP, na.rm = TRUE)/1E9 #184.8

# Wise Needs

ln_needs_mlr1 <-
  Mlr1Needs$coefficients[[1]]*SPdata$constant+
  Mlr1Needs$coefficients[[2]]*ln_mlr1
SPdata$Mlr1NeedsSP <- exp(ln_needs_mlr1)
SumMlr1Needs <- sum(SPdata$Mlr1NeedsSP, na.rm = TRUE)/1E9 #690.88







write.csv(SPdata, "data/SPdata.csv")


