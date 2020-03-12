#Global Conservation Scenario

###Assumptions###
  # 30% reduction in ag land
  # 30% reduction in CO2 emissions
  # No GDP change from 2018 levels

#setup
rm(list=ls())
library(devtools)
P <- rprojroot::find_rstudio_root_file

#WaldronModel <- readRDS(P("outputs/WaldronModel5.RDS"))
RishmanModel <- readRDS(P("outputs/RishmanModel2.RDS"))
Mlr1 <- readRDS(P("outputs/mlr1.RDS"))

#read data 
BAUData<- read.csv(P("data/SPdata.csv"))
GDPdata <- read.csv(P('data/scenario_data.csv')) %>% 
  arrange(countries)

#Create GCData from BAU
library(dplyr)
GCData <- BAUData %>%
  mutate(futureCO2_EMS=co2ppp*.7)%>%
  mutate(futureagland=aglandpercent*.7) %>%
  mutate(GDP = GDPdata$GDP) %>% 
  mutate(GSGDPgrowth=GDPdata$AverageGDPrate/2)%>%
  mutate(GSGDPgrowth = GSGDPgrowth/100) %>% 
  mutate(GDPmultiplier = 1+GSGDPgrowth) %>% 
  mutate(GSgdp = GDP*(GDPmultiplier^(12)))
GCData$GSgdp[203] = GCData$GDP[203]

GCData<-GCData%>% 
  mutate(ln_GSGDP=log(GSgdp)) %>% 
  mutate(GSgdp_sq = ln_GSGDP^2) %>% 
  mutate(GCco2reduction = .3) %>% 
  mutate(GCAgGrowth = -.3)

# #Extrapolate for Waldron
# ln_waldronexp <-
#   WaldronModel$coefficients[[1]]*GCData$constant+
#   WaldronModel$coefficients[[2]]*GCData$birdspeciesthreatened+
#   WaldronModel$coefficients[[3]]*GCData$mammalspeciesthreatened+
#   WaldronModel$coefficients[[4]]*GCData$ln_landarea+
#   WaldronModel$coefficients[[5]]*GCData$Price_Index_yr2011+
#   WaldronModel$coefficients[[6]]*GCData$terrestrialandmarineprotectedare+
#   WaldronModel$coefficients[[7]]*GCData$futureGDP_sq+
#   WaldronModel$coefficients[[8]]*GCData$Gov
# GCData$ExpWaldronGC = exp(ln_waldronexp) 

#total sum
#WaldronSum <- sum(GCData$ExpWaldron, na.rm = TRUE)/1E9
#107.899 Billion, quite a bit less than BAU

#Rishman Model

ln_rishman <-
  RishmanModel$coefficients[[1]]*GCData$constant+
  RishmanModel$coefficients[[2]]*GCData$birdspeciesthreatened+
  RishmanModel$coefficients[[3]]*GCData$ln_landarea+
  RishmanModel$coefficients[[4]]*GCData$Gov+
  RishmanModel$coefficients[[5]]*GCData$average_population_density+
  RishmanModel$coefficients[[6]]*GCData$futureagland+ 
  RishmanModel$coefficients[[7]]*GCData$GSgdp_sq+
  RishmanModel$coefficients[[8]]*GCData$futureCO2_EMS
GCData$ExpRishmanGC = exp(ln_rishman)  
#total sum of expenditures
RishmanSum <- sum(GCData$ExpRishman, na.rm = TRUE)/1E9
#125.56 Billion, quite a bit lower than BAU

#Wise model

ln_mlr1 <-
  Mlr1$coefficients[[1]]*GCData$constant+
  Mlr1$coefficients[[2]]*GCData$ln_GSGDP+
  Mlr1$coefficients[[3]]*GCData$GSgdp_sq+
  Mlr1$coefficients[[4]]*GCData$Gov+
  Mlr1$coefficients[[5]]*GCData$GCco2reduction+
  Mlr1$coefficients[[6]]*GCData$futureagland+
  Mlr1$coefficients[[7]]*GCData$birdspeciesthreatened+
  Mlr1$coefficients[[8]]*GCData$average_population_density+
  Mlr1$coefficients[[9]]*GCData$GCAgGrowth+
  Mlr1$coefficients[[10]]*GCData$terrestrialprotectedareasoftotal+
  Mlr1$coefficients[[11]]*GCData$ln_landarea
GCData$Expmlr1GC <- exp(ln_mlr1)
mlr1Sum <- sum(GCData$Expmlr1GC, na.rm = TRUE)/1E9

#314.277 Billion, quite a bit less than BAU




#Need to apply needs
mlr1Needs <- readRDS("outputs/mlr1Needs.RDS")
RishmanNeeds <- readRDS("outputs/RishmanNeeds.RDS")
#WaldronNeeds<-readRDS("outputs/AnthonyNeeds.RDS")


#Waldron Needs
# ln_needs_waldron <-
#   WaldronNeeds$coefficients[[1]]*GCData$constant+
#   WaldronNeeds$coefficients[[2]]*ln_waldronexp
# GCData$WaldronNeedsGC <- exp(ln_needs_waldron)
# WaldronNeedsSum <- sum(GCData$WaldronNeedsGC, na.rm = TRUE)/1E9 #140.4no change

#Rishman Needs


ln_needs_rishman <- 
  RishmanNeeds$coefficients[[1]]*GCData$constant+
  RishmanNeeds$coefficients[[2]]*ln_rishman+
  RishmanNeeds$coefficients[[3]]*GCData$mammalspeciesthreatened+
  RishmanNeeds$coefficients[[4]]*GCData$oilrentsofgdp+
  RishmanNeeds$coefficients[[5]]*GCData$terrestrialandmarineprotectedare
GCData$RishmanNeedsGC <- exp(ln_needs_rishman)
SumRishmanNeeds <- sum(GCData$RishmanNeedsGC, na.rm = TRUE)/1E9 #140.2

# Wise Needs

ln_needs_mlr1 <-
  mlr1Needs$coefficients[[1]]*GCData$constant+
  mlr1Needs$coefficients[[2]]*ln_mlr1
GCData$mlr1NeedsGC <- exp(ln_needs_mlr1)
Summlr1Needs <- sum(GCData$mlr1NeedsGC, na.rm = TRUE)/1E9 #115.39.14



GCData <- GCData %>%
  arrange(countries)

write.csv(GCData, "data/GCData.csv")
