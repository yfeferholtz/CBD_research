require(readxl)
require(lubridate)
require(tidyverse)
require(sf)
require(ggplot2)
require(dplyr)
require(stargazer)
require(Hmisc)
require(sjstats)

rm(list=ls())


setwd("~/Dropbox/RM CBD Project/data")
data_by_country<- read_excel("data_by_country.xlsx")

data1 <- data_by_country %>%
  mutate(new_domexp = dom_exp*1000) %>%
  mutate(ln_newdomexp = log(new_domexp)) %>%
  mutate(new_needs = needs*1000) %>%
  mutate(ln_newneeds = log(new_needs))

head(data1,6)

constant <- rep(1, 212)
data1<-cbind(data1,constant)
#create df to test correlation
data2<-data1 %>% 
  select(lndomexp_thousand, lnGDP, governmenteffectivenessestimate, average_population_density, agriculturallandoflandarea, average_forestarealandarea, GDP_CO2, CO2_Ems, Threatened_Species, populationgrowthannual, Gov,  oilrentsofgdp,ln_average_co2emmkt)
res<- cor(data2,use = "complete.obs")
round(res,2)
#Gov is Principal Compenance 

stargazer(res)


library(fastDummies)
data1 <- fastDummies::dummy_cols(data1, select_columns = "type")

## Expenditures based on country distinctions

##na.exclude should pad the fitted values and residuals, but it's not?
reg1<- lm(ln_newdomexp~ lnGDP +
            governmenteffectivenessestimate+
            average_population_density +
            agriculturallandoflandarea +
            average_forestarealandarea +
            GDP_CO2+
            CO2_Ems,
            na.action=na.exclude, data=data1)
rob1<- robust(reg1)
stargazer(reg1)
stargazer(rob1)
summary(reg1)
extrapdomexp <- predict.lm(reg1)
data1$FV1 <- extrapdomexp


ln_extrapolate<-
  reg1$coefficients[[1]]*data1$constant+
  reg1$coefficients[[2]]*data1$lnGDP+
  reg1$coefficients[[3]]*data1$governmenteffectivenessestimate+
  reg1$coefficients[[4]]*data1$average_population_density+
  reg1$coefficients[[5]]*data1$agriculturallandoflandarea+
  reg1$coefficients[[6]]*data1$average_forestarealandarea+
  reg1$coefficients[[7]]*data1$GDP_CO2+
  reg1$coefficients[[8]]*data1$CO2_Ems

ln_extrapolate2 <- ln_extrapolate[is.na(ln_extrapolate) == FALSE]
extrapolate_exp <- exp(ln_extrapolate2)
sum_exp <- sum(extrapolate_exp)/1e9 

 ## Expenditures based on country type

reg2 <- lm(ln_newdomexp ~ 
             lnGDP + 
             agriculturallandoflandarea +
             governmenteffectivenessestimate +
             average_forestarealandarea+
             type_Developed_Countries +
             type_Developing +
             type_Least_dev, 
           na.action=na.exclude,
           data1)

rob2<-robust(reg2)
stargazer(reg2)

extrapdomexp2<-predict.lm(reg2)
data1$FV2<-extrapdomexp2

ln_extrapolate_exp <- 
  reg2$coefficients[[1]]*data1$constant +
  reg2$coefficients[[2]]*data1$lnGDP +
  reg2$coefficients[[3]]*data1$agriculturallandoflandarea +
  reg2$coefficients[[4]]*data1$governmenteffectivenessestimate +
  reg2$coefficients[[5]]*data1$average_forestarealandarea +
  reg2$coefficients[[6]]*data1$type_Developed_Countries +
  reg2$coefficients[[7]]*data1$type_Developing +
  reg2$coefficients[[8]]*data1$type_Least_dev

ln_extrapolate_exp2 <- ln_extrapolate_exp[is.na(ln_extrapolate_exp) == FALSE]
extrapolate_ex2p <- exp(ln_extrapolate_exp2)
sum_exp2 <- sum(extrapolate_exp)/1e9 

## Model to find needs

#create a subset of necessary data from original dataset

data1_sub<- data1 %>% 
  select(countries,
         type,
         ln_newneeds, 
         lnGDP, 
         Threatened_Species,
         urbanpopulationgrowthannual,
         GDPgrowth,
         Gov,
         forestrentsofgdp,
         oilrentsofgdp,
         lnmanual_extrapdomexp,
         ln_newdomexp) 

#add extrapolated domestic expenditures to the data frames (pull NA values out of new needs)
data3 <-  cbind(data1_sub, ln_extrapolate_exp, ln_extrapolate) %>%
  filter(ln_newneeds>0)
#data.frame with NA values 
data4 <- cbind(data1_sub, ln_extrapolate_exp, ln_extrapolate, constant)
data3 <- fastDummies::dummy_cols(data3, select_columns = "type")

reg3 <- lm(ln_newneeds~
             lnmanual_extrapdomexp +
             Threatened_Species, 
           na.action=na.exclude,
           data = data3)
rob3 <- robust(reg3)
extrapneeds1 <- predict.lm(reg3)
ln_extrapneeds1 <-
  reg3$coefficients[[1]]*data4$constant+
  reg3$coefficients[[2]]*data4$lnmanual_extrapdomexp+
  reg3$coefficients[[3]]*data4$Threatened_Species

extrapolate_needs1 <- exp(ln_extrapneeds1)
extrapolate_needs_nona <- extrapolate_needs1[is.na(extrapolate_needs1)==FALSE]
sum_new_needs1 <-  sum(extrapolate_needs_nona)/1E9 

stargazer(reg3, title = "Extrapolated Needs w/ ln dom exp from CBD dataselt")

reg4 <- lm(ln_newneeds ~ ln_extrapolate_exp+
             Threatened_Species,
           na.action= na.exclude,
           data3)

extrapneeds2<- predict.lm(reg4)

ln_extrapneeds2<-
  reg4$coefficients[[1]]*data4$constant+
  reg4$coefficients[[2]]*data4$ln_extrapolate_exp+
  reg4$coefficients[[3]]*data4$Threatened_Species

extrapolate_needs2 <- exp(ln_extrapneeds2)
extrapolate_needs_nona2 <- extrapolate_needs2[is.na(extrapolate_needs2)==FALSE]
sum_new_needs2 <- sum(extrapolate_needs_nona2)

stargazer(reg4, title = "Etrapolated Needs w/ ln dom exp from model with country types")
reg5<- lm(ln_newneeds~ln_extrapolate+Threatened_Species,
          na.action = na.exclude, data3)

extrapneeds3 <- exp(reg5$fitted.values)

ln_extrapneeds3 <-
  reg5$coefficients[[1]]*data4$constant+
  reg5$coefficients[[2]]*data4$ln_extrapolate+
  reg5$coefficients[[3]]*data4$Threatened_Species

extrapolated_needs3 <- exp(ln_extrapneeds3)
extrapolat_needs_nona3 <- extrapolated_needs3[is.na(extrapolated_needs3)==FALSE]
sum_new_needs3 <- sum(extrapolat_needs_nona3)/1E9

sum_new_needs3other<-sum(extrapneeds3)


stargazer(reg5, title = "Extrapolated needs from Extrap dom expenditures from country characteristics")

#Rishman Needs model 2
data5<- data1 %>% 
  filter(ln_newneeds>0)

reg6<- lm(ln_newneeds~
            lnGDP+ln_newdomexp+Threatened_Species+urbanpopulationgrowthannual+Gov+oilrentsofgdp+ln_average_co2emmkt,
          na.action = na.exclude, data5)
extrapneeds4 <- reg6$fitted.values

ln_extrapneeds4<-
  reg6$coefficients[[1]]*data1$constant+
  reg6$coefficients[[2]]*data1$lnGDP+
  reg6$coefficients[[3]]*data1$ln_newdomexp+
  reg6$coefficients[[4]]*data1$Threatened_Species+
  reg6$coefficients[[5]]*data1$urbanpopulationgrowthannual+
  reg6$coefficients[[6]]*data1$Gov+
  reg6$coefficients[[7]]*data1$oilrentsofgdp+
  reg6$coefficients[[8]]*data1$ln_average_co2emmkt

extrapolated_needs4<-exp(ln_extrapneeds4)
Extrapneeds4_nona<-extrapolated_needs4[is.na(extrapolated_needs4)==FALSE]
sum_new_needs4<-sum(Extrapneeds4_nona)/1E9

stargazer(reg6)

datatest<- data5 %>% 
  filter(countries == "Egypt")
testneed<- exp(reg6$coefficients[[1]]+
  reg6$coefficients[[2]]*datatest$lnGDP+
  reg6$coefficients[[3]]*datatest$ln_newdomexp+
  reg6$coefficients[[4]]*datatest$Threatened_Species+
  reg6$coefficients[[5]]*datatest$urbanpopulationgrowthannual+
  reg6$coefficients[[6]]*datatest$Gov+
  reg6$coefficients[[7]]*datatest$oilrentsofgdp+
  reg6$coefficients[[8]]*datatest$ln_average_co2emmkt)

testexp<- exp(reg1$coefficients[[1]]*datatest$constant+
                reg1$coefficients[[2]]*datatest$lnGDP+
                reg1$coefficients[[3]]*datatest$governmenteffectivenessestimate+
                reg1$coefficients[[4]]*datatest$average_population_density+
                reg1$coefficients[[5]]*datatest$agriculturallandoflandarea+
                reg1$coefficients[[6]]*datatest$average_forestarealandarea+
                reg1$coefficients[[7]]*datatest$GDP_CO2+
                reg1$coefficients[[8]]*datatest$CO2_Ems)
  
  
  