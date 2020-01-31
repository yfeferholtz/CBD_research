rm(list=ls())

library(devtools)
P <- rprojroot::find_rstudio_root_file

#read in data
data1<-readRDS(P('outputs/dataforneeds.RDS'))

#add extrapolated domestic expenditures to the data frames (pull NA values out of new needs)
data3 <-  data1%>%
  filter(ln_newneeds>0)

#add type dummies to data3
library(fastDummies)
data3 <- fastDummies::dummy_cols(data3, select_columns = "type")

#regression based on Rishman's manual input expenditures
reg1 <- lm(ln_newneeds~
             lnmanual_extrapdomexp +
             Threatened_Species, 
           na.action=na.exclude,
           data = data3)
#robustness test
library(sjstats)
rob1 <- robust(reg1)

#extrapolated needs
totalneeds1<-sum(exp(reg1$fitted.values))/1E9
#needs for graphing
extrapneeds1 <- predict.lm(reg1)
data3$extrap_needs1<-exp(extrapneeds1)

#graph of reported needs versus extrapolated
ggplot(data3)+
  geom_point(aes(countries, newneeds, color="Reported"))+
  geom_point(aes(countries, extrap_needs1, color = "Extrapolated"))+
  xlab("Countries")+
  ylab("Needs")


#manual extrapolation

ln_extrapneeds1 <-
  reg1$coefficients[[1]]*data1$constant+
  reg1$coefficients[[2]]*data1$lnmanual_extrapdomexp+
  reg1$coefficients[[3]]*data1$Threatened_Species

extrapolate_needs1 <- exp(ln_extrapneeds1)
data1$extrap_needs_manual1<-extrapolate_needs1

#graph with extrapolated needs
ggplot(data1)+
  geom_point(aes(countries, newneeds, color="Reported"))+
  geom_point(aes(countries, extrap_needs_manual1, color = "Extrapolated"))+
  xlab("Countries")+
  ylab("Needs")
#more outliers


extrapolate_needs_nona <- extrapolate_needs1[is.na(extrapolate_needs1)==FALSE]
sum_new_needs1 <-  sum(extrapolate_needs_nona)/1E9 

#output table for regression 1
out<- capture.output(stargazer(reg1, title = "Extrapolated Needs w/ ln dom exp from CBD dataselt"))

#regression using extrapolated needs from regression 1 in expenditures


#stopping point 1/28/20 2:36pm
reg2 <- lm(ln_newneeds ~lnextrapdomexp1+
             Threatened_Species,
           na.action= na.exclude,
           data3)

extrapneeds2<- predict.lm(reg2)

ln_extrapneeds2<-
  reg2$coefficients[[1]]*data3$constant+
  reg2$coefficients[[2]]*data3$ln_extrapolate_exp+
  reg2$coefficients[[3]]*data3$Threatened_Species

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


