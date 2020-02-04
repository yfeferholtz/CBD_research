rm(list=ls())

library(devtools)
P <- rprojroot::find_rstudio_root_file

#read in data
data1<-readRDS(P('outputs/dataforneeds.RDS'))

#add extrapolated domestic expenditures to the data frames (pull NA values out of new needs)
library(tidyverse)
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
rob1 <- parameters::standard_error_robust(reg1)

#extrapolated needs
totalneeds1<-sum(exp(reg1$fitted.values))/1E9
#needs for graphing
extrapneeds1 <- predict.lm(reg1)
data3$extrap_needs1<-exp(extrapneeds1)

library(plotly)
#graph of reported needs versus extrapolated
ggplotly(
ggplot(data3)+
  geom_point(aes(countries, newneeds, color="Reported"))+
  geom_point(aes(countries, extrap_needs1, color = "Extrapolated"))+
  xlab("Countries")+
  ylab("Needs"))


#manual extrapolation

ln_extrapneeds1 <-
  reg1$coefficients[[1]]*data1$constant+
  reg1$coefficients[[2]]*data1$lnmanual_extrapdomexp+
  reg1$coefficients[[3]]*data1$Threatened_Species

extrapolate_needs1 <- exp(ln_extrapneeds1)
data1$extrap_needs_manual1<-extrapolate_needs1


#graph with extrapolated needs
ggplotly(
ggplot(data1)+
  geom_point(aes(countries, newneeds, color="Reported"))+
  geom_point(aes(countries, extrap_needs_manual1, color = "Extrapolated"))+
  xlab("Countries")+
  ylab("Needs"))
#more outliers - the extrapolated info for points that don't have all the data have more error



extrapolate_needs_nona <- extrapolate_needs1[is.na(extrapolate_needs1)==FALSE]
sum_new_needs1 <-  sum(extrapolate_needs_nona)/1E9 
#this value is so much higher. Why? Because we used data1 which has all datapoints, even the countries wihtout reported needs

#output table for regression 1
out<- capture.output(stargazer(reg1, title = "Extrapolated Needs w/ ln dom exp from CBD dataselt"))
saveRDS(out, 'outputs/cbd_needs_tables/needs1table')
#regression using extrapolated needs from regression 1 in expenditures


#model 2 using extrapolated needs from country characteriscs. 
reg2 <- lm(ln_newneeds ~lnextrapdomexp1+
             Threatened_Species,
           na.action= na.exclude,
           data3)
#extrapolated needs
totalneeds2<-sum(exp(reg2$fitted.values))/1E9
#needs for graphing
extrapneeds2 <- predict.lm(reg2)
data3$extrap_needs2<-exp(extrapneeds2)

#graph of reported needs versus extrapolated
ggplotly(
ggplot(data3)+
  geom_point(aes(countries, newneeds, color="Reported"))+
  geom_point(aes(countries, extrap_needs2, color = "Extrapolated"))+
  xlab("Countries")+
  ylab("Needs"))

#manual extrapolation using data1 - all data (not just the points with reported needs)
ln_extrapneeds2<-
  reg2$coefficients[[1]]*data1$constant+
  reg2$coefficients[[2]]*data1$lnextrapdomexp1+
  reg2$coefficients[[3]]*data1$Threatened_Species

#exp of ln
extrapolate_needs2 <- exp(ln_extrapneeds2)
data1$extrap_needs_manual2<-extrapolate_needs2

#graph with extrapolated needs
ggplotly(
ggplot(data1)+
  geom_point(aes(countries, newneeds, color="Reported"))+
  geom_point(aes(countries, extrap_needs_manual2, color = "Extrapolated"))+
  xlab("Countries")+
  ylab("Needs"))

#find manual sum total
extrapolate_needs_nona2 <- extrapolate_needs2[is.na(extrapolate_needs2)==FALSE]
sum_new_needs2 <-  sum(extrapolate_needs_nona2)/1E9 


#model 2 manually again, but this time using data3 which was filtered for newneeds>0
ln_extrapneeds2a<-
  reg2$coefficients[[1]]*data3$constant+
  reg2$coefficients[[2]]*data3$lnextrapdomexp1+
  reg2$coefficients[[3]]*data3$Threatened_Species

#exp of ln
extrapolate_needs2a <- exp(ln_extrapneeds2a)
data3$extrap_needs_manual2a<-extrapolate_needs2a

#graph with extrapolated needs
ggplotly(
ggplot(data3)+
  geom_point(aes(countries, newneeds, color="Reported"))+
  geom_point(aes(countries, extrap_needs_manual2a, color = "Extrapolated"))+
  xlab("Countries")+
  ylab("Needs"))

#find manual sum total
extrapolate_needs_nona2a <- extrapolate_needs2a[is.na(extrapolate_needs2a)==FALSE]
sum_new_needs2a <-  sum(extrapolate_needs_nona2a)/1E9
#this sum is the same as the predicted sum from the model because it only takes into account the 
#countries with reported needs. The manual sums found using data1 are extrapolating the needs for countries
#even without all the data. Looking at the graphs it's not as great of a fit, but it does
#report a value that could be used as a total need per year globally.


#output table for regression 1
out<- capture.output(stargazer(reg2, title = "Etrapolated Needs w/ ln dom exp from model with country types"))
saveRDS(out, 'outputs/cbd_needs_tables/needs2table')

#model 3 - extrap ln dom exp from country type 
# I will used data1 for manual calculations
reg3<- lm(ln_newneeds~lnextrapdomexp2
          +Threatened_Species,
          na.action = na.exclude, data3)

#extrapolated needs
totalneeds3<-sum(exp(reg3$fitted.values))/1E9
#needs for graphing
extrapneeds3 <- predict.lm(reg3)
data3$extrap_needs3<-exp(extrapneeds3)


#graph of reported needs versus extrapolated
ggplotly(
ggplot(data3)+
  geom_point(aes(countries, newneeds, color="Reported"))+
  geom_point(aes(countries, extrap_needs3, color = "Extrapolated"))+
  xlab("Countries")+
  ylab("Needs"))


#manual extrapolation using data1 - all data (not just the points with reported needs)
ln_extrapneeds3<-
  reg3$coefficients[[1]]*data1$constant+
  reg3$coefficients[[2]]*data1$lnextrapdomexp2+
  reg3$coefficients[[3]]*data1$Threatened_Species

#exp of ln
extrapolate_needs3 <- exp(ln_extrapneeds3)
data1$extrap_needs_manual3<-extrapolate_needs3

#graph with extrapolated needs
ggplotly(
  ggplot(data1)+
    geom_point(aes(countries, newneeds, color="Reported"))+
    geom_point(aes(countries, extrap_needs_manual2, color = "Extrapolated"))+
    xlab("Countries")+
    ylab("Needs"))

#find manual sum total
extrapolate_needs_nona3 <- extrapolate_needs3[is.na(extrapolate_needs3)==FALSE]
sum_new_needs3 <-  sum(extrapolate_needs_nona3)/1E9 

out<- capture.output(stargazer(reg3, title = "Extrapolated needs from Extrap dom expenditures from country characteristics"))
saveRDS(out, 'outputs/cbd_needs_tables/needs3table')


#Rishman Needs model 2 - using country characteristics

reg4<- lm(ln_newneeds~
            lnGDP+
            ln_newdomexp+
            Threatened_Species+
            urbanpopulationgrowthannual+
            Gov+
            oilrentsofgdp+
            ln_average_co2emmkt,
          na.action = na.exclude, data3)

#summarize
summary(reg4)
#better fit

#find the total extrapolated needs from model
totalextrapneeds4 <- sum(exp(reg4$fitted.values))

#needs for graphing
extrapneeds4 <- predict.lm(reg4)
data3$extrap_needs4<-exp(extrapneeds4)


#graph of reported needs versus extrapolated
ggplotly(
  ggplot(data3)+
    geom_point(aes(countries, newneeds, color="Reported"))+
    geom_point(aes(countries, extrap_needs4, color = "Extrapolated"))+
    xlab("Countries")+
    ylab("Needs"))


#manual extrapolation - using data1 so all countries
ln_extrapneeds4<-
  reg4$coefficients[[1]]*data1$constant+
  reg4$coefficients[[2]]*data1$lnGDP+
  reg4$coefficients[[3]]*data1$ln_newdomexp+
  reg4$coefficients[[4]]*data1$Threatened_Species+
  reg4$coefficients[[5]]*data1$urbanpopulationgrowthannual+
  reg4$coefficients[[6]]*data1$Gov+
  reg4$coefficients[[7]]*data1$oilrentsofgdp+
  reg4$coefficients[[8]]*data1$ln_average_co2emmkt

#exp of manual ln
extrapolated_needs4<-exp(ln_extrapneeds4)
data1$extrap_needs_manual4<-extrapolated_needs4

#graph with extrapolated needs
ggplotly(
  ggplot(data1)+
    geom_point(aes(countries, newneeds, color="Reported"))+
    geom_point(aes(countries, extrap_needs_manual4, color = "Extrapolated"))+
    xlab("Countries")+
    ylab("Needs"))

#find manual sum total
extrapolate_needs_nona4 <- extrapolated_needs4[is.na(extrapolated_needs4)==FALSE]
sum_new_needs4 <-  sum(extrapolate_needs_nona4)/1E9 

out<- capture.output(stargazer(reg4, title = "Extrapolated needs from Extrap dom expenditures from country characteristics"))
saveRDS(out, 'outputs/cbd_needs_tables/needs3table')

#random country test - using Egypt
datatest<- data3 %>% 
  filter(countries == "Egypt")
testneed<- exp(reg4$coefficients[[1]]+
                 reg4$coefficients[[2]]*datatest$lnGDP+
                 reg4$coefficients[[3]]*datatest$ln_newdomexp+
                 reg4$coefficients[[4]]*datatest$Threatened_Species+
                 reg4$coefficients[[5]]*datatest$urbanpopulationgrowthannual+
                 reg4$coefficients[[6]]*datatest$Gov+
                 reg4$coefficients[[7]]*datatest$oilrentsofgdp+
                 reg4$coefficients[[8]]*datatest$ln_average_co2emmkt)

