

rm(list=ls())

library(devtools)
P <- rprojroot::find_rstudio_root_file

#import data
require(readxl)
data_by_country<- read_excel(P("data/data_by_country.xlsx"))

#data in thousands
require(dplyr)
data1 <- data_by_country %>%
  mutate(new_domexp = dom_exp*1000) %>%
  mutate(ln_newdomexp = log(new_domexp)) %>%
  mutate(new_needs = needs*1000) %>%
  mutate(ln_newneeds = log(new_needs))
#check dataframe
head(data1,6)

#add constant to data1
constant <- rep(1, 212)
data1<-cbind(data1,constant)

#create df to test correlation
data2<-data1 %>% 
  select(lndomexp_thousand, lnGDP, governmenteffectivenessestimate, average_population_density, agriculturallandoflandarea, average_forestarealandarea, GDP_CO2, CO2_Ems, Threatened_Species, populationgrowthannual, Gov,  oilrentsofgdp,ln_average_co2emmkt)
res<- cor(data2,use = "complete.obs")

#print correlation
round(res)
#Gov is Principal Compenance 

#save table
library(stargazer)
out<-capture.output(stargazer(res))
saveRDS(out, P('/outputs/cbd_exp_tables/corr_table_1.RDS'))

#add dummies for type of country
library(fastDummies)
data1 <- fastDummies::dummy_cols(data1, select_columns = "type")

## Expenditures based on country distinctions

# regression 1 - based on country characteristics - RISHMAN
reg1<- lm(ln_newdomexp~ lnGDP +
            governmenteffectivenessestimate+
            average_population_density +
            agriculturallandoflandarea +
            average_forestarealandarea +
            GDP_CO2+
            CO2_Ems,
            na.action=na.exclude, data=data1)
# na.exclude should pad the fitted values and residuals, but it's not?

library(sjstats)
#check robustness
rob1<- parameters::standard_error_robust(reg1)

#output tables for regression 1 and robustness
out2<-capture.output(stargazer(reg1))
saveRDS(out2, P('/outputs/cbd_exp_tables/regression_table_1.RDS'))
out3<-stargazer(rob1)
saveRDS(out3, P('/outputs/cbd_exp_tables/robust_table_1.RDS'))
summary(reg1)

#extrapolated domestic expenditures from fitted values
data1$lnextrapdomexp1 <- predict.lm(reg1)
#total extrapolated domestic expenditures -1 
sumextrapdomexp1<- sum(exp(reg1$fitted.values))/1E9

#Graph extrapolated expenditures against reported

data1$extrap_dom_exp <- exp(data1$lnextrapdomexp1)
#how many missing values?
missing1<-sum(is.na(data1$extrap_dom_exp))

library(ggplot2)
ggplot(data1)+
  geom_point(aes(countries, new_domexp, color="Reported"))+
  geom_point(aes(countries, extrap_dom_exp, color = "Extrapolated"))+
  xlab("Countries")+
  ylab("Domestic Expenditures")

#manual extrapolate 

ln_extrapolate<-
  reg1$coefficients[[1]]*data1$constant+
  reg1$coefficients[[2]]*data1$lnGDP+
  reg1$coefficients[[3]]*data1$governmenteffectivenessestimate+
  reg1$coefficients[[4]]*data1$average_population_density+
  reg1$coefficients[[5]]*data1$agriculturallandoflandarea+
  reg1$coefficients[[6]]*data1$average_forestarealandarea+
  reg1$coefficients[[7]]*data1$GDP_CO2+
  reg1$coefficients[[8]]*data1$CO2_Ems


extrapdomexp_manual1<-exp(ln_extrapolate)
data1$extrap_dom_exp_manual<-extrapdomexp_manual1

#how many missing values?
missing2<-sum(is.na(data1$extrap_dom_exp_manual))
#manual extrapolation gives a lot more data because it doesn't drop countries missing one characteristic


#graph with manual extrapolated expenditures
ggplot(data1)+
  geom_point(aes(countries, new_domexp, color="Reported"))+
  geom_point(aes(countries, extrap_dom_exp_manual, color = "Extrapolated"))+
  xlab("Countries")+
  ylab("Domestic Expenditures")

#more outliers...

ln_extrapolate2 <- ln_extrapolate[is.na(ln_extrapolate) == FALSE]
#sum of domestic expenditures
sum_exp1 <- sum(exp(ln_extrapolate2))/1e9 

 
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
#check robustness
rob2<-parameters::standard_error_robust(reg2)

#output table
out4<-capture.output(stargazer(reg2))
saveRDS(out4, P('/outputs/cbd_exp_tables/regression_table_2.RDS'))

#extrapolated expenditures
sumextrapodomexp2<-sum(exp(reg2$fitted.values))/1E9

#for graph
data1$lnextrapdomexp2<-predict.lm(reg2)
data1$extrap_domexp2<-exp(data1$lnextrapdomexp2)

ggplot(data1)+
  geom_point(aes(countries, new_domexp, color="Reported"))+
  geom_point(aes(countries, extrap_domexp2, color = "Extrapolated"))+
  xlab("Countries")+
  ylab("Domestic Expenditures")

#manually

ln_extrapolate_exp <- 
  reg2$coefficients[[1]]*data1$constant +
  reg2$coefficients[[2]]*data1$lnGDP +
  reg2$coefficients[[3]]*data1$agriculturallandoflandarea +
  reg2$coefficients[[4]]*data1$governmenteffectivenessestimate +
  reg2$coefficients[[5]]*data1$average_forestarealandarea +
  reg2$coefficients[[6]]*data1$type_Developed_Countries +
  reg2$coefficients[[7]]*data1$type_Developing +
  reg2$coefficients[[8]]*data1$type_Least_dev

data1$extrap_dom_exp_manual2<- exp(ln_extrapolate_exp)

ggplot(data1)+
  geom_point(aes(countries, new_domexp, color="Reported"))+
  geom_point(aes(countries, extrap_dom_exp_manual2, color = "Extrapolated"))+
  xlab("Countries")+
  ylab("Domestic Expenditures")
#both have much higher outliers than model 1

ln_extrapolate_exp2 <- ln_extrapolate_exp[is.na(ln_extrapolate_exp) == FALSE]
extrapolate_ex2p <- exp(ln_extrapolate_exp2)
sum_exp2 <- sum(extrapolate_ex2p)/1e9 

dataforneeds<-data1 %>% 
  select(constant,
         countries, 
         type,
         newneeds,
         ln_newneeds, 
         lnGDP, 
         Threatened_Species,
         urbanpopulationgrowthannual,
         GDPgrowth,
         Gov,
         forestrentsofgdp,
         oilrentsofgdp,
         lnmanual_extrapdomexp,
         ln_newdomexp,
         ln_average_co2emmkt,
         extrap_domexp2,
         extrap_dom_exp,
         lnextrapdomexp1,
         lnextrapdomexp2)
saveRDS(dataforneeds, "outputs/dataforneeds.RDS")

#random country test using Egypt
testexp<- exp(reg1$coefficients[[1]]*datatest$constant+
                reg1$coefficients[[2]]*datatest$lnGDP+
                reg1$coefficients[[3]]*datatest$governmenteffectivenessestimate+
                reg1$coefficients[[4]]*datatest$average_population_density+
                reg1$coefficients[[5]]*datatest$agriculturallandoflandarea+
                reg1$coefficients[[6]]*datatest$average_forestarealandarea+
                reg1$coefficients[[7]]*datatest$GDP_CO2+
                reg1$coefficients[[8]]*datatest$CO2_Ems)


