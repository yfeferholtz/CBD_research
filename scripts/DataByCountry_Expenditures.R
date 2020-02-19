

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
  mutate(new_domexp_max = MaxDomExp*1000) %>%
  mutate(ln_newdomexp_max = log(new_domexp_max)) %>%
  mutate(new_domexp_last = LastDomExp*1000) %>%
  mutate(ln_newdomexp_last = log(new_domexp_last)) %>%
  mutate(new_needs = needs*1000) %>%
  mutate(ln_newneeds = log(new_needs))
#check dataframe
#head(data1,6)

#add constant to data1
constant <- rep(1, 212)
data1<-cbind(data1,constant)

saveRDS(data1, "outputs/dataforexpenditure.RDS")
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


## Expenditures based on country distinctions
# create subset with countries that report dom exp
library(dplyr)
datashort<-data1 %>% 
  filter(is.na(ln_newdomexp)==FALSE) %>% 
  select(ln_newdomexp, lnGDP,
           governmenteffectivenessestimate,
           average_population_density ,
           agriculturallandoflandarea ,
           average_forestarealandarea ,
           GDP_CO2,
           CO2_Ems,
         constant,
         new_domexp,
         countries)

# regression 1 - based on country characteristics - RISHMAN
reg1<- lm(ln_newdomexp~ lnGDP +
            governmenteffectivenessestimate+
            average_population_density +
            agriculturallandoflandarea +
            average_forestarealandarea +
            GDP_CO2+
            CO2_Ems,
          na.action=na.exclude,
            datashort)

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
datashort$extrapdomexp1 <- exp(predict.lm(reg1))
#total extrapolated domestic expenditures -1 
sumextrapdomexp1<- sum(exp(reg1$fitted.values))/1E9


#find difference between fitted values - put in new data.frame
datashort$difference = (datashort$new_domexp-datashort$extrapdomexp1)/1E9
datashort$absdifference = abs(datashort$new_domexp-datashort$extrapdomexp1)/1E9

library(plyr)
data_expend<- datashort %>% 
  select(countries, difference, absdifference, new_domexp) %>% 
  arrange(desc(absdifference)) %>% 
  mutate(new_domexp=as.factor(new_domexp/1E9))

ggplotly(
#plot bar graph of top 10 difference countries
ggplot(head(data_expend, n=10), aes(x=countries, y=difference))+
  geom_bar(stat='identity')+
  ylab("difference - in USD billions")+
  ggtitle("Top 10 largest difference between reported and extrapolated domestic expeditures"))


#Graph extrapolated expenditures against reported
# data1$extrap_dom_exp <- exp(data1$lnextrapdomexp1)
# #how many missing values?
# missing1<-sum(is.na(data1$extrap_dom_exp))
# 
# library(ggplot2)
# ggplot(data1)+
#   geom_point(aes(countries, new_domexp, color="Reported"))+
#   geom_point(aes(countries, extrap_dom_exp, color = "Extrapolated"))+
#   xlab("Countries")+
#   ylab("Domestic Expenditures")

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

library(plotly)
#graph with manual extrapolated expenditures
ggplotly(
ggplot(data1)+
  geom_point(aes(countries, new_domexp, color="Reported"))+
  geom_point(aes(countries, extrap_dom_exp_manual, color = "Extrapolated"))+
  xlab("Countries")+
  ylab("Domestic Expenditures"))

#more outliers...

ln_extrapolate2 <- ln_extrapolate[is.na(ln_extrapolate) == FALSE]
#sum of domestic expenditures
sum_exp1 <- sum(exp(ln_extrapolate2))/1e9 

#test manual extrapolation with subset

# ln_extrapolateshort<-
#   reg1$coefficients[[1]]*datashort$constant+
#   reg1$coefficients[[2]]*datashort$lnGDP+
#   reg1$coefficients[[3]]*datashort$governmenteffectivenessestimate+
#   reg1$coefficients[[4]]*datashort$average_population_density+
#   reg1$coefficients[[5]]*datashort$agriculturallandoflandarea+
#   reg1$coefficients[[6]]*datashort$average_forestarealandarea+
#   reg1$coefficients[[7]]*datashort$GDP_CO2+
#   reg1$coefficients[[8]]*datashort$CO2_Ems
# 
# ln_extrapolateshort_nona<- ln_extrapolateshort[is.na(ln_extrapolateshort)==FALSE]
# sumshort <- sum(exp(ln_extrapolateshort_nona))/1E9






#Run the model with OECD 

#create subset
data2 <- data1 %>% 
  filter(new_domexp>0) %>% 
  select(countries,
         OECD,
         type,
         expected_funding_gap,
         agriculturallandoflandarea,
         birdspeciesthreatened,
         mammalspeciesthreatened,
         Threatened_Species,
         oilrentsofgdp,
         urbanpopulationgrowthannual,
         populationgrowthannual,
         CO2_Ems,
         GDP_CO2,
         lnGDP,
         constant,
         new_domexp,
         ln_newdomexp,
         average_agriculture_land,
         average_forestarealandarea,
         governmenteffectivenessestimate, average_population_density
         )
#add dummies for type of country
library(fastDummies)
data2 <- fastDummies::dummy_cols(data2, select_columns = "type")
#run the model

model1<-lm(ln_newdomexp ~ lnGDP+
             governmenteffectivenessestimate+
             average_population_density +
             agriculturallandoflandarea +
             average_forestarealandarea +
             GDP_CO2+
             CO2_Ems+
             OECD,
           na.action=na.exclude,
           data2)
#summarize the model
summary(model1)
#predicted expenditures
data2$extrapdomexp <- exp(predict.lm(reg1))
#predicted total sum of expneidtures for countries that have reported
model1sum <- sum(data2$extrapdomexp[is.na(data2$extrapdomexp)==FALSE])/1E9

#find difference between fitted values - put in new data.frame
data2$difference = (data2$new_domexp-data2$extrapdomexp)/1E9
data2$absdifference = abs(data2$new_domexp-data2$extrapdomexp)/1E9

data2<-arrange(data2, desc(absdifference)) 
ggplotly(
  #plot bar graph of top 10 difference countries
ggplot(head(data2, n=10), aes(x=countries, y=difference))+
  geom_bar(stat='identity')+
  ylab("difference - in USD billions")+
  ggtitle("Top 10 largest difference between reported and extrapolated domestic expeditures (OECD)"))






#add dummies for type of country
data1 <- fastDummies::dummy_cols(data1, select_columns = "type")

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




