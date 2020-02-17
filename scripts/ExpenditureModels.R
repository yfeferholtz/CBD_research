#streamline expenditure models - removing major outliers.

rm(list=ls())

library(devtools)
P <- rprojroot::find_rstudio_root_file

cbd_data<- readRDS("outputs/dataforexpenditure.RDS")

# create subset with countries that report dom exp
library(dplyr)
reported_data<-cbd_data %>% 
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
         ln_newdomexp_last,
         ln_newdomexp_max,
         new_domexp_last,
         new_domexp_max,
         countries)

# regression 1 - based on country characteristics - original RISHMAN

reg1<- lm(ln_newdomexp_last~ lnGDP +
            governmenteffectivenessestimate+
            average_population_density +
            agriculturallandoflandarea +
            average_forestarealandarea +
            GDP_CO2+
            CO2_Ems,
          na.action=na.exclude,
          reported_data)
summary(reg1)

#extrapolated domestic expenditures from fitted values
reported_data$extrapdomexp1 <- exp(predict.lm(reg1))
#total extrapolated domestic expenditures -1 
sumextrapdomexp1<- sum(exp(reg1$fitted.values))/1E9

#find difference between fitted values - put in new data.frame
reported_data$difference1 = (reported_data$new_domexp-reported_data$extrapdomexp1)/1E9
reported_data$absdifference1 = abs(reported_data$new_domexp-reported_data$extrapdomexp1)/1E9

library(plyr)
data_expend<- reported_data %>% 
  select(countries, difference1, absdifference1, new_domexp) %>% 
  arrange(desc(absdifference1)) %>% 
  mutate(new_domexp=as.factor(new_domexp/1E9))
library(plotly)

  #plot bar graph of top 10 difference countries
  ggplot(head(data_expend, n=10), aes(x=reorder(countries, difference1), y=difference1))+
    geom_bar(stat='identity')+
    ylab("difference - in USD billions")+
    ggtitle("Top 10 largest difference between reported and extrapolated domestic expeditures")+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  #drop top 5 largest culprits. 
  N<-2
  altered_data<- reported_data %>% 
    arrange(desc(absdifference1)) %>% 
  tail(-N)

#run model again --- not sure how to make this a loop
  

  reg2<- lm(ln_newdomexp~ lnGDP +
              governmenteffectivenessestimate+
              average_population_density +
              agriculturallandoflandarea +
              average_forestarealandarea +
              GDP_CO2+
              CO2_Ems,
            na.action=na.exclude,
            altered_data)
  summary(reg2)
  #lower r^2 - less observations?
  
  #extrapolated domestic expenditures from fitted values
  altered_data$extrapdomexp2 <- exp(predict.lm(reg2))
  #total extrapolated domestic expenditures -1 
  sumextrapdomexp2<- sum(exp(reg2$fitted.values))/1E9
  #much lower total expenditures for subset of reporting countries
  
  #find difference between fitted values - put in new data.frame
 altered_data$difference2 = (altered_data$new_domexp-altered_data$extrapdomexp2)/1E9
 altered_data$absdifference2 = abs(altered_data$new_domexp-altered_data$extrapdomexp2)/1E9
  

  data_expend<- altered_data %>% 
    select(countries, difference2, absdifference2, new_domexp) %>% 
    arrange(desc(absdifference2))

  #plot bar graph of top 10 difference countries
  ggplot(head(data_expend, n=10), aes(x=reorder(countries, difference2), y=difference2))+
    geom_bar(stat='identity')+
    ylab("difference - in USD billions")+
    ggtitle("Top 10 largest difference between reported and extrapolated domestic expeditures")+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  #drop China culprit. 
  N<-1
  altered_data<- altered_data %>% 
    arrange(desc(absdifference2)) %>% 
    tail(-N)

#one more time without China

  reg3<- lm(ln_newdomexp~ lnGDP +
              governmenteffectivenessestimate+
              average_population_density +
              agriculturallandoflandarea +
              average_forestarealandarea +
              GDP_CO2+
              CO2_Ems,
            na.action=na.exclude,
            altered_data)
  summary(reg3)
  #lower r^2 - less observations
  
  #extrapolated domestic expenditures from fitted values
  altered_data$extrapdomexp3 <- exp(predict.lm(reg3))
  #total extrapolated domestic expenditures -1 
  sumextrapdomexp3<- sum(exp(reg3$fitted.values))/1E9
  #much lower total expenditures for subset of reporting countries
  
  #find difference between fitted values - put in new data.frame
  altered_data$difference3 = (altered_data$new_domexp-altered_data$extrapdomexp3)/1E9
  altered_data$absdifference3 = abs(altered_data$new_domexp-altered_data$extrapdomexp3)/1E9
  
  
  data_expend<- altered_data %>% 
    select(countries, difference3, absdifference3, new_domexp) %>% 
    arrange(desc(absdifference3))
  
  #plot bar graph of top 10 difference countries
  ggplot(head(data_expend, n=10), aes(x=reorder(countries, difference3), y=difference3))+
    geom_bar(stat='identity')+
    ylab("difference - in USD billions")+
    ggtitle("Top 10 largest difference between reported and extrapolated domestic expeditures")+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
 
#using full dataset to extract expenditures for countries that have not reported - using reg3 without the large outliers
  
  ln_extrapolate<-
    reg1$coefficients[[1]]*cbd_data$constant+
    reg1$coefficients[[2]]*cbd_data$lnGDP+
    reg1$coefficients[[3]]*cbd_data$governmenteffectivenessestimate+
    reg1$coefficients[[4]]*cbd_data$average_population_density+
    reg1$coefficients[[5]]*cbd_data$agriculturallandoflandarea+
    reg1$coefficients[[6]]*cbd_data$average_forestarealandarea+
    reg1$coefficients[[7]]*cbd_data$GDP_CO2+
    reg1$coefficients[[8]]*cbd_data$CO2_Ems

#find total predicted expenditures for all countries with these characteristics reported.  
  ln_extrapolate2 <- ln_extrapolate[is.na(ln_extrapolate) == FALSE]
  #sum of domestic expenditures
  sum_exp2 <- sum(exp(ln_extrapolate2))/1e9 
#  this total sum is still less than just the sum of reporting countries from reg1

    
  