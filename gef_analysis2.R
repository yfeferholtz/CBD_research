###GEF analysis###

rm(list=ls())

library(devtools)
P <- rprojroot::find_rstudio_root_file

#import data
require(readxl)
gef6_data <- read_excel(P("data/data collection template.xlsx"), sheet=4)
gef7_data<- read_excel(P("data/data collection template.xlsx"), sheet=3)
exp_data<- readRDS("outputs/dataforexpenditure.RDS")

#merge datasets
gef_data <- merge(gef6_data, gef7_data, by= "Countries", all=TRUE)

library(dplyr)
gef_data <- gef_data %>%
  rename(GEF6=`GEF-6 only grants`,
         GEF7= `GEF-7 only grants`,
         countries= `Countries`)%>%
  select(countries, GEF6, GEF7 )

full_data <- merge(gef_data, exp_data, by= "countries", all=TRUE)

#Regression using GEF 6
  #Rishman model
  reg1<- lm(GEF6~ lnGDP +
            governmenteffectivenessestimate+
            average_population_density +
            agriculturallandoflandarea +
            average_forestarealandarea +
            GDP_CO2+
            CO2_Ems,
          na.action=na.exclude,
          full_data)
  summary(reg1) #adj r2 =.2868

  #predict GEF6
  full_data$predictgef6 <- predict.lm(reg1)
  sum(full_data$predictgef6, na.rm=TRUE)  # $1,384,262,147 compared to
                                          # the actual value $1,473,376,913 
  #difference between fitted and observed
  full_data <- full_data %>%
    mutate(diff_gef6 = GEF6-predictgef6) %>%
    mutate(abs_diff_gef6 = abs(diff_gef6))%>%
    arrange(desc(abs_diff_gef6))

  #plot biggest differences
  library(ggplot2)
  pos <- head(full_data$diff_gef6>=0, n=10) #makes the graph colored based on sign
  ggplot(head(full_data, n=10), aes(x=reorder(countries, diff_gef6), y=diff_gef6/1E6, fill=pos))+
    geom_bar(stat='identity')+
    ylab("difference - in USD millions")+
    ggtitle("Top 10 largest difference between actual and predicted GEF6")+
    theme_minimal()+
    theme(legend.position = "none", axis.title.x=element_blank())+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
   #Brazil is the biggest outlier by far, let's drop it :)
  N<-1
  drop_brazil<- full_data %>% 
    arrange(desc(abs_diff_gef6)) %>% 
    tail(-N)

    #Run it again
  reg2<- lm(GEF6~ lnGDP +
              governmenteffectivenessestimate+
              average_population_density +
              agriculturallandoflandarea +
              average_forestarealandarea +
              GDP_CO2+
              CO2_Ems,
            na.action=na.exclude,
            drop_brazil)
  summary(reg2) #adj r2 =.265, this is actually worse
  
  #predict GEF6
  drop_brazil$predictgef6 <- predict.lm(reg2)
  sum(drop_brazil$predictgef6, na.rm=TRUE)  # $1,269,577,774 compared to
                                            # $1,384,262,147 compared to
                                            # the actual value $1,473,376,913 
                                            # This regression is just getting worse
  
  #difference between fitted and observed
  drop_brazil <- drop_brazil %>%
    mutate(diff_gef6 = GEF6-predictgef6) %>%
    mutate(abs_diff_gef6 = abs(diff_gef6))%>%
    arrange(desc(abs_diff_gef6))
  
  #graph top 10 outliers
  pos <- head(drop_brazil$diff_gef6>=0, n=10) #makes the graph colored based on sign
  ggplot(head(drop_brazil, n=10), aes(x=reorder(countries, diff_gef6), y=diff_gef6/1E6, fill=pos))+
    geom_bar(stat='identity')+
    ylab("difference - in USD millions")+
    ggtitle("Top 10 largest difference between actual and predicted GEF6")+
    theme_minimal()+
    theme(legend.position = "none", axis.title.x=element_blank())+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  #now China is the biggest outlier, but not by much
  
#Same regressions but for GEF7 
  reg3<- lm(GEF7~ lnGDP +
              governmenteffectivenessestimate+
              average_population_density +
              agriculturallandoflandarea +
              average_forestarealandarea +
              GDP_CO2+
              CO2_Ems,
            na.action=na.exclude,
            full_data)
  summary(reg3) #adj r2 =.2565
  
  #predict GEF7
  full_data$predictgef7 <- predict.lm(reg3)
  sum(full_data$predictgef7, na.rm=TRUE)  # $600,809,034 compared to
                                          # the actual value $642,470,139  
  #difference between fitted and observed
  full_data <- full_data %>%
    mutate(diff_gef7 = GEF7-predictgef7) %>%
    mutate(abs_diff_gef7 = abs(diff_gef7))%>%
    arrange(desc(abs_diff_gef7))
  
  #Plot the top 10 outliers
  pos <- head(full_data$diff_gef7>=0, n=10) #makes the graph colored based on sign
  ggplot(head(full_data, n=10), aes(x=reorder(countries, diff_gef7), y=diff_gef7/1E6, fill=pos))+
    geom_bar(stat='identity')+
    ylab("difference - in USD millions")+
    ggtitle("Top 10 largest difference between actual and predicted GEF7")+
    theme_minimal()+
    theme(legend.position = "none", axis.title.x=element_blank())+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # No giant outlier...  
    
  


