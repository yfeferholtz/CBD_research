###CBD-GEF Analysis###
#load packages


library(broom)

library(devtools)
P <- rprojroot::find_rstudio_root_file

#import data
gef_data <- read.csv(P('data/gef-projects.csv'))

#Clean data
gef_data$Grant <- gsub("$", "", gef_data$Grant, fixed=TRUE)
gef_data$Grant <- gsub(",", "", gef_data$Grant, fixed=TRUE)
gef_data$Grant <- as.numeric(gef_data$Grant)
gef_data$Cofinancing <- gsub("$", "", gef_data$Cofinancing, fixed=TRUE)
gef_data$Cofinancing <- gsub(",", "", gef_data$Cofinancing, fixed=TRUE)
gef_data$Cofinancing <- as.numeric(gef_data$Cofinancing)

# create new variable called sum_grant
library(dplyr)
gef_data <- gef_data %>%
  group_by(Countries) %>%
  mutate(sum_grant = sum(Grant))

#Funding for biodiversity
library(stringr)
biodiv_data <- gef_data %>%
  group_by(Countries)%>%
  filter(str_detect(Focal.Areas,"Biodiversity"))
biodiv_data <- biodiv_data %>%
  group_by(Countries)%>%
  mutate(sum_biodiv=sum(Grant))

#plot biodiversity funding
library(ggplot2)
ggplot(biodiv_data) +
  geom_histogram(aes(sum_biodiv/1000000),color="black", fill="lightgray", bins=15)+
  scale_x_log10()+
  labs(title="Historgram of biodiversity funding",
       x="Total grants per country (millions USD)",
       y="Count")+
  theme_classic()

#Difference by country over time
#convert period to numeric
biodiv_data$Period <- gsub("GEF - ", "", biodiv_data$Period, fixed=TRUE)
biodiv_data$Period <- as.numeric(biodiv_data$Period)

#Plot funding over time
afg <- biodiv_data %>%
  filter(Countries == "Afghanistan") #filtering for a specific country

pdf(P('outputs/funding_per_period.pdf'))
ggplot(afg, aes(x=Period, y=Grant/1000000))+ #rescale depending on magnitude
  geom_bar(stat="identity", fill="navy")+
  labs(title="Funding in each Project Period - Afghanistan",
       x="GEF Period",
       y="Grant amount (millions USD)")+
  theme_classic()
dev.off()

#Plot Funding for all countries by period
ggplot(biodiv_data,aes(x=Period, y=Grant/1000000000))+
  geom_bar(stat="identity", fill="navy")+
  labs(title="Total Funding in each Project Period",
       x="GEF Period",
       y="Grant amount (billions USD)")+
  theme_classic()

#Create change in grants variable
biodiv_data <- biodiv_data %>%
  group_by(Countries) %>% 
  arrange(desc(Period), .by_group = TRUE) %>%
  mutate(delta_grant = (Grant-lag(Grant)))

#plot changes
ggplot(biodiv_data,aes(x=Period, y=(delta_grant/1000000)))+
  geom_bar(stat="identity", fill="navy")+
  labs(title="Change in Funding in each Project Period",
       x="GEF Period",
       y="Change in Grant amount (millions USD)")+
  theme_classic()

mean(biodiv_data$delta_grant, na.rm=TRUE)
t.test(biodiv_data$delta_grant, mu=0)  
  






