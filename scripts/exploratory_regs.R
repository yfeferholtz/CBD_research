#Exploratory Regressions
library(readxl)
library(dplyr)
library(stringr)
library(devtools)
P <- rprojroot::find_rstudio_root_file

# read data
data_by_country<- read_excel(P("data/data_by_country.xlsx"))

# clean data
data1 <- data_by_country %>%
  mutate(new_domexp = dom_exp*1000) %>%
  mutate(ln_newdomexp = log(new_domexp)) %>%
  mutate(new_needs = needs*1000) %>%
  mutate(ln_newneeds = log(new_needs))
constant <- rep(1, 212)
data1<-cbind(data1,constant)

# World development indicators (World Bank data)
library(wdi)

#gini index analysis
#collect data
gini_data <- WDI(country = "all", indicator = "SI.POV.GINI", start = 2015, end = 2017, extra = TRUE, cache = NULL)

#clean data
gini_data <- gini_data %>%
  rename(countrycode= iso3c,
         gini=SI.POV.GINI)

#create mean variable
gini_data <- gini_data %>%
  group_by(country)%>%
  mutate(gini_mean=mean(gini))

#merge with existing data set
data2<-merge(gini_data, data1, by=c("countrycode"), all=TRUE)

data2 <- data2 %>%
  select(lndomexp_thousand, ln_newdomexp,lnGDP, governmenteffectivenessestimate, 
         average_population_density, agriculturallandoflandarea, 
         average_forestarealandarea, GDP_CO2, CO2_Ems, Threatened_Species, 
         populationgrowthannual, Gov,  oilrentsofgdp,ln_average_co2emmkt, gini)

#regression (stealing Emily's code)
library(sjstats)
library(stargazer)
reg1<- lm(ln_newdomexp~ lnGDP +
            governmenteffectivenessestimate+
            average_population_density +
            agriculturallandoflandarea +
            average_forestarealandarea +
            GDP_CO2+
            CO2_Ems+
            gini,
          na.action=na.exclude, data=data2)
rob1<- robust(reg1)
stargazer(reg1)
stargazer(rob1)
summary(reg1) #Gini index not statistically significant





