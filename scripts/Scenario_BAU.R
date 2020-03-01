# business as usual model
rm(list=ls())
library(devtools)
P <- rprojroot::find_rstudio_root_file


WaldronModel <- readRDS(P("outputs/WaldronModel5.RDS"))
RishmanModel <- readRDS(P("outputs/RishmanModel2.RDS"))
WiseModel <- readRDS(P("outputs/EmilyModel.RDS"))

#read in original data
FullData <- readRDS(P("outputs/FinancialNeedsDataFromRishman.RDS"))

#create subdata.frame with only attributes used
library(dplyr)
#Can't use select function if MASS is required.
subdata <- FullData %>% 
  select(countries,birdspeciesthreatened,
         ln_landarea,
         Gov,
         average_population_density,
         agriculturallandoflandarea,
         GDP_sq,
         CO2_Ems,
         mammalspeciesthreatened,
         Price_Index_yr2011,
         terrestrialandmarineprotectedare,
         lnGDP,
         ln_CO2ems,
         ln_agland,
         ln_popdnsty,
         countrycode) %>% 
  arrange(countrycode)


#get GDP growth rates for all countries
library(WDI)
gdp.gr.rate.per.year = WDI(indicator='NY.GDP.MKTP.KD.ZG', country = "all", start=2008, end=2018, extra = TRUE)
gdp.gr.rate<- gdp.gr.rate.per.year %>% 
  dplyr::select(iso3c, NY.GDP.MKTP.KD.ZG) %>% 
  mutate(countrycode = as.character(iso3c)) %>% 
  rename("GDPgrowth" = NY.GDP.MKTP.KD.ZG) %>% 
  group_by(countrycode) %>% 
  summarise(AverageGDPrate = mean(GDPgrowth, na.rm = TRUE)) %>% 
  arrange(countrycode)


#Get 2018 GDP
gdp.per.country <- WDI(indicator = 'NY.GDP.MKTP.CD', start = 2018, end = 2018, extra = TRUE) %>% 
  mutate(countrycode = as.character(iso3c)) %>% 
  rename("GDP" = NY.GDP.MKTP.CD) %>% 
  select(countrycode, GDP) %>% 
  arrange(countrycode)
  
#combine data 
#gdp data
gdp.data <- inner_join(gdp.per.country, gdp.gr.rate, by = "countrycode")
#put gdp data into og data
Mergedata<-left_join(subdata, gdp.data, by = "countrycode")

#Determine 2030 GDP for each country
 BAUData <- Mergedata %>% 
  select(countries,
         countrycode,
         AverageGDPrate,
         GDP)
 BAUData<-BAUData %>% 
   mutate(GDP=ifelse(is.na(BAUData$GDP),exp(subdata$lnGDP),BAUData$GDP)) %>% 
  mutate(AverageGDPrate = AverageGDPrate/100) %>% 
   mutate(GDPmultiplier = 1+AverageGDPrate) %>% 
   mutate("2030GDP" = GDP*(GDPmultiplier^(12))) %>% 
   select(countries,countrycode,"2030GDP")
 
 saveRDS(BAUData, "outputs/2030GDPprojectionsUSD.RDS")

 #Now find ag land growth
 
 #find growth rate of ag land for 2006 to 2016 by getting the percentage of ag land by land area for each year and calculating growth rate for each year. Then take the average.
 ag.land.growth.rate <- WDI(indicator = 'AG.LND.AGRI.ZS', start=2006, end = 2016, extra = TRUE) %>% 
   select(iso3c, AG.LND.AGRI.ZS, year) %>% 
   mutate(countrycode = as.character(iso3c)) %>% 
   rename("aglandpercent" = AG.LND.AGRI.ZS) %>% 
   group_by(countrycode) %>% 
   arrange(year, .by_group = TRUE) %>% 
   mutate(lag = lag(aglandpercent)) %>% 
   mutate(pct_change = (aglandpercent - lag(aglandpercent))/lag(aglandpercent)*100) %>% 
   summarise(AvgAgGrowth = mean(pct_change, na.rm = TRUE))

 #download current ag land percent of land - most current year is 2016
 ag.land.current <- WDI(indicator = 'AG.LND.AGRI.ZS', start=2016, end = 2016, extra = TRUE) %>% 
   select(iso3c, AG.LND.AGRI.ZS) %>% 
   mutate(countrycode = as.character(iso3c)) %>% 
   rename("aglandpercent" = AG.LND.AGRI.ZS)
 #merge the two and calculate 2030 expected percentage of ag by land area using the growth rate, 
 ag.land.future <- left_join(ag.land.growth.rate,ag.land.current,by = "countrycode") %>% 
   mutate(growthmultiplier = (AvgAgGrowth)/100+1) %>% 
   mutate(futureagland = aglandpercent*(growthmultiplier^(14))) %>% 
   select(countrycode, futureagland) %>% 
   arrange(countrycode)
 
 #insert this into the new dataframe
 BAUData <- left_join(BAUData, ag.land.future, by = "countrycode")
