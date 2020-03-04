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
         countrycode,
         constant) %>% 
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
         GDP,
         birdspeciesthreatened,
         ln_landarea,
         Gov,
         average_population_density,
         mammalspeciesthreatened,
         Price_Index_yr2011,
         terrestrialandmarineprotectedare,
         ln_popdnsty,constant)
 BAUData<-BAUData %>% 
   mutate(GDP=ifelse(is.na(BAUData$GDP),exp(subdata$lnGDP),BAUData$GDP)) %>% 
  mutate(AverageGDPrate = AverageGDPrate/100) %>% 
   mutate(GDPmultiplier = 1+AverageGDPrate) %>% 
   mutate("2030GDP" = GDP*(GDPmultiplier^(12)))
 #get rid of unnecessary GDP data
BAUData$GDPmultiplier = NULL
BAUData$GDP =NULL
BAUData$AverageGDPrate = NULL
 #save GDP predictions
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

 
 #do the same for CO2
 
 #change in CO2
 co2.growth.rate <- WDI(indicator = 'EN.ATM.CO2E.KT', start = 2004, end = 2014, extra = TRUE) %>%
   select(iso3c, EN.ATM.CO2E.KT, year) %>% 
   mutate(countrycode = as.character(iso3c)) %>% 
   rename('co2emissions' = EN.ATM.CO2E.KT) %>% 
   group_by(countrycode) %>% 
   arrange(year, .by_group = TRUE) %>% 
   mutate(lag = lag(co2emissions)) %>% 
   mutate(pct_change = (co2emissions - lag(co2emissions))/lag(co2emissions)*100) %>% 
   summarise(AvgCO2Growth = mean(pct_change, na.rm = TRUE))
 
 #current co2 levels - 2014 is the latest data
 co2.emissions.levels <- WDI(indicator = 'EN.ATM.CO2E.KT', start = 2014, end = 2014, extra = TRUE) %>% select(iso3c, EN.ATM.CO2E.KT) %>% 
   mutate(countrycode = as.character(iso3c)) %>% 
   rename("co2emissions" = EN.ATM.CO2E.KT)

 #merge the two and use growth rate to find 2030 levels
 
 co2.future.levels <- left_join(co2.growth.rate, co2.emissions.levels, by = "countrycode") %>% 
   mutate(growthmultiplier = (AvgCO2Growth)/100 + 1) %>% 
   mutate(futureco2level = co2emissions*(growthmultiplier^16)) %>% 
   arrange(countrycode) %>% 
   select(countrycode, futureco2level)

#merge with business as usual df
 
 BAUData <- left_join(BAUData, co2.future.levels, by = "countrycode")
 
 BAUData <- BAUData %>%  
   mutate(ln_futureGDP = log(`2030GDP`)) %>% 
   mutate(ln_futureAgLand = log(futureagland)) %>% 
   mutate(ln_futureco2 = log(futureco2level))

 
#find extrapolated expenditures using the 3 models. 
 
 ln_waldronexp <-
   WaldronModel$coefficients[[1]]*BAUData$constant+
   WaldronModel$coefficients[[2]]*BAUData$birdspeciesthreatened+
   WaldronModel$coefficients[[3]]*BAUData$mammalspeciesthreatened+
   WaldronModel$coefficients[[4]]*BAUData$ln_landarea+
   WaldronModel$coefficients[[5]]*BAUData$terrestrialandmarineprotectedare
 
  