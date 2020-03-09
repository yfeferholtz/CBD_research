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
detach("package:plyr", unload=TRUE)
#Can't use select function if MASS is required.
subdata <- FullData %>% 
  dplyr::select(countries,birdspeciesthreatened,
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
         constant,
         AvgCO2ReductionPercent,
         new_domexp) %>% 
  arrange(countrycode)


#get GDP growth rates for all countries
library(WDI)
gdp.gr.rate.per.year = WDI(indicator='NY.GDP.MKTP.KD.ZG', country = "all", start=2008, end=2018, extra = TRUE)
gdp.gr.rate<- gdp.gr.rate.per.year %>% 
  dplyr::select(iso3c, NY.GDP.MKTP.KD.ZG) %>% 
  dplyr::mutate(countrycode = as.character(iso3c)) %>% 
  dplyr::rename("GDPgrowth" = NY.GDP.MKTP.KD.ZG) %>% 
  dplyr::group_by(countrycode) %>% 
  dplyr::summarise(AverageGDPrate = mean(GDPgrowth, na.rm = TRUE)) %>%
  dplyr::arrange(countrycode)

gdp.gr.rate$AverageGDPrate<-ifelse(gdp.gr.rate$AverageGDPrate<0, 0, gdp.gr.rate$AverageGDPrate)


#Get 2018 GDP
gdp.per.country <- WDI(indicator = 'NY.GDP.MKTP.CD', start = 2018, end = 2018, extra = TRUE) %>% 
  dplyr::mutate(countrycode = as.character(iso3c)) %>% 
  dplyr::rename("GDP" = NY.GDP.MKTP.CD) %>% 
  dplyr::select(countrycode, GDP) %>% 
  dplyr::arrange(countrycode)
  
#combine data 
#gdp data
gdp.data <- inner_join(gdp.per.country, gdp.gr.rate, by = "countrycode")
#put gdp data into og data
Mergedata<-left_join(subdata, gdp.data, by = "countrycode")

#write data to folder for other scenarios
write.csv(Mergedata, P('data/scenario_data.csv')) 

#Determine 2030 GDP for each country
BAUData <- Mergedata %>% 
  dplyr::select(countries,
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
         ln_popdnsty,constant,
         AvgCO2ReductionPercent, new_domexp)
 
BAUData<-BAUData %>% 
   mutate(GDP=ifelse(is.na(BAUData$GDP),exp(subdata$lnGDP),BAUData$GDP)) %>% 
   mutate(AverageGDPrate = AverageGDPrate/100) %>% 
   mutate(GDPmultiplier = 1+AverageGDPrate) %>% 
   mutate("2030GDP" = GDP*(GDPmultiplier^(12)))

#get rid of unnecessary GDP data
#BAUData$GDPmultiplier = NULL
#BAUData$GDP =NULL
#BAUData$AverageGDPrate = NULL
 #save GDP predictions
saveRDS(BAUData, "outputs/2030GDPprojectionsUSD.RDS")

 #Now find ag land growth
 
 #find growth rate of ag land for 2006 to 2016 by getting the percentage of ag land by land area for each year and calculating growth rate for each year. Then take the average.
ag.land.growth.rate <- WDI(indicator = 'AG.LND.AGRI.ZS', start=2006, end = 2016, extra = TRUE) %>% 
   dplyr::select(iso3c, AG.LND.AGRI.ZS, year) %>% 
   mutate(countrycode = as.character(iso3c)) %>% 
   rename("aglandpercent" = AG.LND.AGRI.ZS) %>% 
   group_by(countrycode) %>% 
   arrange(year, .by_group = TRUE) %>% 
   mutate(lag = lag(aglandpercent)) %>% 
   mutate(pct_change = (aglandpercent - lag(aglandpercent))/lag(aglandpercent)*100) %>% 
   summarise(AvgAgGrowth = mean(pct_change, na.rm = TRUE))

# download current ag land percent of land - most current year is 2016
 ag.land.current <- WDI(indicator = 'AG.LND.AGRI.ZS', start=2016, end = 2016, extra = TRUE) %>% 
   dplyr::select(iso3c, AG.LND.AGRI.ZS) %>% 
   mutate(countrycode = as.character(iso3c)) %>% 
   rename("aglandpercent" = AG.LND.AGRI.ZS)
 
 scendata <- left_join(ag.land.current, ag.land.growth.rate, by = "countrycode") 

# merge the two and calculate 2030 expected percentage of ag by land area using the growth rate, 
 ag.land.future <- left_join(ag.land.growth.rate,ag.land.current,by = "countrycode") %>% 
   mutate(growthmultiplier_agland = (AvgAgGrowth)/100+1) %>% 
   mutate(futureagland = aglandpercent*(growthmultiplier_agland^(14))) %>% 
   dplyr::select(countrycode, futureagland, growthmultiplier_agland) %>% 
   arrange(countrycode)
 
 ag.land.data <- inner_join(ag.land.current, ag.land.future, by = "countrycode")
 #insert this into the new dataframe
 
BAUData <- left_join(BAUData, ag.land.data, by = "countrycode")

 
 #do the same for CO2
 
 #change in CO2
 co2.growth.rate <- WDI(indicator = 'EN.ATM.CO2E.KT', start = 2004, end = 2014, extra = TRUE) %>%
   dplyr::select(iso3c, EN.ATM.CO2E.KT, year) %>%
   mutate(countrycode = as.character(iso3c)) %>%
   rename('co2emissions' = EN.ATM.CO2E.KT) %>%
   group_by(countrycode) %>%
   arrange(year, .by_group = TRUE) %>%
   mutate(lag = lag(co2emissions)) %>%
   mutate(pct_change = (co2emissions - lag(co2emissions))/lag(co2emissions)*100) %>%
   summarise(AvgCO2Growth = mean(pct_change, na.rm = TRUE))

 #current co2 levels - 2014 is the latest data
co2.emissions.levels <- WDI(indicator = 'EN.ATM.CO2E.KT', start = 2014, end = 2014, extra = TRUE) %>%
   dplyr::select(iso3c, EN.ATM.CO2E.KT) %>%
   mutate(countrycode = as.character(iso3c)) %>%
   rename("co2emissions" = EN.ATM.CO2E.KT)
#save co2 and ag data for other scenarios
co2data<- left_join(co2.emissions.levels, co2.growth.rate, by = "countrycode")
scendata<- left_join(scendata, co2data, by = "countrycode") %>% 
  write.csv("data/scen_data.csv")
# 
#  #merge the two and use growth rate to find 2030 levels
#  
#  co2.future.levels <- left_join(co2.growth.rate, co2.emissions.levels, by = "countrycode") %>% 
#    mutate(growthmultiplier_co2ems = (AvgCO2Growth)/100 + 1) %>% 
#    mutate(futureco2level = co2emissions*(growthmultiplier_co2ems^16)) %>% 
#    arrange(countrycode) %>% 
#    dplyr::select(countrycode, futureco2level, growthmultiplier_co2ems )
#  
#  
# co2.data <- inner_join(co2.emissions.levels, co2.future.levels, by = "countrycode")
 #need to find GDP PPP for 2030 to get Rishman's CO2_EMS value
 
# gdp.ppp.rate <- WDI(indicator = "NY.GDP.MKTP.PP.CD", start = 2008, end = 2018, extra = TRUE) %>%
#    dplyr::select(iso3c, NY.GDP.MKTP.PP.CD, year) %>% 
#    mutate(countrycode = as.character(iso3c)) %>% 
#    rename('ppp' = NY.GDP.MKTP.PP.CD) %>% 
#    group_by(countrycode) %>% 
#    arrange(year, .by_group = TRUE) %>% 
#    mutate(lag = lag(ppp)) %>% 
#    mutate(pct_change = (ppp- lag(ppp))/lag(ppp)*100) %>% 
#    summarise(ppprate = mean(pct_change, na.rm = TRUE))
#  
#  current.gdp.ppp <- WDI(indicator = "NY.GDP.MKTP.PP.CD", start = 2018, end = 2018, extra = TRUE) %>%
#    dplyr::select(iso3c, NY.GDP.MKTP.PP.CD) %>% 
#    mutate(countrycode = as.character(iso3c)) %>% 
#    rename("pppGDP" = NY.GDP.MKTP.PP.CD)
#  
#  ppp.future.levels = left_join(current.gdp.ppp,gdp.ppp.rate, by = "countrycode") %>% 
#    mutate(growthmultiplier_ppplevels = (ppprate)/100 + 1) %>% 
#    mutate(futurePPP = pppGDP*(growthmultiplier_ppplevels^16)) %>% 
#    arrange(countrycode) %>% 
#    dplyr::select(countrycode, futurePPP, growthmultiplier_ppplevels)



# do co2ppp because that's what Rishman uses
co2.ppp.gr.rate<-WDI(indicator ="EN.ATM.CO2E.PP.GD", start = 2004, end = 2014, extra = TRUE) %>% 
  dplyr::select(year, iso3c, EN.ATM.CO2E.PP.GD) %>% 
  mutate(countrycode = as.character(iso3c)) %>% 
  rename('co2ppp' = EN.ATM.CO2E.PP.GD) %>% 
  group_by(countrycode) %>% 
  arrange(year, .by_group = TRUE) %>% 
  mutate(lag = lag(co2ppp)) %>% 
  mutate(pct_change = (co2ppp - lag(co2ppp))/lag(co2ppp)*100) %>% 
  summarise(AvgCO2Growthppp = mean(pct_change, na.rm = TRUE)) 
  
#current (2014) levels
#current co2 levels - 2014 is the latest data
co2.emissions.levels.ppp <- WDI(indicator = 'EN.ATM.CO2E.PP.GD', start = 2014, end = 2014, extra = TRUE) %>%
  dplyr::select(iso3c, EN.ATM.CO2E.PP.GD) %>%
  mutate(countrycode = as.character(iso3c)) %>%
  rename("co2ppp" = EN.ATM.CO2E.PP.GD)

co2.ppp.future.levels <- left_join(co2.ppp.gr.rate, co2.emissions.levels.ppp, by = "countrycode") %>% 
     mutate(growthmultiplier_co2ems = (AvgCO2Growthppp)/100 + 1) %>%
     mutate(futureco2levelppp = co2ppp*(growthmultiplier_co2ems^16)) %>%
     arrange(countrycode) %>%
     dplyr::select(countrycode, futureco2levelppp, growthmultiplier_co2ems )
 
 #combine PPP and CO2
 # CO2ems<- left_join(co2.data, ppp.future.levels, by = "countrycode") %>% 
 #   mutate(futureCO2_EMS = futureco2level/futurePPP)

#merge with business as usual df
BAUData <- left_join(BAUData, co2.ppp.future.levels, by = "countrycode")
 
BAUData <- BAUData %>%  
   mutate(ln_futureGDP = log(`2030GDP`)) %>% 
   mutate(ln_futureAgLand = log(futureagland)) %>% 
   mutate(ln_futureco2ppp = log(futureco2levelppp)) %>% 
   mutate(futureGDP_sq = ln_futureGDP^2)

write.csv(BAUData, P('outputs/BAUdata.csv')) 
 
#find extrapolated expenditures using the 3 models. 
 
 #should I find the 2030 levels of population density, too?
 
 #Waldron Model
 ln_waldronexp <-
   WaldronModel$coefficients[[1]]*BAUData$constant+
   WaldronModel$coefficients[[2]]*BAUData$birdspeciesthreatened+
   WaldronModel$coefficients[[3]]*BAUData$mammalspeciesthreatened+
   WaldronModel$coefficients[[4]]*BAUData$ln_landarea+
   WaldronModel$coefficients[[5]]*BAUData$Price_Index_yr2011+
   WaldronModel$coefficients[[6]]*BAUData$terrestrialandmarineprotectedare+
   WaldronModel$coefficients[[7]]*BAUData$futureGDP_sq+
   WaldronModel$coefficients[[8]]*BAUData$Gov
BAUData$ExpWaldron = exp(ln_waldronexp) 
#total sum of expenditures
WaldronSum <- sum(BAUData$ExpWaldron, na.rm = TRUE)/1E9 #120.62 bil

#Rishman Model

ln_rishman <-
  RishmanModel$coefficients[[1]]*BAUData$constant+
  RishmanModel$coefficients[[2]]*BAUData$birdspeciesthreatened+
  RishmanModel$coefficients[[3]]*BAUData$ln_landarea+
  RishmanModel$coefficients[[4]]*BAUData$Gov+
  RishmanModel$coefficients[[5]]*BAUData$average_population_density+
  RishmanModel$coefficients[[6]]*BAUData$futureagland+
  RishmanModel$coefficients[[7]]*BAUData$futureGDP_sq+
  RishmanModel$coefficients[[8]]*BAUData$futureco2levelppp
BAUData$ExpRishman = exp(ln_rishman)  
#total sum of expenditures
RishmanSum <- sum(BAUData$ExpRishman, na.rm = TRUE)/1E9 #125.56 bil

#Wise model

ln_wise <-
  WiseModel$coefficients[[1]]*BAUData$constant+
  WiseModel$coefficients[[2]]*BAUData$ln_futureGDP+
  WiseModel$coefficients[[3]]*BAUData$futureGDP_sq+
  WiseModel$coefficients[[4]]*BAUData$Gov+
  WiseModel$coefficients[[5]]*BAUData$AvgCO2ReductionPercent+
  WiseModel$coefficients[[6]]*BAUData$futureagland+
  WiseModel$coefficients[[7]]*BAUData$birdspeciesthreatened +
  WiseModel$coefficients[[8]]*BAUData$average_population_density
BAUData$ExpWise <- exp(ln_wise)
#total sum of expenditures

WiseSum <- sum(BAUData$ExpWise, na.rm = TRUE)/1E9 #429.82bil


#fill in extrapolated dom exp where there isn't reported data
BAUData$Waldron_Manual_Exp <- ifelse(is.na(BAUData$new_domexp)==TRUE, BAUData$ExpWaldron, BAUData$new_domexp)
BAUData$Rishman_Manual_Exp <- ifelse(is.na(BAUData$new_domexp)==TRUE, BAUData$ExpRishman, BAUData$new_domexp)
BAUData$Wise_Manual_Exp <- ifelse(is.na(BAUData$new_domexp)==TRUE, BAUData$ExpWise, BAUData$new_domexp)


write.csv(BAUData, "outputs/BAUdata.csv")
#now find needs
