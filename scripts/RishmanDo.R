#transfer Rishman .do files to R
rm(list=ls())

library(devtools)
P <- rprojroot::find_rstudio_root_file

library(readxl)
# ************USING Financial_needs_3_SCBD dataset**********************
#   **Table 1**
#   
SCBD <- read_xls("data/financial_needs_data3_SCBD.xls")
SCBD_summ<-summary(SCBD)

# *****USING Financial_needs_3 dataset*****************
#   
#   **Table 2**
#   
FinNeeds <- read_xls("data/financial_needs_data3.xls")
#summary stats for reported needs
FinNeeds<-FinNeeds[order(FinNeeds$type, FinNeeds$countries),]

#data in thousands
require(dplyr)
FinNeeds <- FinNeeds %>%
  mutate(new_domexp = dom_exp*1000) %>%
  mutate(ln_newdomexp = log(new_domexp)) %>%
  mutate(new_needs = needs*1000) %>%
  mutate(ln_newneeds = log(new_needs))

#add constant to data1
constant <- rep(1, 212)
FinNeeds<-cbind(FinNeeds,constant)


library(dplyr)
FinNeedsSum<-FinNeeds %>% 
  summarise_at("newneeds", list(mean, min, max), na.rm = TRUE) 
rownames(FinNeedsSum) = "newneeds"
colnames(FinNeedsSum)<-c("mean","min","max")


# **Table 3**
# correlation table
library(stats)
dataforCor<-FinNeeds %>% 
  select_if(is.numeric, toupper)
cortbl <- cor(dataforCor, use = "complete.obs")

# ** Table 4 ***
## regression based on Anthony's data
modelAnthony <- lm(ln_newdomexp ~ birdspeciesthreatened +
                   mammalspeciesthreatened +
                   ln_landarea +
                   governmenteffectivenessestimate +
                   politicalstabilityandabsenceofvi +
                   Price_Index_yr2011 +
                   terrestrialandmarineprotectedare +
                   lnGDP+
                    GDP_sq, data = FinNeeds, na.action = na.exclude)
summary(modelAnthony)
# r-squared 65.6
# stat significance only on landarea, bird species, and government effectiveness


#checks on the model
model1AIC<-AIC(modelAnthony, k = 2)
model1BIC <- BIC(modelAnthony)

library(car)
model1VIF<-vif(modelAnthony) #the VIF for lnGDP and GDP_sq are VERYYY high. Problematic
sum(model1VIF)

#Robust standard errors
library(sjstats)
rob1<-parameters::standard_error_robust(modelAnthony) #only difference between Rishman's model 1 and 2 is the robust SE

#run with robust SE
library(MASS)
modelAnthony2rob <- rlm(ln_newdomexp ~ birdspeciesthreatened +
                      mammalspeciesthreatened +
                      ln_landarea +
                      governmenteffectivenessestimate +
                      politicalstabilityandabsenceofvi +
                      Price_Index_yr2011 +
                      terrestrialandmarineprotectedare +
                      lnGDP+
                      GDP_sq, data = FinNeeds, na.action = na.exclude)
summary(modelAnthony2rob)
#checks on the model
model2AIC<-AIC(modelAnthony2rob, k = 2)
model2BIC <- BIC(modelAnthony2rob)
model2VIF<-vif(modelAnthony2rob) #the VIF for lnGDP and GDP_sq are VERYYY high. Problematic
sum(model2VIF)

#Anthony model 3 - less variables 
#Robust
modelAnthony3rob<- rlm(ln_newdomexp~ birdspeciesthreatened+
                     mammalspeciesthreatened +
                     ln_landarea+ Price_Index_yr2011 +
                     terrestrialandmarineprotectedare +
                     lnGDP +
                     GDP_sq +
                     Gov, na.action = na.exclude, data = FinNeeds)
#Model fit
modelAnthony3 <- lm(ln_newdomexp~ birdspeciesthreatened+
                      mammalspeciesthreatened +
                      ln_landarea+ Price_Index_yr2011 +
                      terrestrialandmarineprotectedare +
                      lnGDP +
                      GDP_sq +
                      Gov, na.action = na.exclude, data = FinNeeds)
summary(modelAnthony3) #R2 = .666 Gov and ln_landarea significant

#checks on the model
model3AIC<-AIC(modelAnthony3rob, k = 2)
model3BIC <- BIC(modelAnthony3rob)
model3VIF<- vif(modelAnthony3rob) #GDP variables not AS high, but still nearly 500

sum(model3VIF)

#Anthony model 4- minus GDP_sq 
modelAnthony4rob<- rlm(ln_newdomexp ~ birdspeciesthreatened +
                     mammalspeciesthreatened+
                     ln_landarea +
                     Price_Index_yr2011 +
                     terrestrialandmarineprotectedare +
                     lnGDP +
                     Gov, na.action = na.exclude, FinNeeds)
#fit
modelAnthony4<-lm(ln_newdomexp ~ birdspeciesthreatened +
                    mammalspeciesthreatened+
                    ln_landarea +
                    Price_Index_yr2011 +
                    terrestrialandmarineprotectedare +
                    lnGDP +
                    Gov, na.action = na.exclude, FinNeeds)
summary(modelAnthony4) #r2 .658, ln_landarea, lnGDP, Gov significant

#checks on the model
model4AIC<-AIC(modelAnthony4rob, k = 2)
model4BIC <- BIC(modelAnthony4rob)
model4VIF<- vif(modelAnthony4rob) #VIF MUCH better (highest 4.33 - price index)
sum(model4VIF)

#Anthony model 5 (GDP_sq instead of ln_GDP) 
#robust
modelAnthony5rob <- rlm(ln_newdomexp ~ birdspeciesthreatened +
                      mammalspeciesthreatened +
                      ln_landarea +
                      Price_Index_yr2011+
                      terrestrialandmarineprotectedare +
                      GDP_sq +
                      Gov, na.action = na.exclude, FinNeeds)
#fit
modelAnthony5 <- lm(ln_newdomexp ~ birdspeciesthreatened +
                      mammalspeciesthreatened +
                      ln_landarea +
                      Price_Index_yr2011+
                      terrestrialandmarineprotectedare +
                      GDP_sq +
                      Gov, na.action = na.exclude, FinNeeds)
summary(modelAnthony5) #r2 .662, intercept, ln_landarea, GDP_sq, and Gov significant
#checks on the model
model5AIC<-AIC(modelAnthony5rob, k = 2)
model5BIC <- BIC(modelAnthony5rob)
model5VIF<- vif(modelAnthony5rob) #VIF MUCH better, highest (4.37 price index)

sum(model5VIF)

#Anthony model 6, no gov 
modelAnthony6rob <- rlm(ln_newdomexp ~ birdspeciesthreatened +
                      mammalspeciesthreatened +
                      ln_landarea +
                      Price_Index_yr2011+
                      terrestrialandmarineprotectedare +
                      GDP_sq, na.action = na.exclude, FinNeeds)
#fit
modelAnthony6 <- lm(ln_newdomexp ~ birdspeciesthreatened +
                          mammalspeciesthreatened +
                          ln_landarea +
                          Price_Index_yr2011+
                          terrestrialandmarineprotectedare +
                          GDP_sq, na.action = na.exclude, FinNeeds)
summary(modelAnthony6) #r2 .595 Intercept, ln_landarea, PI, GDP_sq significant

#checks on the model
model6AIC<-AIC(modelAnthony6rob, k = 2)
model6BIC <- BIC(modelAnthony6rob)
model6VIF<- vif(modelAnthony6rob) #Lowest VIF scores, but higher AIC
sum(model6VIF)


#estimation from model 5, best fit with low VIF

ln_yhatAnthony <- predict.lm(modelAnthony5rob)
FinNeeds$yhatAnthony<-exp(ln_yhatAnthony)/1E9
extrapExpAnth<- sum(exp(FinNeeds$yhatAnthony[is.na(FinNeeds$yhatAnthony)==FALSE]))/1E9 #43.4 bill

#manual extrapolation

FinNeeds$ln_anthony <-
  modelAnthony5rob$coefficients[[1]]*FinNeeds$constant+
  modelAnthony5rob$coefficients[[2]]*FinNeeds$birdspeciesthreatened+
  modelAnthony5rob$coefficients[[3]]*FinNeeds$mammalspeciesthreatened+
  modelAnthony5rob$coefficients[[4]]*FinNeeds$ln_landarea+
  modelAnthony5rob$coefficients[[5]]*FinNeeds$Price_Index_yr2011+
  modelAnthony5rob$coefficients[[6]]*FinNeeds$terrestrialandmarineprotectedare+
  modelAnthony5rob$coefficients[[7]]*FinNeeds$GDP_sq+
  modelAnthony5rob$coefficients[[8]]*FinNeeds$Gov

FinNeeds$anthonyMan <- exp(FinNeeds$ln_anthony)

FinNeeds$absdiff <- abs(FinNeeds$new_domexp-FinNeeds$anthonyMan)
#take off top two outliers
library(plyr)
N<-2
altered_data<- FinNeeds %>% 
  arrange(desc(absdiff)) %>% 
  tail(-N)
#now run the model again and manually extrapolate again
modelAnthony5rob <- rlm(ln_newdomexp ~ birdspeciesthreatened +
                          mammalspeciesthreatened +
                          ln_landarea +
                          Price_Index_yr2011+
                          terrestrialandmarineprotectedare +
                          GDP_sq +
                          Gov, na.action = na.exclude, altered_data)

FinNeeds$ln_anthony <-
  modelAnthony5rob$coefficients[[1]]*FinNeeds$constant+
  modelAnthony5rob$coefficients[[2]]*FinNeeds$birdspeciesthreatened+
  modelAnthony5rob$coefficients[[3]]*FinNeeds$mammalspeciesthreatened+
  modelAnthony5rob$coefficients[[4]]*FinNeeds$ln_landarea+
  modelAnthony5rob$coefficients[[5]]*FinNeeds$Price_Index_yr2011+
  modelAnthony5rob$coefficients[[6]]*FinNeeds$terrestrialandmarineprotectedare+
  modelAnthony5rob$coefficients[[7]]*FinNeeds$GDP_sq+
  modelAnthony5rob$coefficients[[8]]*FinNeeds$Gov

FinNeeds$anthonyMan <- exp(FinNeeds$ln_anthony)
extraExpAnthMan<- sum(exp(FinNeeds$ln_anthony[is.na(FinNeeds$ln_anthony)==FALSE]))/1E9 #100.79 bill



#Table 5 - extrapolated needs
modelNeedsAnth <- lm(ln_newneeds ~ ln_yhatAnthony, na.action = na.exclude, FinNeeds)
summary(modelNeedsAnth)
modelNeedsAnthMod <- lm(newneeds ~ anthonyMan, na.action = na.exclude, FinNeeds)
summary(modelNeedsAnthMod)


#plot Figure 1
library(ggplot2)

ggplot(altered_data,aes(x=ln_anthony, y=  ln_newdomexp))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE) +
  xlim(10.5, 22.5) +
  ylim(10.5, 22.5) +
  xlab('Predicted Domestic Expenditures in Logs') +
  ylab('Observed Domestic Expenditures in Logs') +
  geom_text(x=15, y=11.5, label="Barbados", color = 'red') +
  geom_text(x=17, y=10.5, label="Guinea", color = 'red') +
  geom_text(x=13, y=12.5, label="Maldives", color = 'red') +
  geom_text(x=21, y=22.5, label="Exact prediction", color = 'blue') +
  theme_classic()

#can't figure out why this is formatting improperly

# Table 6 - 

#fill in extrapolated dom exp where there isn't reported data
FinNeeds<-FinNeeds %>% 
  mutate(ln_ManualExtrap = ifelse(is.na(ln_newdomexp)==TRUE, FinNeeds$ln_anthony, FinNeeds$ln_newdomexp)) %>% 
  mutate(ManualExtrap = exp(ln_ManualExtrap))
#needs model
modelNeeds2 <- rlm(ln_newneeds ~ ln_ManualExtrap, FinNeeds)
summary(modelNeeds2)
#determine needs
ln_needs <-
  modelNeeds2$coefficients[[1]]*FinNeeds$constant+
  modelNeeds2$coefficients[[2]]*FinNeeds$ln_ManualExtrap
sumneeds1 <- sum(exp(ln_needs[is.na(ln_needs)==FALSE]))/1E9 #155.07 billion

#save this needs model
saveRDS(modelNeeds2, "outputs/AnthonyNeeds.RDS")

# table 7
#generate expenditure error
error <- FinNeeds$ln_newdomexp-FinNeeds$ln_anthony

#Table 8 - cor table from above. 

#Table 9 - ALl of my assessments show lower R^2 values than Rishman found

 modelRishmanrob <-rlm(lndomexp_thousand~ birdspeciesthreatened +
                      ln_landarea +
                      Gov+
                      average_population_density+
                      agriculturallandoflandarea+
                      GDP_sq +
                      CO2_Ems +
                      lnGDP +
                      GDP_CO2, na.action = na.exclude, FinNeeds)
 summary(modelRishmanrob)
 #fit test
 modelRishman<-lm(lndomexp_thousand~ birdspeciesthreatened +
                     ln_landarea +
                     Gov+
                     average_population_density+
                     agriculturallandoflandarea+
                     GDP_sq +
                     CO2_Ems +
                     lnGDP +
                     GDP_CO2, na.action = na.exclude, FinNeeds)
summary(modelRishman) #r2 = .682, significant - intercept, bird species, land area, gov, ag land, gdp, ln gdp

modelRAIC<-AIC(modelRishmanrob, k = 2)
modelRBIC <- BIC(modelRishmanrob)
modelRVIF<- vif(modelRishmanrob) #VMajor collinearity between gdp sq, co2, lnGDP and GDP_C02

#model 2 - less gdp c02 and ln gdp
modelR2rob <- rlm(lndomexp_thousand ~birdspeciesthreatened +
                    ln_landarea +
                    Gov +
                    average_population_density +
                    agriculturallandoflandarea +
                    GDP_sq +
                    CO2_Ems, na.action = na.exclude, FinNeeds)
#fit
modelR2 <- lm(lndomexp_thousand ~birdspeciesthreatened +
                ln_landarea +
                Gov +
                average_population_density +
                agriculturallandoflandarea +
                GDP_sq +
                CO2_Ems, na.action = na.exclude, FinNeeds)
summary(modelR2) #R2 .675 Significant - Intercept, bird species, land area, gov, ag land

modelR2AIC<-AIC(modelR2rob, k = 2)
modelR2BIC <- BIC(modelR2rob)
modelR2VIF<- vif(modelR2rob) #Highest VIF 5.2 ln_landarea

#model 3 - 
modelR3rob <- rlm(lndomexp_thousand ~birdspeciesthreatened+
                    ln_landarea +
                    Gov+
                    populationgrowthannual+
                    agriculturallandoflandarea +
                    GDP_sq +
                    ln_average_co2emmkt, na.action = na.exclude, FinNeeds)
modelR3 <- lm(lndomexp_thousand ~birdspeciesthreatened+
                ln_landarea +
                Gov+
                populationgrowthannual+
                agriculturallandoflandarea +
                GDP_sq +
                ln_average_co2emmkt, na.action = na.exclude, FinNeeds)
summary(modelR3) #r2 .6801 significant - Intercept, birds, landarea, gov, ag land

modelR3AIC<-AIC(modelR3rob, k = 2)
modelR3BIC <- BIC(modelR3rob)
modelR3VIF<- vif(modelR3rob) #Highest VIF 15 GDP sq, ln avg co2 14. 

#model 4 - 
modelR4rob <- rlm(lndomexp_thousand~ birdspeciesthreatened +
                   ln_landarea + 
                   Gov +
                   populationgrowthannual+
                   agriculturallandoflandarea +
                   GDP_sq+
                   ln_average_co2emmkt +
                   Price_Index_yr2011, na.action =na.exclude, FinNeeds )
#fit

modelR4 <- lm(lndomexp_thousand~ birdspeciesthreatened +
                ln_landarea + 
                Gov +
                populationgrowthannual+
                agriculturallandoflandarea +
                GDP_sq+
                ln_average_co2emmkt +
                Price_Index_yr2011, na.action =na.exclude, FinNeeds )
summary(modelR4) #r2 .681 significant Intercept, land area, Gov

modelR4AIC<-AIC(modelR4rob, k = 2)
modelR4BIC <- BIC(modelR4rob)
modelR4VIF<- vif(modelR4rob) #VIF 24 - GDP sq 18 ln avg co2. could be problematic

#use model 2 to extrapolate expenditures. 

yhatRishman <- predict.lm(modelR2rob)
FinNeeds$rishmanexp <- exp(yhatRishman)
sumExtrapRishman <- sum(FinNeeds$rishmanexp[is.na(FinNeeds$rishmanexp) == FALSE])/1E9 #37.8 billion

#manual extrapolation

FinNeeds$ln_rishman <-
  modelR2rob$coefficients[[1]]*FinNeeds$constant+
  modelR2rob$coefficients[[2]]*FinNeeds$birdspeciesthreatened+
  modelR2rob$coefficients[[3]]*FinNeeds$ln_landarea+
  modelR2rob$coefficients[[4]]*FinNeeds$Gov+
  modelR2rob$coefficients[[5]]*FinNeeds$average_population_density+
  modelR2rob$coefficients[[6]]*FinNeeds$agriculturallandoflandarea+
  modelR2rob$coefficients[[7]]*FinNeeds$GDP_sq+
  modelR2rob$coefficients[[8]]*FinNeeds$CO2_Ems

manualRishman <- exp(FinNeeds$ln_rishman)  
summanualrish <- sum(manualRishman[is.na(manualRishman)==FALSE])/1E9 #135.93 billion

#plot it
this <- data.frame(1:71) %>% 
  mutate(fittedvalues = modelR2rob$fitted.values) %>% 
  mutate(residuals = modelR2rob$residuals)
ggplot(this, aes(fittedvalues, residuals))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

# Drop the outliers


FinNeeds$RishAbsDiff <- abs(FinNeeds$new_domexp - FinNeeds$rishmanexp)
#drop top two outliers
N<-2
altered_dataR<- FinNeeds %>%
  arrange(desc(RishAbsDiff)) %>%
  tail(-N)

#rerun model

modelR2rob2 <- rlm(lndomexp_thousand ~birdspeciesthreatened +
                     ln_landarea +
                     Gov +
                     average_population_density +
                     agriculturallandoflandarea +
                     GDP_sq +
                     CO2_Ems, na.action = na.exclude, altered_dataR)
modelR22 <- lm(lndomexp_thousand ~birdspeciesthreatened +
                 ln_landarea +
                 Gov +
                 average_population_density +
                 agriculturallandoflandarea +
                 GDP_sq +
                 CO2_Ems, na.action = na.exclude, altered_dataR)
summary(modelR22) #r2 .6595 - not as good of fit
#manually extract
FinNeeds$ln_rishman2 <-
  modelR2rob2$coefficients[[1]]*FinNeeds$constant+
  modelR2rob2$coefficients[[2]]*FinNeeds$birdspeciesthreatened+
  modelR2rob2$coefficients[[3]]*FinNeeds$ln_landarea+
  modelR2rob2$coefficients[[4]]*FinNeeds$Gov+
  modelR2rob2$coefficients[[5]]*FinNeeds$average_population_density+
  modelR2rob2$coefficients[[6]]*FinNeeds$agriculturallandoflandarea+
  modelR2rob2$coefficients[[7]]*FinNeeds$GDP_sq+
  modelR2rob2$coefficients[[8]]*FinNeeds$CO2_Ems
ExtrapExp <- exp(FinNeeds$ln_rishman2)
summodel2<- sum(ExtrapExp[is.na(ExtrapExp)==FALSE])/1E9 #109.73 billion

#error
errorRish <- FinNeeds$newneeds - ExtrapExp




#Table 10
FinNeeds<-FinNeeds %>% 
  mutate(ln_ManualExtrapRishman = ifelse(is.na(ln_newdomexp)==TRUE, FinNeeds$ln_rishman2, FinNeeds$ln_newdomexp)) %>% 
  mutate(ManualExtrapRishman = exp(ln_ManualExtrap))

modelNeedsRish <- lm(ln_newneeds ~ ln_ManualExtrapRishman + 
                       mammalspeciesthreatened + 
                       oilrentsofgdp +
                       terrestrialprotectedareasoftotal, na.action = na.exclude, FinNeeds)
summary(modelNeedsRish) #R2 .403

#manually extract needs
ln_needsRish <- 
  modelNeedsRish$coefficients[[1]]*FinNeeds$constant+
  modelNeedsRish$coefficients[[2]]*FinNeeds$ln_rishman+
  modelNeedsRish$coefficients[[3]]*FinNeeds$mammalspeciesthreatened+
  modelNeedsRish$coefficients[[4]]*FinNeeds$oilrentsofgdp+
  modelNeedsRish$coefficients[[5]]*FinNeeds$terrestrialprotectedareasoftotal

sumneedsRish<- sum(exp(ln_needsRish[is.na(ln_needsRish)==FALSE]))/1E9 #177.28 billion (180 countries)


#Based on Rishman's Analysis, we will use Anthony's model 5 and Rishman model 2.
#save the model with top two outliers removed
saveRDS(modelR22, "outputs/RishmanModel2.RDS")
saveRDS(modelAnthony5rob, "outputs/WaldronModel5.RDS")
saveRDS(modelNeedsRish, "outputs/RishmanNeeds.RDS")

#Try to find a model where ag, GDP and CO2 are significant
#make logs of co2 and ag
FinNeeds <- FinNeeds %>% 
  mutate(ln_agland = log(agriculturallandoflandarea)) %>% 
  mutate(ln_CO2ems = log(CO2_Ems)) %>% 
  mutate(ln_popdnsty = log(average_population_density)) %>% 
  mutate(ln_GDP2010 = log(gdpconstant2010us))

# EmModel <- lm(ln_newdomexp ~ GDP_sq + 
#                  Gov + 
#                  ln_CO2ems + 
#                  ln_agland + 
#                  birdspeciesthreatened+
#                  ln_popdnsty+
#                 ln_landarea,
#               FinNeeds, na.action = na.exclude)
# 
# summary(EmModel)
# #model tests
# modelEmAIC<-AIC(EmModel, k = 2)
# modelEmBIC <- BIC(EmModel)
# modelEmVIF<- vif(EmModel) #no autocorrelation
# 
# ln_emexp <-
#   EmModel$coefficients[[1]]*FinNeeds$constant+
#   EmModel$coefficients[[2]]*FinNeeds$ln_CO2ems+
#   EmModel$coefficients[[3]]*FinNeeds$ln_agland+
#   EmModel$coefficients[[4]]*FinNeeds$birdspeciesthreatened+
#   EmModel$coefficients[[5]]*FinNeeds$ln_popdnsty
# EmExp <- exp(ln_emexp)
# 
# # Drop the outliers
# 
# FinNeeds$EmAbsDiff <- abs(FinNeeds$new_domexp - EmExp)
# #drop top two outliers
# N<-2
# altered_dataE<- FinNeeds %>%
#   arrange(desc(EmAbsDiff)) %>%
#   tail(-N)
# 
# EmModel2 <- lm(ln_newdomexp ~ lnGDP + 
#                 Gov + 
#                 ln_CO2ems + 
#                 ln_agland + 
#                 birdspeciesthreatened+
#                 ln_popdnsty,
#               altered_dataE, na.action = na.exclude)
# 
# summary(EmModel2)
# 
# ln_emexp2 <-
#   EmModel2$coefficients[[1]]*FinNeeds$constant+
#   EmModel2$coefficients[[2]]*FinNeeds$ln_CO2ems+
#   EmModel2$coefficients[[3]]*FinNeeds$ln_agland+
#   EmModel2$coefficients[[4]]*FinNeeds$birdspeciesthreatened+
#   EmModel2$coefficients[[5]]*FinNeeds$ln_popdnsty
# EmExpSum <- sum(exp(ln_emexp2[is.na(ln_emexp2)==FALSE])) #111.124
# #save this model, too
# saveRDS(EmModel2, "outputs/EmilyModel.RDS")
# 


#try with change in co2 levels instead
library(WDI)
co2.reduction.rate <- WDI(indicator = "EN.ATM.CO2E.KT", start = 2004, end = 2014, extra = TRUE)
  dplyr::select(iso3c, EN.ATM.CO2E.KT, year) %>% 
  dplyr::mutate(countrycode = as.character(iso3c)) %>% 
  dplyr::rename('co2emissions' = EN.ATM.CO2E.KT) %>% 
  dplyr::group_by(countrycode) %>% 
  dplyr::arrange(year, .by_group = TRUE) %>% 
  dplyr::mutate(lag = lag(co2emissions)) %>% 
  dplyr::mutate(pct_change = (co2emissions - lag(co2emissions))/lag(co2emissions)*100) %>% 
  dplyr::summarise(AvgCO2ReductionPercent = -mean(pct_change, na.rm = TRUE))
#add to FinNeeds
  #world bank data not downloading now
FinNeeds<-readRDS("outputs/FinancialNeedsDataFromRishman.RDS")


#FinNeeds <- inner_join(FinNeeds, co2.reduction.rate, by = "countrycode") 
FinNeeds$ln_CO2reduct <- log(FinNeeds$AvgCO2ReductionPercent)

#now run model with c02 reduction percent instead of levels

EmModelC <- lm(ln_newdomexp ~ lnGDP+
                 GDP_sq+ 
                Gov + 
                AvgCO2ReductionPercent+ 
                 agriculturallandoflandarea + 
                birdspeciesthreatened+
                 mammalspeciesthreatened+
                average_population_density,
              FinNeeds, na.action = na.exclude)
summary(EmModelC)

modelEmAIC<-AIC(EmModelC, k = 2)
modelEmBIC <- BIC(EmModelC)
modelEmVIF<- vif(EmModelC) #no major autocorrelation

ln_emexp <-
  EmModelC$coefficients[[1]]*FinNeeds$constant+
  EmModelC$coefficients[[2]]*FinNeeds$lnGDP+
  EmModelC$coefficients[[3]]*FinNeeds$GDP_sq+
  EmModelC$coefficients[[4]]*FinNeeds$Gov+
  EmModelC$coefficients[[5]]*FinNeeds$AvgCO2ReductionPercent+
  EmModelC$coefficients[[6]]*FinNeeds$agriculturallandoflandarea+
  EmModelC$coefficients[[7]]*FinNeeds$birdspeciesthreatened+
  EmModelC$coefficients[[8]]*FinNeeds$average_population_density
EmExp <- exp(ln_emexp)


# Drop the outliers

FinNeeds$EmAbsDiff <- abs(FinNeeds$new_domexp - EmExp)
#drop top two outliers
N<-2
altered_dataE<- FinNeeds %>%
  arrange(desc(EmAbsDiff)) %>%
  tail(-N)

EmModel2 <- lm(ln_newdomexp ~ 
                 lnGDP+
                 GDP_sq+ 
                 Gov + 
                 AvgCO2ReductionPercent+ 
                 agriculturallandoflandarea + 
                 birdspeciesthreatened+
                 average_population_density,
               FinNeeds, na.action = na.exclude)
summary(EmModel2)

ln_emexp2 <-
  EmModel2$coefficients[[1]]*FinNeeds$constant+
  EmModel2$coefficients[[2]]*FinNeeds$lnGDP+
  EmModel2$coefficients[[3]]*FinNeeds$GDP_sq+
  EmModel2$coefficients[[4]]*FinNeeds$Gov+
  EmModel2$coefficients[[5]]*FinNeeds$AvgCO2ReductionPercent+
  EmModel2$coefficients[[6]]*FinNeeds$agriculturallandoflandarea+
  EmModel2$coefficients[[7]]*FinNeeds$birdspeciesthreatened+
  EmModel2$coefficients[[8]]*FinNeeds$average_population_density
FinNeeds$EmExtrapExp <- exp(ln_emexp2)
EmExpSum <- sum(exp(ln_emexp2), na.rm = TRUE)/1E9 #186.36 bil
#save this model, and the df
saveRDS(EmModel2, "outputs/EmilyModel.RDS")
saveRDS(FinNeeds, "outputs/FinancialNeedsDataFromRishman.RDS")
