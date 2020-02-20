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
modelAnthony <- lm(lndomexp_thousand ~ birdspeciesthreatened +
                   mammalspeciesthreatened +
                   ln_landarea +
                   governmenteffectivenessestimate +
                   politicalstabilityandabsenceofvi +
                   Price_Index_yr2011 +
                   terrestrialandmarineprotectedare +
                   lnGDP+
                    GDP_sq, data = FinNeeds, na.action = na.exclude)
tab<-summary(modelAnthony)
# r-squared 65.6
# stat significance only on landarea, bird species, and government effectiveness

#checks on the model
model1AIC<-AIC(modelAnthony, k = 2)
model1BIC <- BIC(modelAnthony)

library(car)
model1VIF<-vif(modelAnthony) #the VIF for lnGDP and GDP_sq are VERYYY high. Problematic

#Robust standard errors
library(sjstats)
rob1<-parameters::standard_error_robust(modelAnthony) #only difference between Rishman's model 1 and 2 is the robust SE

#Anthony model 2 - less variables (rishman model 3)
modelAnthony2<- lm(lndomexp_thousand~ birdspeciesthreatened+
                     mammalspeciesthreatened +
                     ln_landarea+ Price_Index_yr2011 +
                     terrestrialandmarineprotectedare +
                     lnGDP +
                     GDP_sq +
                     Gov, na.action = na.exclude, data = FinNeeds)
summary(modelAnthony2) #R2 = .666 Gov and ln_landarea significant

#checks on the model
model2AIC<-AIC(modelAnthony2, k = 2)
model2BIC <- BIC(modelAnthony2)
model2VIF<- vif(modelAnthony2) #GDP variables not AS high, but still nearly 500
#robust SE
library(parameters)
rob2<-standard_error_robust(modelAnthony2)


#Anthony model 3 - minus GDP_sq (Rishman model 4)
modelAnthony3<- lm(lndomexp_thousand ~ birdspeciesthreatened +
                     mammalspeciesthreatened+
                     ln_landarea +
                     Price_Index_yr2011 +
                     terrestrialandmarineprotectedare +
                     lnGDP +
                     Gov, na.action = na.exclude, FinNeeds)
summary(modelAnthony3) #r2 .658, ln_landarea, lnGDP, Gov significant

#checks on the model
model3AIC<-AIC(modelAnthony3, k = 2)
model3BIC <- BIC(modelAnthony3)
model3VIF<- vif(modelAnthony3) #VIF MUCH better
#robust SE
rob2<-standard_error_robust(modelAnthony3) 

#Anthony model 4 (GDP_sq instead of ln_GDP) - Rishman model 5
modelAnthony4 <- lm(lndomexp_thousand ~ birdspeciesthreatened +
                      mammalspeciesthreatened +
                      ln_landarea +
                      Price_Index_yr2011+
                      terrestrialandmarineprotectedare +
                      GDP_sq +
                      Gov, na.action = na.exclude, FinNeeds)
summary(modelAnthony4) #r2 .662, intercept, ln_landarea, GDP_sq, and Gov significant
#checks on the model
model4AIC<-AIC(modelAnthony4, k = 2)
model4BIC <- BIC(modelAnthony4)
model4VIF<- vif(modelAnthony4) #VIF MUCH better
#robust SE
rob2<-standard_error_robust(modelAnthony4) 

#Anthony model 5, no Gov, Rishman model 6
modelAnthony5 <- lm(lndomexp_thousand~ birdspeciesthreatened +
                      mammalspeciesthreatened +
                      ln_landarea +
                      Price_Index_yr2011+
                      terrestrialandmarineprotectedare +
                      GDP_sq, na.action = na.exclude, FinNeeds)
summary(modelAnthony5) #r2 .595 Intercept, ln_landarea, GDP_sq significant

#checks on the model
model5AIC<-AIC(modelAnthony5, k = 2)
model5BIC <- BIC(modelAnthony5)
model5VIF<- vif(modelAnthony5) #Lowest VIF scores, but higher AIC
#robust SE
rob2<-standard_error_robust(modelAnthony4)


