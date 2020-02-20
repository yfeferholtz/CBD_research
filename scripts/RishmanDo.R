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
rob1<-parameters::standard_error_robust(modelAnthony)
