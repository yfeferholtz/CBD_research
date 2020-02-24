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
modelAnthony <- lm(lndomexp_thousand ~ birdspeciesthreatened +
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
modelAnthony2rob <- rlm(lndomexp_thousand ~ birdspeciesthreatened +
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
modelAnthony3rob<- rlm(lndomexp_thousand~ birdspeciesthreatened+
                     mammalspeciesthreatened +
                     ln_landarea+ Price_Index_yr2011 +
                     terrestrialandmarineprotectedare +
                     lnGDP +
                     GDP_sq +
                     Gov, na.action = na.exclude, data = FinNeeds)
#Model fit
modelAnthony3 <- lm(lndomexp_thousand~ birdspeciesthreatened+
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
modelAnthony4rob<- rlm(lndomexp_thousand ~ birdspeciesthreatened +
                     mammalspeciesthreatened+
                     ln_landarea +
                     Price_Index_yr2011 +
                     terrestrialandmarineprotectedare +
                     lnGDP +
                     Gov, na.action = na.exclude, FinNeeds)
#fit
modelAnthony4<-lm(lndomexp_thousand ~ birdspeciesthreatened +
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
modelAnthony5rob <- rlm(lndomexp_thousand ~ birdspeciesthreatened +
                      mammalspeciesthreatened +
                      ln_landarea +
                      Price_Index_yr2011+
                      terrestrialandmarineprotectedare +
                      GDP_sq +
                      Gov, na.action = na.exclude, FinNeeds)
#fit
modelAnthony5 <- lm(lndomexp_thousand ~ birdspeciesthreatened +
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
modelAnthony6rob <- rlm(lndomexp_thousand~ birdspeciesthreatened +
                      mammalspeciesthreatened +
                      ln_landarea +
                      Price_Index_yr2011+
                      terrestrialandmarineprotectedare +
                      GDP_sq, na.action = na.exclude, FinNeeds)
#fit
modelAnthony6 <- lm(lndomexp_thousand~ birdspeciesthreatened +
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
extrapExpAnth<- sum(exp(yhatAnthony[is.na(yhatAnthony)==FALSE]))/1E9 #43.4 bill

#manual extrapolation

ln_anthony <-
  modelAnthony5rob$coefficients[[1]]*FinNeeds$constant+
  modelAnthony5rob$coefficients[[2]]*FinNeeds$birdspeciesthreatened+
  modelAnthony5rob$coefficients[[3]]*FinNeeds$mammalspeciesthreatened+
  modelAnthony5rob$coefficients[[4]]*FinNeeds$ln_landarea+
  modelAnthony5rob$coefficients[[5]]*FinNeeds$Price_Index_yr2011+
  modelAnthony5rob$coefficients[[6]]*FinNeeds$terrestrialandmarineprotectedare+
  modelAnthony5rob$coefficients[[7]]*FinNeeds$GDP_sq+
  modelAnthony5rob$coefficients[[8]]*FinNeeds$Gov

FinNeeds$anthonyMan <- exp(ln_anthony)
extraExpAnthMan<- sum(exp(ln_anthony[is.na(ln_anthony)==FALSE]))/1E9 #103.07 bill

#Table 5 - extrapolated needs
modelNeedsAnth <- lm(ln_newneeds ~ yhatAnthony, na.action = na.exclude, FinNeeds)
summary(modelNeedsAnth)
modelNeedsAnthMod <- lm(ln_newneeds ~ anthonyMan, na.action = na.exclude, FinNeeds)
summary(modelNeedsAnthMod)

#find the largest outliers
FinNeeds$difference = FinNeeds$dom_exp-FinNeeds$anthonyMan
FinNeeds$absdiff = abs(FinNeeds$difference)

#take off top two outliers
library(plyr)
N<-2
altered_data<- FinNeeds %>% 
  arrange(desc(absdiff)) %>% 
  tail(-N)
#plot
library(ggplot2)
library(plotly)
ggplotly(
ggplot(FinNeeds)+
  geom_point(aes(dom_exp, anthonyMan)))

