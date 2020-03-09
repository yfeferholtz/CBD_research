# https://www.datacamp.com/community/tutorials/pca-analysis-r#intro
# https://uc-r.github.io/pca

library(stringr)
library(devtools)
P <- rprojroot::find_rstudio_root_file

# read data
library(readxl)
data_new <- read_xls(P("data/financial_needs_data3.xls"))

library(dplyr)
library(tidyverse)
data_new.all.vars <- data_new %>%
  dplyr::select( 
    lnGDP,
    GDP_sq,
    ln_landarea,
    governmenteffectivenessestimate,
    politicalstabilityandabsenceofvi,
    Price_Index_yr2011,
    CO2_Ems,
    GDP_CO2,
    oilrentsofgdp,
    agriculturallandoflandarea,
    average_population_density,
    mammalspeciesthreatened,
    birdspeciesthreatened,
    terrestrialprotectedareasoftotal,
    countries,
    countrycode,
    domexp_thousand,
    newneeds
    ) %>%
  mutate(ln.dom.exp = log(domexp_thousand)) %>%
  mutate(ln.needs = log(newneeds))

data_new.14vars <- data_new.all.vars %>%
  drop_na(    lnGDP,
              GDP_sq,
              ln_landarea,
              governmenteffectivenessestimate,
              politicalstabilityandabsenceofvi,
              Price_Index_yr2011,
              CO2_Ems,
              GDP_CO2,
              oilrentsofgdp,
              agriculturallandoflandarea,
              average_population_density,
              mammalspeciesthreatened,
              birdspeciesthreatened,
              terrestrialprotectedareasoftotal,
        ) %>%
  dplyr::arrange(countries)

# Dataframe with only numeric variables for PCA
data_new.pca <- data_new.14vars[, c(1:14)]

# Estimate PCA
pca.14vars <- prcomp(data_new.pca, retx = TRUE, center = TRUE, scale = TRUE)

# Gives the proportion of variance explained per PC
summary(pca.14vars)

# Generate a dataframe with PC prediction
pca <- as.data.frame(pca.14vars$x[,1])
colnames(pca)[1] <- 'pc_1'
pca$pc_2 <- pca.14vars$x[,2]
pca$pc_3 <- pca.14vars$x[,3]
pca$pc_4 <- pca.14vars$x[,4]
pca$pc_5 <- pca.14vars$x[,5]
pca$pc_6 <- pca.14vars$x[,6]

# Generate dataframe that includes dom expenditures and needs
data.for.lm <- cbind(data_new.14vars, pca) # careful how to merge these 2 datasets

# Regress coefficients from PCA to estimate dom expenditures
lm.one <- lm(ln.dom.exp ~ pc_1 + pc_2 + pc_3 + pc_4 + pc_5 + pc_6, data = data.for.lm)
summary(lm.one) # Only 66 observations are used
prediction.lm.one <- predict(lm.one)
exp.prediction.lm.one <- exp(prediction.lm.one)
sum(exp.prediction.lm.one)/1e9

#add constant to data1
constant <- rep(1, nrow(pca))
data.pred <-cbind(constant, pca)

# Predict dom expenditures for all observation 
ln_dom_exp_extrapolate <-
  lm.one$coefficients[[1]]*data.pred$constant +
  lm.one$coefficients[[2]]*data.pred$pc_1 +
  lm.one$coefficients[[3]]*data.pred$pc_2 +  
  lm.one$coefficients[[4]]*data.pred$pc_3 +
  lm.one$coefficients[[5]]*data.pred$pc_4 +
  lm.one$coefficients[[6]]*data.pred$pc_5 +
  lm.one$coefficients[[7]]*data.pred$pc_6

exp.ln_dom_exp_extrapolate <- exp(ln_dom_exp_extrapolate)
sum(exp.ln_dom_exp_extrapolate)/1e9

# Merge with otherr data
data.final <- cbind(data.for.lm, ln_dom_exp_extrapolate)
data.final2 <- cbind(data.final, exp.ln_dom_exp_extrapolate, constant) %>%
  mutate(diff.dom.exp = ln_dom_exp_extrapolate - ln.dom.exp)


library(ggplot2)
# Plot regression
ggplot(data.final2, aes(ln.dom.exp, ln_dom_exp_extrapolate)) +
  geom_point() +
  geom_smooth(method=lm, color= 'blue') +
  xlim(13, 22.5) +
  ylim(13, 22.5) +
  xlab('Observed Domestic Expenditures in Logs') +
  ylab('Predicted Domestic Expenditures in Logs') +
  geom_text(x=16, y=13.5, label="Guinea", color = 'red') +
  geom_text(x=15, y=19, label="Uruguay", color = 'red') +
  #geom_text(x=21, y=22.5, label="Exact prediction", color = 'red') +
  theme_classic()

# Regress predicted dom expenditures with fin needs
lm.fin.needs <- lm(ln.needs ~ ln_dom_exp_extrapolate , data.final2)
summary(lm.fin.needs) # Only 40 observations are used for regression

# Plot regression
ggplot(data.final2, aes(ln_dom_exp_extrapolate, ln.needs)) +
  geom_point() +
  geom_smooth(method=lm, color= 'red') +
  xlim(13, 22.5) +
  ylim(13, 22.5) +
  xlab('Predicted Domestic Expenditures in Logs') +
  ylab('Predicted Financial Needs in Logs') +
  geom_text(x=15.5, y=13.5, label="Guinea", color = 'red') +
  #geom_text(x=21, y=22.5, label="Exact prediction", color = 'red') +
  theme_classic()

prediction.needs <- predict.lm(lm.fin.needs)
exp.prediction.needs <- exp(prediction.needs)
sum(exp.prediction.needs)/1e9

ln.needs <-
  lm.fin.needs$coefficients[[1]]*data.final2$constant+
  lm.fin.needs$coefficients[[2]]*data.final2$ln_dom_exp_extrapolate 

exp.ln.needs <- exp(ln.needs)
sum(exp.ln.needs)/1e9



## Other MEthod ###
# Generate a dataframe with PC prediction
predict.pca <- as.data.frame(predict(pca.14vars, data_new.14vars))
predict.pca <- cbind(predict.pca, data_new.14vars$domexp_thousand )
predict.pca <- cbind(predict.pca, data_new.14vars$newneeds ) %>%
  mutate(ln.dom_exp = log(data_new.14vars$domexp_thousand)) %>%
  mutate(ln.needs = log(data_new.14vars$newneeds)) 


# Regress coefficients from PCA to estimate dom expenditures
lm.two <- lm(ln.dom_exp ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = predict.pca)
summary(lm.two)
prediction.lm.two <- predict(lm.two)
exp.prediction.lm.two <- exp(prediction.lm.two)
sum.exp.prediction.lm.two <- sum(exp.prediction.lm.two)

# Predict dom expenditures based on coeffients for observations
prediction.lm.two <- predict.lm(lm.two)
exp.prediction.lm.two <- exp(prediction.lm.two)
sum.exp.prediction.lm.two <- sum(exp.prediction.lm.two)


#add constant to data1
constant <- rep(1, nrow(predict.pca))
data.pred <-cbind(constant, predict.pca)

# Predict dom expenditures for all observation 
ln_dom_exp_extrapolate <-
  lm.two$coefficients[[1]]*data.pred$constant +
  lm.two$coefficients[[2]]*data.pred$PC1 +
  lm.two$coefficients[[3]]*data.pred$PC2 +  
  lm.two$coefficients[[4]]*data.pred$PC3 +
  lm.two$coefficients[[5]]*data.pred$PC4 +
  lm.two$coefficients[[6]]*data.pred$PC5 +
  lm.two$coefficients[[7]]*data.pred$PC6

exp.ln_dom_exp_extrapolate <- exp(ln_dom_exp_extrapolate)
sum(exp.ln_dom_exp_extrapolate)/1e9

# Merge with otherr data
data.final <- cbind(data.for.lm, ln_dom_exp_extrapolate)
data.final2 <- cbind(data.final, exp.ln_dom_exp_extrapolate, constant)

# Plot regression
ggplot(data.final2, aes(ln.dom.exp, ln_dom_exp_extrapolate )) +
  geom_point() +
  geom_smooth(method=lm, color= 'red') +
  #xlim(13, 22.5) +
  #ylim(13, 22.5) +
  xlab('Observed Domestic Expenditures in Logs') +
  ylab('Predicted Domestic Expenditures in Logs') +
  geom_text(x=15.5, y=13.5, label="Guinea", color = 'red') +
  #geom_text(x=21, y=22.5, label="Exact prediction", color = 'red') +
  theme_classic()


# Regress predicted dom expenditures with fin needs
lm.fin.needs <- lm(ln.needs ~ ln_dom_exp_extrapolate , data.final2)
summary(lm.fin.needs) # Only 40 observations are used for regression

# Plot regression
ggplot(data.final2, aes(ln_dom_exp_extrapolate, ln.needs)) +
  geom_point() +
  geom_smooth(method=lm, color= 'red') +
  #xlim(13, 22.5) +
  #ylim(13, 22.5) +
  xlab('Predicted Domestic Expenditures in Logs') +
  ylab('Predicted Financial Needs in Logs') +
  geom_text(x=15.5, y=13.5, label="Guinea", color = 'red') +
  #geom_text(x=21, y=22.5, label="Exact prediction", color = 'red') +
  theme_classic()

prediction.needs <- predict.lm(lm.fin.needs)
exp.prediction.needs <- exp(prediction.needs)
sum(exp.prediction.needs)/1e9

ln.needs <-
  lm.fin.needs$coefficients[[1]]*data.final2$constant+
  lm.fin.needs$coefficients[[2]]*data.final2$ln_dom_exp_extrapolate 

exp.ln.needs <- exp(ln.needs)
sum(exp.ln.needs)/1e9 #94.25 billion
