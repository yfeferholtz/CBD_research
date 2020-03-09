#### Projections ####
library(stringr)
library(devtools)
P <- rprojroot::find_rstudio_root_file

# read data
library(readxl)
data_new <- read_xls(P("data/financial_needs_data3.xls"))
data_projected <- read.csv((P('outputs/BAUdata.csv')))

projected.BAU <- data_projected %>%
  dplyr::select(countrycode, ln_futureGDP, futureGDP_sq, ln_futureAgLand, ln_futureco2 )

data_projected.BAU <- left_join(data_new, projected.BAU , by='countrycode')

data_new.pca2 <- data_projected.BAU %>%
  dplyr::select( 
    ln_futureGDP, 
    futureGDP_sq,
    ln_landarea,
    governmenteffectivenessestimate,
    politicalstabilityandabsenceofvi,
    Price_Index_yr2011,
    ln_futureco2,
    GDP_CO2,
    oilrentsofgdp,
    ln_futureAgLand,
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

data_for.pca2 <- data_new.pca2 %>%
  drop_na(    ln_futureGDP, 
              futureGDP_sq,
              ln_landarea,
              governmenteffectivenessestimate,
              politicalstabilityandabsenceofvi,
              Price_Index_yr2011,
              ln_futureco2,
              GDP_CO2,
              oilrentsofgdp,
              ln_futureAgLand,
              average_population_density,
              mammalspeciesthreatened,
              birdspeciesthreatened,
              terrestrialprotectedareasoftotal,
  ) %>%
  dplyr::arrange(countries)

# Dataframe with only numeric variables for PCA
pca2.projected.gdp.14vars <- data_for.pca2[, c(1:14)]

# Estimate PCA
pca2.14vars <- prcomp(pca2.projected.gdp.14vars, retx = TRUE, center = TRUE, scale = TRUE)

# Gives the proportion of variance explained per PC
summary(pca2.14vars)

# PRedict factor PCs
predict.pca2 <- as.data.frame(predict(pca2.14vars, pca2.projected.gdp.14vars))
predict.pca2 <- cbind(data_for.pca2, predict.pca2)

# Regress coefficients from PCA to estimate dom expenditures
lm.projection.gdp <- lm(ln.dom.exp ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = predict.pca2)
summary(lm.projection.gdp)
prediction.lm.projection.gdp <- predict(lm.projection.gdp)
exp.prediction.lm.projection.gdp <- exp(prediction.lm.projection.gdp)
sum(exp.prediction.lm.projection.gdp)/1e9

#add constant to data1
constant <- rep(1, nrow(predict.pca2))
data.pred.with.gdp <-cbind(constant, predict.pca2)

# Predict dom expenditures for all observation 
ln_dom_exp_projected <-
  lm.projection.gdp$coefficients[[1]]*data.pred.with.gdp$constant +
  lm.projection.gdp$coefficients[[2]]*data.pred.with.gdp$PC1 +
  lm.projection.gdp$coefficients[[3]]*data.pred.with.gdp$PC2 +  
  lm.projection.gdp$coefficients[[4]]*data.pred.with.gdp$PC3 +
  lm.projection.gdp$coefficients[[5]]*data.pred.with.gdp$PC4 +
  lm.projection.gdp$coefficients[[6]]*data.pred.with.gdp$PC5 +
  lm.projection.gdp$coefficients[[7]]*data.pred.with.gdp$PC6

exp.ln_dom_exp_projected <- exp(ln_dom_exp_projected)
sum(exp.ln_dom_exp_projected)/1e9

# Merge with other data
data.final.projected2 <- cbind(predict.pca2, ln_dom_exp_projected, exp.ln_dom_exp_projected, constant)

# Plot regression
ggplot(data.final.projected2, aes(ln.dom.exp, ln_dom_exp_projected )) +
  geom_point() +
  geom_smooth(method=lm, color= 'red') +
  #xlim(13, 22.5) +
  #ylim(13, 22.5) +
  xlab('Observed Domestic Expenditures in Logs') +
  ylab('Projected Domestic Expenditures in Logs') +
  #geom_text(x=15.5, y=13.5, label="Guinea", color = 'red') +
  #geom_text(x=21, y=22.5, label="Exact prediction", color = 'red') +
  theme_classic()
# Regress predicted dom expenditures with fin needs
lm.fin.needs.projected.BAU <- lm(ln.needs ~ ln_dom_exp_projected , data.final.projected2)
summary(lm.fin.needs.projected.BAU) # Only 40 observations are used for regression

prediction.needs.projected.BAU <- predict.lm(lm.fin.needs.projected.BAU)
exp.prediction.needs.projected.BAU <- exp(prediction.needs.projected.BAU)
sum(exp.prediction.needs.projected.BAU)/1e9

ln.needs.projected.BAU <-
  lm.fin.needs.projected.BAU$coefficients[[1]]*data.final.projected2$constant+
  lm.fin.needs.projected.BAU$coefficients[[2]]*data.final.projected2$ln_dom_exp_projected 

exp.ln.needs.projected.BAU <- exp(ln.needs.projected.BAU)
sum(exp.ln.needs.projected.BAU)/1e9 # 102.4 billion

data.final.projected3 <- cbind(data.final.projected2, exp.ln.needs.projected.gdp, ln.needs.projected.gdp)
# Plot regression
ggplot(data.final.projected3, aes(ln.needs, ln.needs.projected.gdp )) +
  geom_point() +
  geom_smooth(method=lm, color= 'blue') +
  geom_abline(intercept = 0, slope = 1, color = 'red') +
  xlim(13, 23) +
  ylim(13, 23) +
  xlab('Observed Financial Needs in Logs') +
  ylab('Predicted Financial Needs in Logs') +
  #geom_text(x=15.5, y=13.5, label="Guinea", color = 'red') +
  #geom_text(x=21, y=22.5, label="Exact prediction", color = 'red') +
  theme_classic()
