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
    CO2_Ems,
    GDP_CO2,
    oilrentsofgdp,
    agriculturallandoflandarea,
    average_population_density,
    mammalspeciesthreatened,
    birdspeciesthreatened,
    terrestrialprotectedareasoftotal,
    domexp_thousand
  ) %>%
  mutate(ln.dom.exp = log(domexp_thousand))

data_new.all.vars <- data_new.all.vars %>% select(-domexp_thousand)

data_new.15vars <- as.data.frame(data_new.all.vars) %>%
  drop_na(    ln.dom.exp,
              lnGDP,
              GDP_sq,
              ln_landarea,
              governmenteffectivenessestimate,
              politicalstabilityandabsenceofvi,
              CO2_Ems,
              GDP_CO2,
              oilrentsofgdp,
              agriculturallandoflandarea,
              average_population_density,
              mammalspeciesthreatened,
              birdspeciesthreatened,
              terrestrialprotectedareasoftotal
  )

library(ranger)
set.seed(1234)
expenditures_rf = ranger( ln.dom.exp ~ 
                          lnGDP +
                          GDP_sq +
                          ln_landarea +
                          governmenteffectivenessestimate +
                          politicalstabilityandabsenceofvi +
                          CO2_Ems +
                          GDP_CO2 + 
                          oilrentsofgdp +
                          agriculturallandoflandarea +
                          average_population_density +
                          mammalspeciesthreatened +
                          birdspeciesthreatened +
                          terrestrialprotectedareasoftotal,
                    data = data_new.15vars,
                    probability = FALSE,
                    classification = FALSE)


data_new.all.vars.no.exps <- as.data.frame(data_new.all.vars) %>% select(-ln.dom.exp) %>% 
  drop_na(  lnGDP,
              GDP_sq,
              ln_landarea,
              governmenteffectivenessestimate,
              politicalstabilityandabsenceofvi,
              CO2_Ems,
              GDP_CO2,
              oilrentsofgdp,
              agriculturallandoflandarea,
              average_population_density,
              mammalspeciesthreatened,
              birdspeciesthreatened,
              terrestrialprotectedareasoftotal
  )
# prediction
predicted.sample <- expenditures_rf %>%
  predict(data_new.all.vars.no.exps)

sum.dom.exp <- sum(exp(predicted.sample$predictions))/1e9

library("DALEX")
dom.exp.ex = explain(expenditures_rf,
                     data  = data_new.15vars,
                     y     = data_new.15vars$ln.dom.exp,
                     label = "Regression Forest")

dom.exp.ex %>%
  model_performance() %>%
  plot(geom = 'roc')

dom.exp.ex %>%
  model_parts() %>%
  plot(show_boxplots = FALSE)

dom.exp.ex %>%
  predict_parts(data_new.15vars[20,]) %>%
  plot()

dom.exp.ex %>%
  model_profile() %>%
  plot(variables = "lnGDP", "mammalspeciesthreatened")

library(rms)

library("modelDown")
modelDown(dom.exp.ex)

library(modelStudio)
modelStudio(dom.exp.ex)


