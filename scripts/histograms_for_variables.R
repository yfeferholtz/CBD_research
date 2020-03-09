
library(dplyr)
datashort2 <- datashort %>%
  dplyr::mutate(ln_average_population_density = log(average_population_density), 
                                                    ln_emissions = log(CO2_Ems),
                                      ln_land   = log(agriculturallandoflandarea),
                                   ln_forest = log(average_forestarealandarea))
library(ggplot2)

ggplot(datashort2, aes(lnGDP))+
  geom_histogram(color='blue')

ggplot(datashort2, aes(ln_average_population_density))+
  geom_histogram(color='blue') 

ggplot(datashort2, aes(governmenteffectivenessestimate))+
  geom_histogram(color='blue') 

ggplot(datashort2, aes(ln_land))+
  geom_histogram(color='blue') 

ggplot(datashort2, aes(average_forestarealandarea))+
  geom_histogram(color='blue') 

ggplot(datashort2, aes(CO2_Ems))+
  geom_histogram(color='blue') 

# GDP
ggplot(datashort, aes(x = lnGDP, y = ln_newdomexp))+
    geom_point() +
  geom_smooth(method = lm, color='red')

# Government effectiveness
ggplot(datashort, aes(x = governmenteffectivenessestimate, y = ln_newdomexp))+
  geom_point() +
  geom_smooth(method = lm, color='red')

# Population density
ggplot(datashort2, aes(x = ln_average_population_density, y = ln_newdomexp))+
  geom_point() +
  geom_smooth(method = lm, color='red')

# Ag area
ggplot(datashort2, aes(x = ln_land, y = ln_newdomexp))+
  geom_point() +
  geom_smooth(method = lm, color='red')

# Forest area
ggplot(datashort2, aes(x = ln_forest, y = ln_newdomexp))+
  geom_point() +
  geom_smooth(method = lm, color='red')

# Emissions
ggplot(datashort2, aes(x = ln_emissions, y = ln_newdomexp))+
  geom_point() +
  geom_smooth(method = lm, color='red')



##### 
data1 <- data1 %>%
  mutate(ln_Threatened_Species = log(Threatened_Species))

reg_exp_species <- lm(ln_Threatened_Species ~
                        lnmanual_extrapdomexp , 
                      na.action=na.exclude,
                      data = data1)
summary(reg_exp_species)




ggplot(data1, aes(x = ln_Threatened_Species  , y = lnmanual_extrapdomexp ))+
  geom_point() +
  geom_smooth(method = lm, color='red')