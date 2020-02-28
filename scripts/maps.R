###Maps###
#setup
library(devtools)
P <- rprojroot::find_rstudio_root_file
data_by_country<- read_excel(P("data/data_by_country.xlsx"))

#map set up
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(dplyr)
world <- ne_countries(scale='medium',returnclass = 'sf')
world <- world %>%
  rename(countries=sovereignt)

#merge dataframes
new_data<-merge(world, data_by_country, by="countries", all=TRUE)

### Maps for Expenditures ###
#map for yhat_extrapdomexp
ggplot(data=new_data)+
  geom_sf(aes(fill=yhat_extrapdomexp))+
  scale_fill_gradient2(na.value="gray")+
  theme_classic()

#map for lnmanual_extrapdomexp
ggplot(data=new_data)+
  geom_sf(aes(fill=lnmanual_extrapdomexp))+
  scale_fill_gradient2(na.value="gray")+
  theme_classic()

#map for yhat_method1
p1 <-ggplot(data=new_data)+
  geom_sf(aes(fill=yhat_method1))+
  scale_fill_gradient2(na.value="gray")+
  theme_classic()
saveRDS(p1, 'outputs/maps/yhat_method1')

#map for yhat_method2
p2<-ggplot(data=new_data)+
  geom_sf(aes(fill=yhat_method2))+
  scale_fill_gradient2(na.value="gray")+
  theme_classic()
saveRDS(p2, 'outputs/maps/yhat_method2')

### Maps for needs ###
#map for extrap_needs_method1
p1 <-ggplot(data=new_data)+
  geom_sf(aes(fill=extrap_needs_method1))+
  scale_fill_gradient2(na.value="gray")+
  theme_classic()
p1
saveRDS(p1, 'outputs/maps/extrap_needs_method1')

#map for extrap_needsII
p1 <-ggplot(data=new_data)+
  geom_sf(aes(fill=extrap_needsII))+
  scale_fill_gradient2(na.value="gray")+
  theme_classic()
p1
saveRDS(p1, 'outputs/maps/extrap_needsII')





