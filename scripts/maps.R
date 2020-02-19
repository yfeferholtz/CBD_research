###Maps###
#setup
library(devtools)
P <- rprojroot::find_rstudio_root_file

#load data
library(readxl)
data_by_country<- read_excel(P("data/data_by_country.xlsx"))

#map 
library(sf)
library(ggplot2)
ggplot(data=data_by_country)+
  geom_sf()
