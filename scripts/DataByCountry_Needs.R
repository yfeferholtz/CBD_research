rm(list=ls())

library(devtools)
P <- rprojroot::find_rstudio_root_file

#import data
require(readxl)
data_by_country<- read_excel(P("data/data_by_country.xlsx"))