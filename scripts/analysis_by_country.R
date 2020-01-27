# Second analysis 

library(dplyr)
library(stringr)
library(devtools)
P <- rprojroot::find_rstudio_root_file

# read data
gef_data <- readRDS(P('outputs/clean_gef_data.RDS'))

# filter biodiversity
biodiv_data <- gef_data %>%
  group_by(Countries)%>%
  filter(str_detect(Focal.Areas,"Biodiversity"))

# create sum of grants
biodiv_data <- biodiv_data %>%
  group_by(Countries)%>%
  mutate(sum_biodiv=sum(Grant))
