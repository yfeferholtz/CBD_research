# Second analysis 

library(dplyr)
library(stringr)
library(devtools)
P <- rprojroot::find_rstudio_root_file

# read data
gef_data <- readRDS(P('outputs/clean_gef_data.RDS'))

### GEF 6 ###
# filter biodiversity
biodiv_data_gef6 <- gef_data %>%
  filter(Period == "GEF - 6") %>%
  filter(str_detect(Focal.Areas,"Biodiversity"))
 
# create sum of grants
biodiv_data_gef6 <- biodiv_data_gef6 %>%
  group_by(Countries)%>%
  summarise(sum_biodiv=sum(Grant), sum_biodiv_cofin = sum(Cofinancing) ) %>% 
  mutate(total_funds = sum_biodiv+ sum_biodiv_cofin)

library(tidyr)
biodiv_data_gef6_pos <- biodiv_data_gef6 %>%
  separate_rows(Countries, sep = ", ") %>%
  filter(total_funds >0)

freq.gef6 <- data.frame(table(biodiv_data_gef6_pos$total_funds)) %>%
  dplyr::rename("total_funds" = Var1)

sum.projects.by.country.gef6 <- merge(biodiv_data_gef6_pos, freq.gef6, by= "total_funds") %>%
  mutate(Grant = total_funds/Freq) %>%
  group_by(Countries) %>%
  summarise(sum_funds_gef6 = sum(Grant))

sum(sum.projects.by.country.gef6$sum_funds_gef6)/1e9

### GEF 7 ###
biodiv_data_gef7 <- gef_data %>%
  filter(Period == "GEF - 7") %>%
  filter(str_detect(Focal.Areas,"Biodiversity")) 

# create sum of grants
biodiv_data_gef7 <- biodiv_data_gef7 %>%
  group_by(Countries)%>%
  summarise(sum_biodiv=sum(Grant), sum_biodiv_cofin = sum(Cofinancing) ) %>% 
  mutate(total_funds = sum_biodiv+ sum_biodiv_cofin)

library(tidyr)
biodiv_data_gef7_pos <- biodiv_data_gef7 %>%
  separate_rows(Countries, sep = ", ") %>%
  filter(total_funds >0)

freq.gef7 <- data.frame(table(biodiv_data_gef7_pos$total_funds)) %>%
  dplyr::rename("total_funds" = Var1)

sum.projects.by.country.gef7 <- merge(biodiv_data_gef7_pos, freq.gef7, by= "total_funds") %>%
  mutate(Grant = total_funds/Freq) %>%
  group_by(Countries) %>%
  summarise(sum_funds_gef7 = sum(Grant))

sum(sum.projects.by.country.gef7$sum_funds_gef7)/1e9

both_gef <- right_join(sum.projects.by.country.gef7, sum.projects.by.country.gef6, by= "Countries")
