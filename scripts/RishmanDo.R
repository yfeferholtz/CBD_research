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

FinNeeds<-FinNeeds[order(FinNeeds$type, FinNeeds$countries),]
