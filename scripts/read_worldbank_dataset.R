library(dplyr)
library(WDI)
library(devtools)
P <- rprojroot::find_rstudio_root_file

country_list <- c('ABW','AFG','AGO','ARM','BEN','BFA','BGD','BMU','BOL','BRA','CCK','CHE','CHL',
                  'COM','CPV','CRI','DMA','DNK','DOM','ETH','FIN','FJI','GGY','GHA','GIB','GRL','GTM','GUF',
                  'HUN','IDN','IMN','ITA','JAM','JEY','KNA','KOR','KWT','LSO','LTU','LUX','MDV','MEX','MHL',
                  'MOZ','MRT','MSR','NER','NFK','NGA','OMN','PAK','PAN','PRK','PRT','PRY','SAU','SDN','SEN',
                  'SMR','SOM','SPM','SYC','SYR','TCA','TON','TTO','TUN','URY','USA','UZB','WLF','WSM','YEM',
                  'AIA','ALA','ATF','ATG','BGR','BHR','BRB','BRN','CHN','CIV','CUB','CXR','DZA','ECU','FLK',
                  'FRA','GIN','GLP','GUM','GUY','IND','IOT','JOR','JPN','LAO','LBN','LVA','MAC','MKD','MLI',
                  'MTQ','MUS','NIC','NIU','PCN','PER','PSE','PYF','SGP','SGS','SRB','STP','TCD','TGO','TUR',
                  'TUV','VAT','VCT','ZAF','ZMB','ALB','AUS','AUT','AZE','BHS','BIH','BLM','BTN',
                  'BVT','BWA','CMR','COD','COG','CYM','CYP','CZE','EGY','ERI','ESH','FRO','FSM','GAB','GMB',
                  'GNB','GNQ','HKG','HMD','HND','IRL','IRN','IRQ','KAZ','KEN','KGZ','LBR','LBY','LCA',
                  'MAR','MCO','MLT','MMR','MNE','MWI','MYS','MYT','NLD','NOR','NPL','PHL','PLW','PNG','QAT',
                  'REU','ROU','SHN','SJM','SLB','SUR','SVK','SVN','THA','TJK','TKL','TWN','TZA','UGA','VEN',
                  'VGB','VIR','ZWE','ARE','ARG','BDI','BEL','BLR','BLZ','CAF','CAN','COK','COL','DEU','DJI',
                  'ESP','EST','GBR','GEO','GRC','GRD','HRV','HTI','ISL','ISR','KHM','KIR','LIE','LKA','MDA',
                  'MDG','MNG','MNP','NAM','NCL','NRU','NZL','POL','PRI','RUS','RWA','SLE','SLV','SWE','SWZ',
                  'TKM','TLS','UKR','UMI','VNM','VUT')

# read data from world bank dataset
ag_land = WDI(indicator='AG.LND.AGRI.ZS', start=2010, end=2018) %>%
  dplyr::select(iso2c, AG.LND.AGRI.ZS)
colnames(ag_land)[1] <- "ISO2"
colnames(ag_land)[2] <- "ag_land"
ag_land$ISO2 <- as.factor(ag_land$ISO2)




