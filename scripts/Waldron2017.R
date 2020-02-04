#Waldron 2017 Data

library(devtools)
P <- rprojroot::find_rstudio_root_file

#Read in text file
data<-read.delim("data/Waldron_data.txt", sep="\t")

write.csv(data, P("data/Waldron.csv"), append = FALSE)
