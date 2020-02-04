#Waldron 2017 Data

library(devtools)
P <- rprojroot::find_rstudio_root_file

#Read in text file
data<-read.delim("data/Waldron_data.txt", sep="\t")

library(xlsx)

write.xlsx2(data, "Waldron2017Data", sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE, append = FALSE)
