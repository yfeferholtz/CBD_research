#Exploratory Regressions
library(readxl)
library(dplyr)
library(stringr)
library(devtools)
P <- rprojroot::find_rstudio_root_file

# read data
library(readxl)
#data <- read_excel(P("data/final_data.xls")) 
data <- read.csv(P("data/final_data.csv")) 
colnames(data)[6] <- "ISO3"

library(countrycode)
ISO2 = countrycode(data$ISO3, "iso3c", "iso2c")
data <- cbind(data, ISO2)

library(WDI)
# read data from world bank dataset
gdp.gr.rate = WDI(indicator='NY.GDP.MKTP.KD.ZG', start=2018, end=2018) %>%
  dplyr::select(iso2c, NY.GDP.MKTP.KD.ZG)
colnames(gdp.gr.rate)[1] <- "ISO2"
colnames(gdp.gr.rate)[2] <- "gdp.gr.rate"
gdp.gr.rate$ISO2 <- as.factor(gdp.gr.rate$ISO2)

data.with.gdprate <- merge(data, gdp.gr.rate, by = 'ISO2')

tot.nat.res = WDI(indicator='NY.GDP.TOTL.RT.ZS', start=2016, end=2016) %>%
  dplyr::select(iso2c, NY.GDP.TOTL.RT.ZS)
colnames(tot.nat.res)[1] <- "ISO2"
colnames(tot.nat.res)[2] <- "tot.nat.res"
tot.nat.res$ISO2 <- as.factor(tot.nat.res$ISO2)

data.with.new <- merge(data.with.gdprate, tot.nat.res, by = 'ISO2')


library(tidyr)
data.no.na <- data[, c(5:22)] %>%
  drop_na()

data.plot1 <- data[, c(2:22)] %>%
  drop_na

data.finance <-  data[, c(6, 49, 50)] %>% 
  drop_na(dom_exp)

data.selected <- data.no.na[, c(2:18)] 

data.pca <- data.selected[, c(2:17)]

pca.16vars <- prcomp(data.pca, center = TRUE, scale = TRUE)
summary(pca.16vars)

# str(first.pca)

library(ggbiplot)
ggbiplot(first.pca, labels=rownames(data.no.na))

ggbiplot(first.pca, ellipse=TRUE, choices =1:2, groups=data.plot1$type) +
#scale_colour_manual(name="Origin", values= c("forest green", "red3", "dark blue"))+
  ggtitle("PCA for country data") +
  xlab('High GDP <-> Low GDP') +
  ylab('High Gov Effectiveness <-> Low Gov Eff.') +
  theme_minimal()+
  theme(legend.position = "right")


library(factoextra)
fviz_eig(first.pca)

fviz_pca_ind(first.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(first.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

screeplot(first.pca, type = "l", npcs = 15, main = "Screeplot of the first 6 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(first.pca$sdev^2 / sum(first.pca$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = cumpro[6], col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)

plot(first.pca$x[,1],first.pca$x[,2], xlab="PC1 (27)", ylab = "PC2 (20%)", main = "PC1 / PC2 - plot")

fviz_pca_ind(first.pca, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = data.no.na$type, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))

pca <- as.data.frame(pca.16vars$x[,1])
colnames(pca)[1] <- 'pc_1'
pca$pc_2 <- pca.16vars$x[,2]
pca$pc_3 <- pca.16vars$x[,3]
pca$pc_4 <- pca.16vars$x[,4]
pca$pc_5 <- pca.16vars$x[,5]
pca$pc_6 <- pca.16vars$x[,6]
pca$ISO3 <- data.selected$ISO3

data.for.lm <- merge(data.finance, pca, by = 'ISO3') %>%
  mutate(ln.dom_exp = log(dom_exp)) %>%
  mutate(ln.needs = log(needs))

lm.one <- lm(ln.dom_exp ~ pc_1 + pc_2 + pc_3 + pc_4 + pc_5 + pc_6, data = data.for.lm)
summary(lm.one)

prediction.lm.one <- predict.lm(lm.one)
exp.prediction.lm.one <- exp(prediction.lm.one)*1000
sum.exp.prediction.lm.one <- sum(exp.prediction.lm.one)

#add constant to data1
constant <- rep(1, nrow(pca))
data.pred <-cbind(constant, pca)

ln_extrapolate <-
  lm.one$coefficients[[1]]*data.pred$constant +
  lm.one$coefficients[[2]]*data.pred$pc_1 +
  lm.one$coefficients[[3]]*data.pred$pc_2 +  
  lm.one$coefficients[[4]]*data.pred$pc_3 +
  lm.one$coefficients[[5]]*data.pred$pc_4 +
  lm.one$coefficients[[6]]*data.pred$pc_5 +
  lm.one$coefficients[[7]]*data.pred$pc_6

exp.ln_extrapolate <- exp(ln_extrapolate)*1000
sum.exp.ln_extrapolate <- sum(exp.ln_extrapolate)

data.final <- cbind(data.pred, exp.ln_extrapolate, ln_extrapolate)

data.final <- data.final %>%
  mutate( diff.exp = data$dom_exp*1000 - exp.ln_extrapolate) 

data.final2 <- data.final %>%  
  drop_na(diff.exp)

sum(data.final2$diff.exp)

data.final3 <- data.final2 %>%  
  dplyr::filter(diff.exp < -5e7)

ggplot(data.final3, aes(x= reorder(ISO3,diff.exp), diff.exp, label=rownames(ISO3)))+ 
  geom_bar(stat ="identity") 
  
# Financial needs
lm.fin.needs <- lm(ln.needs ~ ln_extrapolate , data.final)
summary(lm.fin.needs)

prediction.needs <- predict.lm(lm.fin.needs)
exp.prediction.needs <- exp(prediction.needs)*1000
sum.exp.prediction.needs <- sum(exp.prediction.needs)

ln.needs <-
  lm.fin.needs$coefficients[[1]]*data.final$constant+
  lm.fin.needs$coefficients[[2]]*data.final$ln_extrapolate 

exp.ln.needs <- exp(ln.needs)*1000
sum.exp.ln.needs <- sum(exp.ln.needs)

data.final <- cbind(data.final, exp.ln.needs)
write.table(data.final, P("data/Rishman_fin_needs.xls"), sep="\t")

########
# Waldron data
data_waldron <- read_excel(P("data/Waldron.xlsx")) 
colnames(data_waldron)[3] <- "ISO3"

#all.data <- merge(data, data_waldron, by='ISO3')
#write.table(all.data, P("data/all_data.csv"), sep="\t")


data.no.na2 <- data_waldron[, c(3,4,5,10:180)]

data.no.na2 <- data.no.na2 %>%
  drop_na() 

data.no.na2 <- data.no.na2 %>%
  dplyr::filter(ISO3!='USA')

data.no.na3 <- data.no.na2[ ,3:ncol(data.no.na2)] 

waldron.pca <- prcomp(data.no.na3, center = TRUE, scale = TRUE)
summary(waldron.pca)

#str(waldron.pca)

library(ggbiplot)
ggbiplot(waldron.pca, labels=rownames(data.no.na3))
screeplot(waldron.pca, type = "l", npcs = 15, main = "Screeplot of the first 6 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

pca.waldron <- as.data.frame(waldron.pca$x[,1])
colnames(pca.waldron)[1] <- 'pc_1'
pca.waldron$pc_2 <- waldron.pca$x[,2]
pca.waldron$pc_3 <- waldron.pca$x[,3]
pca.waldron$pc_4 <- waldron.pca$x[,4]
pca.waldron$pc_5 <- waldron.pca$x[,5]
pca.waldron$pc_6 <- waldron.pca$x[,6]
pca.waldron$pc_7 <- waldron.pca$x[,7]
pca.waldron$pc_8 <- waldron.pca$x[,8]
pca.waldron$pc_9 <- waldron.pca$x[,9]
pca.waldron$pc_10 <- waldron.pca$x[,10]
pca.waldron$pc_11 <- waldron.pca$x[,11]
pca.waldron$pc_12 <- waldron.pca$x[,12]
pca.waldron$pc_13 <- waldron.pca$x[,13]
pca.waldron$ISO3 <- data.no.na2$ISO3

data.for.lm.waldron <- merge(data, pca.waldron, by='ISO3') %>%
  mutate(ln.dom_exp = log(dom_exp)) %>%
  mutate(ln.needs = log(needs))

lm.waldron <- lm(ln.dom_exp ~  
                pc_1 + 
                pc_2 + 
                pc_3 + 
                pc_4 + 
                pc_5 + 
                pc_6 +
                pc_7 +
                pc_8 +
                pc_9 +
                pc_10 +
                pc_11 +
                pc_12 +
                pc_13,
              data = data.for.lm.waldron)
summary(lm.waldron)

prediction.lm.waldron <- predict.lm(lm.waldron)
exp.prediction.lm.waldron <- exp(prediction.lm.waldron)*1000
sum.exp.prediction.lm.waldron <- sum(exp.prediction.lm.waldron)

#add constant to data1
constant <- rep(1, 137)
data.pred.waldron <- cbind(data.for.lm.waldron,constant)

ln_extrapolate.waldron <-
  lm.waldron$coefficients[[1]]*data.pred.waldron$constant+
  lm.waldron$coefficients[[2]]*data.pred.waldron$pc_1 +
  lm.waldron$coefficients[[3]]*data.pred.waldron$pc_2+
  lm.waldron$coefficients[[4]]*data.pred.waldron$pc_3+
  lm.waldron$coefficients[[5]]*data.pred.waldron$pc_4+
  lm.waldron$coefficients[[6]]*data.pred.waldron$pc_5+
  lm.waldron$coefficients[[7]]*data.pred.waldron$pc_6+
  lm.waldron$coefficients[[8]]*data.pred.waldron$pc_7+
  lm.waldron$coefficients[[9]]*data.pred.waldron$pc_8+
  lm.waldron$coefficients[[10]]*data.pred.waldron$pc_9+
  lm.waldron$coefficients[[11]]*data.pred.waldron$pc_10+
  lm.waldron$coefficients[[12]]*data.pred.waldron$pc_11+
    lm.waldron$coefficients[[13]]*data.pred.waldron$pc_12+
    lm.waldron$coefficients[[14]]*data.pred.waldron$pc_13

exp.ln_extrapolate.waldron <- exp(ln_extrapolate.waldron)*1000
sum.exp.ln_extrapolate.waldron <- sum(exp.ln_extrapolate.waldron)

data.final.waldron <- cbind(data.pred.waldron, ln_extrapolate.waldron,  exp.ln_extrapolate.waldron)

data.final.waldron <- data.final.waldron %>%
  mutate( diff.exp = data.for.lm.waldron$dom_exp*1000 - exp.ln_extrapolate.waldron)

data.final.waldron2 <- data.final.waldron %>%  
  drop_na(diff.exp)

sum(data.final.waldron2$diff.exp)/1e9

data.final.waldron3 <- data.final.waldron2 %>%  
  dplyr::filter(diff.exp > 1e9)
ggplot(data.final.waldron3, aes(x= reorder(ISO3,diff.exp), diff.exp, label=rownames(ISO3)))+ 
  geom_bar(stat ="identity") 


lm.waldron.fin.needs <- lm( ln.needs ~ ln_extrapolate.waldron , data.final.waldron)
summary(lm.waldron.fin.needs)

prediction.needs.waldron <- predict.lm(lm.waldron.fin.needs)
exp.prediction.needs.waldron <- exp(prediction.needs.waldron)*1000
sum.exp.prediction.needs.waldron <- sum(exp.prediction.needs.waldron)

ln.needs.waldron <-
  lm.waldron.fin.needs$coefficients[[1]]*data.final.waldron$constant+
  lm.waldron.fin.needs$coefficients[[2]]*data.final.waldron$ln_extrapolate.waldron 

exp.ln.needs.waldron <- exp(ln.needs.waldron)*1000
sum.exp.ln.needs.waldron <- sum(exp.ln.needs.waldron)


data.final.waldron4 <- cbind(data.final.waldron, exp.ln.needs.waldron)
ggplot(data.final.waldron4 %>% dplyr::filter(exp.ln.needs.waldron>1e10), aes(x= reorder(ISO3,exp.ln.needs.waldron), exp.ln.needs.waldron, label=rownames(ISO3)))+ 
  geom_bar(stat ="identity") 

write.table(data.final.waldron4, P("data/waldron_fin_needs.xls"), sep="\t")
