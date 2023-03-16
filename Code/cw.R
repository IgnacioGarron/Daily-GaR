library(readxl)            # read excel
library(tidyverse)
library(ggpubr) # ggarrrange
library(GAS) # backtesting VaR
library(forecast)

########################################################

rm(list = ls()) # Limpiar environment
setwd("/Users/ignaciogarronvedia/Documents/GitHub/Daily-GaR")
set.seed(12345)
seed_n<-12345
load("Data/weights.RData")


rnames<-c("GaR-MIDAS","GaR-BMIDAS","GaR-LASSO","GaR-EN","GaR-LASSO-PCA",
          "GaR-EN-PCA","GaR-ASGL")

m <- "LASSO"

select<-c("ISPREAD","EEFR","RET","SMB","HML","MOM","VXO","CSPREAD","TERM","TED","ADS")

y<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,select]
    
GDP_real<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("q_n","date","GDP_real")]
yb<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("q_n","date","CISS")]
w<-weigths_c[[m]][,select]
GaR<-y[,1]*w[,1]+y[,2]*w[,2]+y[,3]*w[,3]+y[,4]*w[,4]+y[,5]*w[,5]+y[,6]*w[,6]+
y[,7]*w[,7]+y[,8]*w[,8]+y[,9]*w[,9]+y[,10]*w[,10]+y[,11]*w[,11]
GaR<-cbind(GDP_real,GaR,"GaRCISS"=yb$CISS)

ggplot() + geom_line(aes(y = GaR$GaR - GaR$GaRCISS, x = GaR$date))

