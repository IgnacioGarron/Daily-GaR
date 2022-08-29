library(readxl)            # read excel
library(tidyverse)
library(ggpubr) # ggarrrange



########################################################

rm(list = ls()) # Limpiar environment
setwd("/Users/ignaciogarronvedia/Documents/GitHub/Daily-GaR")
set.seed(12345)
seed_n<-12345
load("Data/weights.RData")




MM<-c("MIDAS","BMIDAS","LASSO","EN","LASSOPCA","ENPCA")

for (m in MM)
m<-"LASSO"
j<-1
y<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,-c(1:3,14,16)]
GDP_real<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("q_n","date","GDP_real")]
yb<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("q_n","date","CISS")]
w<-weigths_c[[m]][,-1]
GaR<-y[,1]*w[,1]+y[,2]*w[,2]+y[,3]*w[,3]+y[,4]*w[,4]+y[,5]*w[,5]+y[,6]*w[,6]+
  y[,7]*w[,7]+y[,8]*w[,8]+y[,9]*w[,9]+y[,10]*w[,10]+y[,11]*w[,11]

GaR<-cbind(GDP_real,GaR,"GaRCISS"=yb$CISS)

Table1<-data.frame(matrix(ncol=14,nrow=7))
colnames(Table1)<-c("h_d=0","","h_d=10","",
                    "h_d=20","","h_d=40","",
                    "h_d=60","","h_d=90","","h_d=120","")
rnames<-c("GaR-MIDAS","GaR-BMIDAS","GaR-LASSO","GaR-EN","GaR-LASSO-PCA",
          "GaR-EN-PCA","GaR-SGLASSO")
row.names(Table1)<-rnames



for (h in c(20,40,60)) {
  
  yy<-matrix(ncol=5,nrow=140-85+1)
  
  for (x in 85:140){
    h_len=length(GaR[which(GaR$q_n==x),c(2)])
    yy[x-85+1,]=GaR[which(GaR$q_n==x),][h_len-h]
  }
}
  
  if (h==0){
    l=1
  } else if (h==10) {
    l=2
  } else if (h==20) {
    l=3
  }else if (h==40) {
    l=4
  }else if (h==60) {
    l=5
  }else if (h==90) {
    l=6
  }else if (h==120) {
    l=7
  }

# TL

  tyb<-sum((yy[,3]-yy[,5])*(0.10-ifelse(yy[,3]<yy[,5],1,0)))/length(yy[,3])
  Table1[j,l]<-round(sum((yy[,3]-yy[,4])*(0.10-ifelse(yy[,3]<yy[,4],1,0)))/length(yy[,4])/tyb,3)


