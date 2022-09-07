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

Table1a<-data.frame(matrix(ncol=10,nrow=7))
colnames(Table1a)<-c("h_d=0","","h_d=10","",
                    "h_d=20","","h_d=40","",
                    "h_d=60","")
rnames<-c("GaR-MIDAS","GaR-BMIDAS","GaR-LASSO","GaR-EN","GaR-LASSO-PCA",
          "GaR-EN-PCA","GaR-SGLASSO")
row.names(Table1a)<-rnames

Table1b=Table1a
Table1c=Table1a
Table1d=Table1a

j=0
MM<-c("MIDAS","BMIDAS","LASSO","EN","LASSOPCA","ENPCA")
for (m in MM){
select<-c("ISPREAD","EEFR","RET","SMB","HML","MOM","VXO","CSPREAD","TERM","TED","ADS")
j=1+j
y<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,select]

GDP_real<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("q_n","date","GDP_real")]
yb<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("q_n","date","CISS")]
w<-weigths_c[[m]][,select]
GaR<-y[,1]*w[,1]+y[,2]*w[,2]+y[,3]*w[,3]+y[,4]*w[,4]+y[,5]*w[,5]+y[,6]*w[,6]+
  y[,7]*w[,7]+y[,8]*w[,8]+y[,9]*w[,9]+y[,10]*w[,10]+y[,11]*w[,11]

GaR<-cbind(GDP_real,GaR,"GaRCISS"=yb$CISS)


for (h in c(0,10,20,40,60)) {
  
  yy<-matrix(ncol=3,nrow=140-85+1)
  
  for (x in 85:140){
    h_len=length(GaR[which(GaR$q_n==x),c(2)])
    yy[x-85+1,1]=GaR[which(GaR$q_n==x),3][h_len-h]
    yy[x-85+1,2]=GaR[which(GaR$q_n==x),4][h_len-h]
    yy[x-85+1,3]=GaR[which(GaR$q_n==x),5][h_len-h]
  }
  
  if (h==0){
    l=1
  } else if (h==10) {
    l=3
  } else if (h==20) {
    l=5
  }else if (h==40) {
    l=7
  }else if (h==60) {
    l=9
  }

# TL

  tyb<-sum((yy[,1]-yy[,3])*(0.10-ifelse(yy[,1]<yy[,3],1,0)))/length(yy[,3])
  Table1b[j,l]<-round(sum((yy[,1]-yy[,2])*(0.10-ifelse(yy[,1]<yy[,2],1,0)))/length(yy[,2])/tyb,3)
  tyb<-sum((yy[1:52,1]-yy[1:52,3])*(0.10-ifelse(yy[1:52,1]<yy[1:52,3],1,0)))/length(yy[1:52,3])
  Table1a[j,l]<-round(sum((yy[1:52,1]-yy[1:52,2])*(0.10-ifelse(yy[1:52,1]<yy[1:52,2],1,0)))/length(yy[1:52,2])/tyb,3)

# KP
  Table1c[j,l]<-round(BacktestVaR(yy[,1],yy[,2], alpha = .10)$LRuc[[2]],3)
  Table1d[j,l]<-round(BacktestVaR(yy[1:52,1],yy[1:52,2], alpha = .10)$LRuc[[2]],3)

  if (h==0){
    l=2
  } else if (h==10) {
    l=4
  } else if (h==20) {
    l=6
  }else if (h==40) {
    l=8
  }else if (h==60) {
    l=10
  }
  # DM
  
  Table1b[j,l]<-round(dm.test((yy[,1]-yy[,3])*(0.10-ifelse(yy[,1]<yy[,3],1,0)),
                             (yy[,1]-yy[,2])*(0.10-ifelse(yy[,1]<yy[,2],1,0))
                             ,h=1,alternative=c("greater"),power=1)$p.value,3)
  Table1a[j,l]<-round(dm.test((yy[1:52,1]-yy[1:52,3])*(0.10-ifelse(yy[1:52,1]<yy[1:52,3],1,0)),
                             (yy[1:52,1]-yy[1:52,2])*(0.10-ifelse(yy[1:52,1]<yy[1:52,2],1,0))
                             ,h=1,alternative=c("greater"),power=1)$p.value,3)
  
  # DQ
  Table1c[j,l]<-round(BacktestVaR(yy[,1],yy[,2], alpha = .10)$DQ[[2]],3)
  Table1d[j,l]<-round(BacktestVaR(yy[1:52,1],yy[1:52,2], alpha = .10)$DQ[[2]],3)
  
  
  }
}

Table1<-rbind(Table1a,Table1b)
write_csv(Table1,"Tables/Table1.csv")

Table12<-rbind(Table1d,Table1c)
write_csv(Table12,"Tables/Table12.csv")


########################################################

########################################################

########################################################

Table2a<-data.frame(matrix(ncol=10,nrow=12))
colnames(Table2a)<-c("h_d=0","","h_d=10","",
                     "h_d=20","","h_d=40","",
                     "h_d=60","")
rnames<-c("ISPREAD","EEFR","RET","SMB","HML","MOM","VXO","CSPREAD","TERM","TED","ADS","CISS")
row.names(Table2a)<-rnames
Table2b=Table2a
Table2c=Table2a
Table2d=Table2a
j=0
MM<-c("MIDAS","BMIDAS","LASSO","EN","LASSOPCA","ENPCA")
for (m in MM){
  
  selectGaR<-c("ISPREAD","EEFR","RET","SMB","HML","MOM","VXO","CSPREAD","TERM","TED","ADS")
  select<-c("ISPREAD","EEFR","RET","SMB","HML","MOM","VXO","CSPREAD","TERM","TED","ADS","CISS")
  j=1+j
  y<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,selectGaR]
  GDP_real<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("q_n","date","GDP_real")]
  yb<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("q_n","date","CISS")]
  w<-weigths_c[[m]][,selectGaR]
  GaR<-y[,1]*w[,1]+y[,2]*w[,2]+y[,3]*w[,3]+y[,4]*w[,4]+y[,5]*w[,5]+y[,6]*w[,6]+
    y[,7]*w[,7]+y[,8]*w[,8]+y[,9]*w[,9]+y[,10]*w[,10]+y[,11]*w[,11]
  GaR<-cbind(GDP_real,GaR,"GaRCISS"=yb$CISS)
  
  y<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("q_n",select)] # select CISS
  
  for (h in c(0,10,20,40,60)) {
    
    yy<-matrix(ncol=3,nrow=140-85+1)
    ym<-matrix(ncol=12,nrow=140-85+1)
  
    for (x in 85:140){
      h_len=length(GaR[which(GaR$q_n==x),c(2)])
      yy[x-85+1,1]=GaR[which(GaR$q_n==x),3][h_len-h]
      yy[x-85+1,2]=GaR[which(GaR$q_n==x),4][h_len-h]
      yy[x-85+1,3]=GaR[which(GaR$q_n==x),5][h_len-h]
      for (k in (seq(1,12,1))){
        ym[x-85+1,k]=y[which(y$q_n==x),k+1][h_len-h]
      }
    }
    
    if (h==0){
      l=1
    } else if (h==10) {
      l=3
    } else if (h==20) {
      l=5
    }else if (h==40) {
      l=7
    }else if (h==60) {
      l=9
    }
    
    # TL
    
    for (k in (seq(1,12,1))){
      tyb<-sum((yy[,1]-yy[,2])*(0.10-ifelse(yy[,1]<yy[,2],1,0)))/length(yy[,2])
      Table2b[k,l]<-round(sum((yy[,1]-ym[,k])*(0.10-ifelse(yy[,1]<ym[,k],1,0)))/length(ym[,k])/tyb,3)
      tyb<-sum((yy[1:52,1]-yy[1:52,2])*(0.10-ifelse(yy[1:52,1]<yy[1:52,2],1,0)))/length(yy[1:52,2])
      Table2a[k,l]<-round(sum((yy[1:52,1]-ym[1:52,k])*(0.10-ifelse(yy[1:52,1]<ym[1:52,k],1,0)))/length(ym[1:52,k])/tyb,3)
      # KP
      Table2c[k,l]<-round(BacktestVaR(yy[,1],ym[,k], alpha = .10)$LRuc[[2]],3)
      Table2d[k,l]<-round(BacktestVaR(yy[1:52,1],ym[1:52,k], alpha = .10)$LRuc[[2]],3)
    }
    if (h==0){
      l=2
    } else if (h==10) {
      l=4
    } else if (h==20) {
      l=6
    }else if (h==40) {
      l=8
    }else if (h==60) {
      l=10
    }
    # DM
    for (k in (seq(1,12,1))){
      Table2b[k,l]<-round(dm.test((yy[,1]-yy[,2])*(0.10-ifelse(yy[,1]<yy[,2],1,0)),
                                  (yy[,1]-ym[,k])*(0.10-ifelse(yy[,1]<ym[,k],1,0))
                                  ,h=1,alternative=c("greater"),power=1)$p.value,3)
      Table2a[k,l]<-round(dm.test((yy[1:52,1]-yy[1:52,2])*(0.10-ifelse(yy[1:52,1]<yy[1:52,2],1,0)),
                                  (yy[1:52,1]-ym[1:52,k])*(0.10-ifelse(yy[1:52,1]<ym[1:52,k],1,0))
                                  ,h=1,alternative=c("greater"),power=1)$p.value,3)
      # DQ
      Table2c[k,l]<-round(BacktestVaR(yy[,1],ym[,k], alpha = .10)$DQ[[2]],3)
      Table2d[k,l]<-round(BacktestVaR(yy[1:52,1],ym[1:52,k], alpha = .10)$DQ[[2]],3)
      
    }  
  }
  Table2<-rbind(Table2a,Table2b)
  write_csv(Table2,paste0("Tables/Table2",m,".csv"))
  Table22<-rbind(Table2d,Table2c)
  write_csv(Table22,paste0("Tables/Table22",m,".csv"))
}
