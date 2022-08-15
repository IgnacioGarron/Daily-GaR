###########################################################################
###########################################################################
###                                                                     ###

library(stats)
library(quantreg)
library(grDevices)
#library(sROC)
library(quadprog)
library(psych)
library(forecast)
library(elasticnet)
library(glmnet)
library(normtest)
library(readxl)            # read excel
library(tidyverse)
library(bannerCommenter) # Baners
library(GAS) # backtesting VaR
library(bayesQR)
library(ggridges)
library(ggpubr) # ggarrrange

banner("Parte 1:", "Procesamiento de la base de datos", emph = TRUE)
###########################################################################
###########################################################################
###                                                                     ###
###                              PARTE 1:                               ###
###                  PROCESAMIENTO DE LA BASE DE DATOS                  ###
###                                                                     ###
###########################################################################
###########################################################################

########################################################

rm(list = ls()) # Limpiar environment
setwd("/Users/ignaciogarronvedia/Documents/GitHub/Daily-GaR")
set.seed(12345)
seed_n<-12345

source("Code/almon_lag.R") # Almon Lag with two end point restrictions as in Mogliani and Simoni (2021)
source("Code/dailymatrix.R") # Almon Lag with two end point restrictions as in Mogliani and Simoni (2021)
source("Code/lambda_lasso.R") # Lambda lasso
source("Code/eigenratio.R") #Ahn and Horenstein (2013).

####### Quarterly GDP Data Import #######

data = readxl::read_excel("Data/Data.xlsx", sheet = "BASE 2")
data <- data.frame(data)
data$date <- as.Date(data[,"date"])

######## Date #######

date<-data[complete.cases(data$date,data$GDP),c("date")][-4:-1]
data[complete.cases(data$date,data$GDP),c("q_n")]

# GDP vintages

GDP_vintages = readxl::read_excel("Data/ROUTPUTQvQd.xlsx")

rGDP_vintages = GDP_vintages %>% pivot_longer( names_to= "variable",
                                               values_to = "val", cols = -c("DATE")) %>% 
  dplyr::group_by(variable) %>% 
  mutate(val=log(val/lag(val,1))*100) %>% 
  pivot_wider(names_from = "variable",values_from = "val")

lrGDP_vintages = GDP_vintages %>% pivot_longer( names_to= "variable",
                                                values_to = "val", cols = -c("DATE")) %>% 
  dplyr::group_by(variable) %>% 
  mutate(val=log(val/lag(val,1)*100)) %>% 
  dplyr::group_by(variable) %>% 
  mutate(val=lag(val,1)) %>% 
  pivot_wider(names_from = "variable",values_from = "val")

rGDP_vintages = rGDP_vintages[161:296,c(89:145)] 
rGDP_vintages = cbind(date,rGDP_vintages)

lrGDP_vintages = lrGDP_vintages[161:296,c(89:145)] 
lrGDP_vintages = cbind(date,lrGDP_vintages)

# GDP latest vintage
y=as.matrix(data$GDP)
y=y[complete.cases(y)] 
y=y[-4:-1] # 136 trimestres

# ADS vintages

ADS_vintages = readxl::read_excel("Data/ads_all_vintages-zip.xlsx")
ADS_vintages = ADS_vintages[,-1]
ADS_vintages=cbind("date"=data$date,ADS_vintages)
ADS_vintages=cbind("q_n"=data$q_n,ADS_vintages)
str(ADS_vintages)



banner("Parte 2:", "Nowcasting GaR LASSOPCA", emph = TRUE)
############################################################################
############################################################################
###                                                                      ###
###                               PARTE 2:                               ###
###                         NOWCASTING GAR LASSOPCA                      ###
###                                                                      ###
############################################################################
############################################################################

Tini=80 #2006-Q4
Tbig=length(y)
Ttau=Tbig-Tini+1  


yLASSOPCA<-data.frame(date=data[which(data$q_n>=Tini+4+1),"date"])
yLASSOPCA<-cbind(yLASSOPCA,"ISPREAD"=NA,"EEFR"=NA,"RET"=NA,"SMB"=NA
              ,"HML"=NA,"MOM"=NA,"VXO"=NA,
              "CSPREAD"=NA,"TERM"=NA,"TED"=NA,"CISS"=NA,
              "ADS"=NA)
LASSOPCA_select<-yLASSOPCA

ADS_real<-data.frame(date=data[,"date"])
ADS_real<-cbind(ADS_real,"ADS_real"=NA)
GDP_real<-data.frame(date=date)
GDP_real<-cbind(GDP_real,"GDP_real"=NA)


#### Real time ADS

j=3
for (t in (1:(length(y)+4))){   
  ##incorporating latest vintage
  for (i in data[which(data$q_n==(t)),"date"]){
    ### Create daily matrix
    while (is.na(ADS_vintages[which(ADS_vintages$date==as.Date(i,origin="1970-1-1")),j])){
      j=j+1
    }
    ADS_real[which(ADS_real$date==as.Date(i,origin="1970-1-1")),2]=ADS_vintages[which(ADS_vintages$date==as.Date(i,origin="1970-1-1")),j]
  }
}

#### Real time GDP

g=2
for (t in (1:length(y))){   
  ##incorporating latest vintage of GDP
  while (is.na(rGDP_vintages[t,g])){
    g=g+1
  }
  GDP_real[t,2]=rGDP_vintages[t,g]
}

plot(GDP_real$GDP_real,t="l")



for (varname in c("ISPREAD","EEFR","RET","SMB","HML","MOM","VXO","CSPREAD","TERM","TED","CISS","ADS")){
#for (varname in c("ADS")){
  
  g=2 # col GDP vintage init
  j=3 # col ADS vintage init
  
  for (t in (Tini:(length(y)-1))){
    
    #matching daily dates for nowcast
    q_t = data[which(data$q_n==(t+4)),"date"]
    q_t_1 = data[which(data$q_n==(t+4+1)),"date"]
    k=length(q_t_1)-length(q_t)
    if (k==1){q_t=c(q_t,q_t[length(q_t)])
    }else if (k==2){q_t=c(q_t,q_t[length(q_t)-1],q_t[length(q_t)])
    }else if (k==3){q_t=c(q_t,q_t[length(q_t)-2],q_t[length(q_t)-1],q_t[length(q_t)])
    }else if (k==-1){q_t=q_t[-length(q_t)]
    }else if (k==-2){q_t=q_t[-(length(q_t)-1):-length(q_t)]}
    print(k)
    print(q_t[length(q_t)])
    #print(length(q_t))
    print(q_t_1[length(q_t_1)])
    #print(length(q_t_1))
    print(length(q_t_1)-length(q_t))
    #matching daily dates for nowcasting
    for (day in 1:length(q_t)){
      
      if (varname=="ADS"){
        while (is.na(ADS_vintages[which(ADS_vintages$date==q_t_1[day]),j])){
          j=j+1
        }
        ADS_update_t_1=ADS_vintages[which(ADS_vintages$date<=q_t_1[day]),c(1,2,j)]
        names(ADS_update_t_1)=c("q_n","date","ADS")
        fin_1=daily_matrix(data_d=ADS_update_t_1, #t+4 (1 year daily lags)
                           N_lag=93,data_names=c("ADS","q_n"),q_data=t+1)$daily
        fin_1=scale(fin_1)
      } else {
        data_update=data[which(data$date<=q_t_1[day]),c("q_n",varname)]
        fin_1=daily_matrix(data_d=data_update, #t+4 (1 year daily lags)
                           N_lag=93,data_names=c(varname,"q_n"),q_data=t+1)$daily
        fin_1=scale(fin_1)
      }
      
      while (is.na(rGDP_vintages[t,g])){
        g=g+1
      }
      gdp=rGDP_vintages[(1:t),g]
      lgdp=scale(lrGDP_vintages[1:(t+1),g])
      
      XX=cbind(rep(1,length(gdp)),fin_1[1:(t),])
      XX=fin_1[1:(t),]
      
      # estimate at t penalized LASSO
      lambdalasso=lambda.BC(fin_1[1:t,],c = 1,alpha=0.10,tau=0.10)
      beta_lasso=rqss(gdp~XX, tau = 0.10,method = "lasso",lambda=lambdalasso)$coefficients
      Lasso_I=abs(beta_lasso[-1])>10^{-6}
      if (length(fin_1[1,abs(beta_lasso[-1])>10^{-6}])==0){
        Lasso_I=rank(abs(beta_lasso[-1]))<2 
        beta=rq(gdp[1:(t)]~lgdp[1:t]+as.matrix(subset(fin_1[1:t,],select=Lasso_I)), tau = 0.10)$coefficients
        n_1=0
        # estimate at t+1
        yLASSOPCA[yLASSOPCA$date==q_t_1[day],varname]=beta%*%c(1,lgdp[t+1],as.matrix(subset(fin_1,select=Lasso_I))[(t+1),])
      } else if (length(fin_1[1,abs(beta_lasso[-1])>10^{-6}])==1){
        n_1=0
        # post-estimation LASSO
        beta=rq(gdp[1:(t)]~lgdp[1:t]+as.matrix(subset(fin_1[1:t,],select=Lasso_I)), tau = 0.10)$coefficients
        # estimate at t+1
        yLASSOPCA[yLASSOPCA$date==q_t_1[day],varname]=beta%*%c(1,lgdp[t+1],as.matrix(subset(fin_1,select=Lasso_I))[(t+1),])
      } else if (length(fin_1[1,abs(beta_lasso[-1])>10^{-6}])>1){
        # Select number of PCA according to eigenratio
        n_1<-numb_factors(fin_1[,Lasso_I],kmax=ncol(as.matrix(fin_1[,Lasso_I])))
        pc1<-prcomp(fin_1[,Lasso_I], retx = TRUE, center = FALSE, scale=FALSE,rank. = n_1 )$x
        if (n_1==1){
          # mismo que Lima and Godeiro 2020
          beta=rq(gdp[1:t]~lgdp[1:t]+pc1[1:t],tau = 0.10)$coefficients 
          yLASSOPCA[yLASSOPCA$date==q_t_1[day],varname]=beta%*%c(1,lgdp[t+1],pc1[t+1])
        } else {
          eq=summary(rq(gdp[1:t]~lgdp[1:t]+pc1[1:t,],tau = 0.10),se = "ker",cov=TRUE)
          I=eq$coef[3:nrow(eq$coef),4]<0.01 # p<0.01 hard threshold
          if (sum(I)==0){
            I=rank(eq$coef[3:nrow(eq$coef),4])<2 # si no se cumple condicion, nos quedamos con el x m?s significativo
          }
          pc1_t<-pc1[1:t,which(I)]
          pc1_t1<-pc1[(t+1),which(I)]
          beta=rq(gdp[1:t]~lgdp[1:t]+pc1_t,tau = 0.10)$coefficient
          yLASSOPCA[yLASSOPCA$date==q_t_1[day],varname]=beta%*%c(1,lgdp[t+1],pc1_t1)
        }
      }
      LASSOPCA_select[LASSOPCA_select$date==q_t_1[day],varname]=n_1
    }
  }
}



yLASSOPCA<-cbind("q_n"=data$q_n[data$q_n>=85],yLASSOPCA)
GDP_real<-cbind("q_n" = seq(85,140),GDP_real[81:136,])
yLASSOPCA<-merge.data.frame(yLASSOPCA,GDP_real[,c("q_n","GDP_real")],by = "q_n")

plot(yLASSOPCA[,"ADS"],t="l")
lines(yLASSOPCA[,"GDP_real"],t="l",col=2)

write.csv(yLASSOPCA, file = paste0("Data/nowcasting_LASSOPCA",".csv"))
write.csv(LASSOPCA_select, file = paste0("Data/LASSOPCA_select",".csv"))

