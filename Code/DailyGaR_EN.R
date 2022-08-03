###########################################################################
###########################################################################
###                                                                     ###

library(stats)
library(quantreg)
library(nlme)
library(grDevices)
#library(sROC)
library(quadprog)
library(psych)
library(forecast)
library(lars)
library(elasticnet)
library(glmnet)
library(normtest)
library(latex2exp)
library(readxl)            # read excel
library(tidyverse)
library(bannerCommenter) # Baners
library(GAS) # backtesting VaR
library(bayesQR)
library(fitdistrplus)      # Package to fit parametric distributions
library(sn)                # Skew-t distribution
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

banner("Parte 2:", "Nowcasting ADS", emph = TRUE)
############################################################################
############################################################################
###                                                                      ###
###                               PARTE 2:                               ###
###                            NOWCASTING ADS                            ###
###                                                                      ###
############################################################################
############################################################################

Tini=80 #2006-Q4
Tbig=length(y)
Ttau=Tbig-Tini+1  

c(ISPREAD,EEFR,RET,SMB,HML,MOM,VXO,CSPREAD,TERM,TED,CISS)

yEN<-data.frame(date=data[which(data$q_n>=Tini+4+1),"date"])
yEN<-cbind(yLASSO,"ISPREAD"=NA,"EEFR"=NA,"RET"=NA,"SMB"=NA
              ,"HML"=NA,"MOM"=NA,"VXO"=NA,
              "CSPREAD"=NA,"TERM"=NA,"TED"=NA,"CISS"=NA,
              "ADS"=NA)
EN_select<-yEN
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
  while (is.na(GDP_vintages[t,g])){
    g=g+1
  }
  GDP_real[t,2]=GDP_vintages[t,g]
}

plot(GDP_real$GDP_real,t="l")

#write.csv(ADS_real, file = paste0("Data/ADS_real",".csv"))

#### Nowcasting for ADS

g=2 # col GDP vintage init
j=3 # col ADS vintage init
#158
#for (t in (130:(length(y)-1))){
for (t in 130){  
  #for (t in (Tini:Tini)){
  
  #matching daily dates for nowcast
  q_t = data[which(data$q_n==(t+4)),"date"]
  q_t_1 = data[which(data$q_n==(t+4+1)),"date"]
  k=length(q_t_1)-length(q_t)
  if (k==1){q_t=c(q_t,q_t[length(q_t)])}
  else if (k==2){q_t=c(q_t,q_t[length(q_t)-1],q_t[length(q_t)])}
  else if (k==3){q_t=c(q_t,q_t[length(q_t)-2],q_t[length(q_t)-1],q_t[length(q_t)])}
  else if (k==-1){q_t=q_t[-length(q_t)]}
  else if (k==-2){q_t=q_t[-(length(q_t)-1):-length(q_t)]}
  print(k)
  print(q_t[length(q_t)])
  #print(length(q_t))
  print(q_t_1[length(q_t_1)])
  #print(length(q_t_1))
  print(length(q_t_1)-length(q_t))
  #matching daily dates for nowcasting
  for (day in 1:length(q_t)){
    ### Create daily matrix
    ##incorporating latest vintage
    ### Create daily matrix
    while (is.na(ADS_vintages[which(ADS_vintages$date==q_t_1[day]),j])){
      j=j+1
    }
    ADS_update_t_1=ADS_vintages[which(ADS_vintages$date<=q_t_1[day]),c(1,2,j)]
    names(ADS_update_t_1)=c("q_n","date","ADS")
    fin_1=daily_matrix(data_d=ADS_update_t_1, #t+4 (1 year daily lags)
                       N_lag=93,data_names=c("ADS","q_n"),q_data=t+1)$daily
    fin_1=scale(fin_1)
    ### Estimate regression at t
    
    # GDP vintage at t
    while (is.na(rGDP_vintages[t,g])){
      g=g+1
    }
    gdp=rGDP_vintages[(1:t),g]
    lgdp=lrGDP_vintages[1:(t+1),g]
    
    # estimate at t
    #LAMBDA 2 FOR EN
    XX=cbind(lgdp,fin_1)
    lambda2=cv.glmnet(XX[1:t,],gdp[1:(t)],alpha=0.5)$lambda.min
    I=diag(ncol(XX))*sqrt(lambda2)
    O=rep(0,ncol(XX))
    y_plus=c(gdp,O)
    x_plus=rbind(XX[1:t,],I)*(1/sqrt(1+lambda2))
    lambda1=(1/sqrt(1+lambda2_1))*lambda.BC(XX[1:t,],c = 1,alpha=0.10,tau=0.10)
    
    beta=rq(y_plus ~ x_plus, tau = 0.10,method = "lasso",lambda=lambda1)$coefficients
    beta=beta*sqrt(1+lambda2)
    II2=abs(beta[-2:-1])>10^{-6} #sam4e as Belloni and Cherno (2011)
    EN_select[EN_select$date==q_t_1[day],"ADS"]=length(fin_1[1,abs(beta[-2:-1])>10^{-6}])
    # estimate at t+1
    yEN[yEN$date==q_t_1[day],"ADS"]=c(beta[1:2],beta[-2:-1][II2])%*%c(1,lgdp[t+1],as.matrix(subset(fin_1,select=II2))[(t+1),])
    
  }
}


#prueba=rq(gdp[1:(t)]~lgdp[1:(t)]+fin_1[1:(t),], tau = 0.10,method = "lasso",lambda=(1/(sqrt(1+lambda2_1)*(t-1)))*LassoLambdaHat(XX[1:t,],alpha=0.10,tau=0.10))$coefficients



banner("Parte 3:", "Nowcasting financial indicators", emph = TRUE)
###########################################################################
###########################################################################
###                                                                     ###
###                              PARTE 3:                               ###
###                   NOWCASTING FINANCIAL INDICATORS                   ###
###                                                                     ###
###########################################################################
###########################################################################

Tini=80 #2006-Q4
Tbig=length(y)
Ttau=Tbig-Tini+1  


g=2 # col GDP vintage init
j=3 # col ADS vintage init
#for (varname in c("ISPREAD","EEFR","RET","SMB","HML","MOM","VXO","CSPREAD","TERM","TED","CISS")){

for (varname in c("TERM","TED","CISS")){
  
  for (t in (Tini:(length(y)-1))){
    
    #matching daily dates for nowcast
    q_t = data[which(data$q_n==(t+4)),"date"]
    q_t_1 = data[which(data$q_n==(t+4+1)),"date"]
    k=length(q_t_1)-length(q_t)
    if (k==1){q_t=c(q_t,q_t[length(q_t)])}
    else if (k==2){q_t=c(q_t,q_t[length(q_t)-1],q_t[length(q_t)])}
    else if (k==3){q_t=c(q_t,q_t[length(q_t)-2],q_t[length(q_t)-1],q_t[length(q_t)])}
    else if (k==-1){q_t=q_t[-length(q_t)]}
    else if (k==-2){q_t=q_t[-(length(q_t)-1):-length(q_t)]}
    print(k)
    print(q_t[length(q_t)])
    #print(length(q_t))
    print(q_t_1[length(q_t_1)])
    #print(length(q_t_1))
    print(length(q_t_1)-length(q_t))
    #matching daily dates for nowcasting
    for (day in 1:length(q_t)){
      
      data_update=data[which(data$date<=q_t_1[day]),c("q_n",varname)]
      fin_1=daily_matrix(data_d=data_update, #t+4 (1 year daily lags)
                         N_lag=93,data_names=c(varname,"q_n"),q_data=t+1)$daily
      fin_1=scale(fin_1)
      
      while (is.na(rGDP_vintages[t,g])){
        g=g+1
      }
      gdp=rGDP_vintages[(1:t),g]
      lgdp=scale(lrGDP_vintages[1:(t+1),g])

      # estimate at t
      #LAMBDA 2 FOR EN
      XX=cbind(lgdp,fin_1)
      lambda2=cv.glmnet(XX[1:t,],gdp[1:(t)],alpha=0.5)$lambda.min
      I=diag(ncol(XX))*sqrt(lambda2)
      O=rep(0,ncol(XX))
      y_plus=c(gdp,O)
      x_plus=rbind(XX[1:t,],I)*(1/sqrt(1+lambda2))
      lambda1=(1/sqrt(1+lambda2_1))*lambda.BC(XX[1:t,],c = 1,alpha=0.10,tau=0.10)
      
      beta=rq(y_plus ~ x_plus, tau = 0.10,method = "lasso",lambda=lambda1)$coefficients
      beta=beta*sqrt(1+lambda2)
      II2=abs(beta[-2:-1])>10^{-6} #sam4e as Belloni and Cherno (2011)
      EN_select[EN_select$date==q_t_1[day],varname]=length(fin_1[1,abs(beta[-2:-1])>10^{-6}])
      # estimate at t+1
      yEN[yEN$date==q_t_1[day],varname]=c(beta[1:2],beta[-2:-1][II2])%*%c(1,lgdp[t+1],as.matrix(subset(fin_1,select=II2))[(t+1),])
      
    }
  }
}


yEN<-cbind("q_n"=data$q_n[data$q_n>=85],yEN)
GDP_real<-cbind("q_n" = seq(85,140),GDP_real[81:136,])
yEN<-merge.data.frame(yEN,GDP_real[,c("q_n","GDP_real")],by = "q_n")


plot(EN_select[,"ADS"])

plot(yLASSO[,"GDP_real"],t="l")
lines(yLASSO[,"CISS"],t="l",col ="blue")



