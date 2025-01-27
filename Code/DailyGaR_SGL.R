###########################################################################
###########################################################################
###                                                                     ###

library(stats)
library(quantreg)
#library(sROC)
library(quadprog)
library(psych)
library(forecast)
library(elasticnet)
library(glmnet)
library(readxl)            # read excel
library(tidyverse)
library(bannerCommenter) # Baners
library(bayesQR)
library(ggridges)
library(ggpubr) # ggarrrange
library(reticulate) # RUN PYTHON FROM R

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



Sbanner("Parte 2:", "Nowcasting GaR GSL", emph = TRUE)
############################################################################
############################################################################
###                                                                      ###
###                               PARTE 2:                               ###
###                         NOWCASTING GAR LASSO                         ###
###                                                                      ###
############################################################################
############################################################################

Tini=80 #2006-Q4
Tbig=length(y)
Ttau=Tbig-Tini+1  

yASGL<-data.frame(date=data[which(data$q_n>=Tini+4+1),"date"])
yASGL_GaR<-cbind(yASGL,"GaR"=NA)
yASGL_W<-cbind("GDP(-1)"=NA,"ISPREAD"=NA,"EEFR"=NA,"RET"=NA,"SMB"=NA
               ,"HML"=NA,"MOM"=NA,"VXO"=NA,
               "CSPREAD"=NA,"TERM"=NA,"TED"=NA,
               "ADS"=NA)

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


np<-import("numpy", convert = FALSE)
asgl<-import("asgl")
sk<-import("sklearn.datasets")

  
g=2 # col GDP vintage init
j=3 # col ADS vintage init

for (t in (Tini:(length(y)-1))){
#for (t in Tini){    
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

    k=2
    data_update=data[which(data$date<=q_t_1[day]),c("q_n","ISPREAD")]
    fin_1=daily_matrix(data_d=data_update, #t+4 (1 year daily lags)
                       N_lag=93,data_names=c("ISPREAD","q_n"),q_data=t+1)$daily
    fin=fin_1
    gg = c(1,rep(k,k,ncol(fin_1)))
    
    for (varname in c("EEFR","RET","SMB","HML","MOM","VXO","CSPREAD","TERM","TED")){
      k=k+1
      data_update=data[which(data$date<=q_t_1[day]),c("q_n",varname)]
      fin_1=daily_matrix(data_d=data_update, #t+4 (1 year daily lags)
                         N_lag=93,data_names=c(varname,"q_n"),q_data=t+1)$daily
      fin=cbind(fin,fin_1)
      gg=c(gg,rep(k,k,ncol(fin_1)))
    }
      
    for (varname in c("ADS")){
      while (is.na(ADS_vintages[which(ADS_vintages$date==q_t_1[day]),j])){
        j=j+1
      }
      k=k+1
      ADS_update_t_1=ADS_vintages[which(ADS_vintages$date<=q_t_1[day]),c(1,2,j)]
      names(ADS_update_t_1)=c("q_n","date","ADS")
      fin_1=daily_matrix(data_d=ADS_update_t_1, #t+4 (1 year daily lags)
                         N_lag=93,data_names=c("ADS","q_n"),q_data=t+1)$daily
      fin=cbind(fin,fin_1)
      gg=c(gg,rep(k,k,ncol(fin_1)))
    }
    
    fin=scale(fin)
    while (is.na(rGDP_vintages[t,g])){
      g=g+1
    }
    gdp=rGDP_vintages[(1:t),g]
    lgdp=scale(lrGDP_vintages[1:(t+1),g])
  
    # Numpy arrays
    XX_t = as.array(cbind(lgdp[1:t],fin[1:t,]))
    XX_t1 = as.array(cbind(lgdp,fin))
    YY_t = as.array(gdp)
    
    # Obtain weight values
    model='qr'
    penalization = 'asgl'
    weight_technique = 'pca_pct'
    lasso_power_weight = as.array( 1)
    gl_power_weight = as.array(1)
    tau = 0.10
    #variability_pct = 0.9
    
    weights = asgl$WEIGHTS(model=model, penalization=penalization, weight_technique=weight_technique, lasso_power_weight=lasso_power_weight, 
                           gl_power_weight=gl_power_weight,tau = tau)
    asgl_weigths= weights$fit(x=XX_t, y=YY_t, group_index=gg)
    lasso_weigths=asgl_weigths[[1]]
    gl_weigths=asgl_weigths[[2]]

    lambda1=0.001 #0.001
    alpha=0.25 #0.25
    
    # Fit class using optimal values
    asgl_model = asgl$ASGL(model=model, penalization='asgl',lambda1=lambda1, alpha=alpha, 
                           lasso_weights=lasso_weigths, gl_weights=gl_weigths, parallel=T,
                           tau=tau)
    
    asgl_model$fit(x = XX_t, y = YY_t, group_index = gg)
    asgl_model$coef_
    pr<-asgl_model$predict(x_new=XX_t1)
    print(pr[[1]][t+1])
    yASGL_GaR[yASGL_GaR$date==q_t_1[day],"GaR"]=pr[[1]][t+1]
    yASGL_W<-rbind(yASGL_W,t(as.matrix(gl_weigths[[1]])))
  }
}


yASGL_w<-cbind(data[data$q_n>=85,c("q_n","date")],(1/yASGL_W[-1,])/rowSums(1/yASGL_W[-1,]))
yASGL_w2<-cbind(data[data$q_n>=85,c("q_n","date")],yASGL_W[-1,])

plot(yASGL[,"GaR"])
cbind("q_n"=data$q_n[data$q_n>=85],yASGL_GaR)
yASGL<-cbind("q_n"=data$q_n[data$q_n>=85],yASGL_GaR)
GDP_real<-cbind("q_n" = seq(85,140),GDP_real[81:136,])
yASGL<-merge.data.frame(yASGL,GDP_real[,c("q_n","GDP_real")],by = "q_n")

plot(yASGL[,"GaR"],t="l")
lines(yASGL[,"GDP_real"],t="l",col=2)

write.csv(yASGL, file = paste0("Data/nowcasting_ASGL",".csv"))
write.csv(yASGL_w, file = paste0("Data/ASGL_gselect",".csv"))
write.csv(yASGL_w2, file = paste0("Data/ASGL_gselect_raw",".csv"))
