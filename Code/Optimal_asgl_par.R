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
            "CSPREAD"=NA,"TERM"=NA,"TED"=NA,"CISS"=NA,
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

j=1
data_update=data[,c("q_n","ISPREAD")]
fin_1=daily_matrix(data_d=data_update, #t+4 (1 year daily lags)
                   N_lag=93,data_names=c("ISPREAD","q_n"),q_data=136)$daily
fin=fin_1
gg = c(rep(j,j,ncol(fin_1)))

for (varname in c("EEFR","RET","SMB","HML","MOM","VXO","CSPREAD","TERM","TED","ADS")){
  j=j+1
  data_update=data[,c("q_n",varname)]
  fin_1=daily_matrix(data_d=data_update, #t+4 (1 year daily lags)
                     N_lag=93,data_names=c(varname,"q_n"),q_data=136)$daily
  fin=cbind(fin,fin_1)
  gg=c(gg,rep(j,j,ncol(fin_1)))
}


# Numpy arrays
XX = as.array(scale(fin))
YY = as.array(GDP_real[,2])

class(XX)
class(YY)
class(gg)


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
asgl_weigths= weights$fit(x=XX, y=YY, group_index=gg)

lasso_weigths=asgl_weigths[[1]]
gl_weigths=asgl_weigths[[2]]

# Define model parameters for CV
model = 'qr'  # linear model
penalization = 'asgl'  # sparse group lasso penalization
parallel = T  # Code executed in parallel
error_type = 'QRE'  # Error measuremente considered. MSE stands for Mean Squared Error.
tau = 0.10

lambda1 = as.array(c(0.001, 0.01, 0.1, 1, 10)) # 4 possible values for lambda
alpha = as.array(0.25, 0.5, 0.75, 1) # 20 possible values for alpha

# Define a cross validation object
cv_class <- asgl$CV(model=model, penalization=penalization, 
                    lambda1=lambda1, alpha=alpha, error_type=error_type, 
                    parallel=parallel, random_state=np_array(99,dtype = "int64"),
                    weight_technique = "pls_pct",tau=tau)


# Compute error using k-fold cross validation
error<-cv_class$cross_validation(x=XX, y=YY, group_index=gg)


# Obtain the mean error across different folds
error1 = rowMeans(error)
# Select the minimum error
minimum_error_idx = np$argmin(error1)

# Select the parameters associated to mininum error values
optimal_parameters = cv_class$retrieve_parameters_value(minimum_error_idx)

cv_class$retrieve_parameters_value(2)
lambda1=optimal_parameters$lambda1 
alpha=optimal_parameters$alpha

lambda #0.001
alpha #0.25

# Fit class using optimal values
asgl_model = asgl$ASGL(model=model, penalization='asgl',lambda1=lambda1, alpha=alpha, 
                       lasso_weights=lasso_weigths, gl_weights=gl_weigths, parallel=T,
                       tau=tau)

asgl_model$fit(x = XX, y = YY, group_index = gg)
asgl_model$coef_
pr<-asgl_model$predict(x_new=XX)
plot(YY)
lines(pr[[1]],t="l",col=2)

