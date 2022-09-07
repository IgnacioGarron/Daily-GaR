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
library(fbi) # fred_qd
library(viridis)
library(ggpubr)

banner("Nowcats combination", emph = TRUE)
###########################################################################
###########################################################################
###                                                                     ###
###                         NOWCATS COMBINATION                         ###
###                                                                     ###
###########################################################################
###########################################################################

########################################################

rm(list = ls()) # Limpiar environment
setwd("/Users/ignaciogarronvedia/Documents/GitHub/Daily-GaR")
set.seed(12345)
seed_n<-12345

########################################################

LASSO<-read_csv("Data/nowcasting_LASSO.csv")[,-c(1,14,16)]

weigths_c<-list()
TL<-list()

for (i in c("MIDAS","BMIDAS","LASSO","EN","LASSOPCA","ENPCA","GSLASSO")){
weigths_c[[i]]<-LASSO
TL[[i]]<-LASSO
}




MM<-c("LASSO","EN","MIDAS","BMIDAS", "LASSOPCA","ENPCA")
for (M in MM){
  Y.pred <- read.csv(paste0("Data/nowcasting_",M,".csv"))[,-c(1,14,16)]
  Y <- read.csv(paste0("Data/nowcasting_",M,".csv"), header=TRUE)[,"GDP_real"]

  ##############################################################################
  # FORECAST COMBINATION DTL
  k=2
  delta=0.9
    
  for (j in 3:13){
    TL[[M]][,j]<-(Y-Y.pred[,j])*(0.10-ifelse(Y<Y.pred[,j],1,0))
    
    for (t in 1:nrow(TL[[M]][,1])){
      if (t==1){
        e<-0.0001 # not to get NAN first value
        weigths_c[[M]][t,j]=delta^(0)*(TL[[M]][t,j]+0.0001)
      } else{
        weigths_c[[M]][t,j]=delta^(0)*TL[[M]][t,j]+delta*weigths_c[[M]][(t-1),j]
      }
    }
  }
  sum_w=rowSums(1/weigths_c[[M]][,-c(1,2)])
  weigths_c[[M]]=(1/weigths_c[[M]][,-c(1,2)])/sum_w
  weigths_c[[M]]=cbind("date"=Y.pred[,"date"],weigths_c[[M]])
}

rowSums(weigths_c[["LASSO"]][,-1])

w1<-weigths_c[["MIDAS"]] %>% pivot_longer(names_to = "Variables",
                                            values_to = "value",
                                            cols = -c(1)) %>% 
  ggplot(aes(x=as.Date(date),y=value,fill=Variables)) + geom_area()+
  scale_fill_viridis_d()+ theme_light()+
  xlab("a) MIDAS")+ylab("")

w2<-weigths_c[["BMIDAS"]] %>% pivot_longer(names_to = "Variables",
                                          values_to = "value",
                                          cols = -c(1)) %>% 
  ggplot(aes(x=as.Date(date),y=value,fill=Variables)) + geom_area()+
  scale_fill_viridis_d()+ theme_light()+
  xlab("b) BMIDAS")+ylab("")

w3<-weigths_c[["LASSO"]] %>% pivot_longer(names_to = "Variables",
                                          values_to = "value",
                                          cols = -c(1)) %>% 
  ggplot(aes(x=as.Date(date),y=value,fill=Variables)) + geom_area()+
  scale_fill_viridis_d()+ theme_light()+
  xlab("c) LASSO-Q")+ylab("")

w3i<-weigths_c[["LASSO"]] %>% pivot_longer(names_to = "Variables",
                                          values_to = "value",
                                          cols = -c(1)) %>% 
  ggplot(aes(x=as.Date(date),y=value,fill=Variables)) + geom_area()+
  scale_fill_viridis_d()+ theme_light()+
  xlab("")+ylab("")

w4<-weigths_c[["EN"]] %>% pivot_longer(names_to = "Variables",
                                          values_to = "value",
                                          cols = -c(1)) %>% 
  ggplot(aes(x=as.Date(date),y=value,fill=Variables)) + geom_area()+
  scale_fill_viridis_d()+ theme_light()+
  xlab("d) EN-Q")+ylab("")

w5<-weigths_c[["LASSOPCA"]] %>% pivot_longer(names_to = "Variables",
                                          values_to = "value",
                                          cols = -c(1)) %>% 
  ggplot(aes(x=as.Date(date),y=value,fill=Variables)) + geom_area()+
  scale_fill_viridis_d()+ theme_light()+
  xlab("e) LASSO-PCA-Q")+ylab("")

w6<-weigths_c[["ENPCA"]] %>% pivot_longer(names_to = "Variables",
                                          values_to = "value",
                                          cols = -c(1)) %>% 
  ggplot(aes(x=as.Date(date),y=value,fill=Variables)) + geom_area()+
  scale_fill_viridis_d()+ theme_light()+
  xlab("f) EN-PCA-Q")+ylab("")


ggsave(paste0("Figures/weights",".png"),
       ggarrange(w1,w2,w3,w4,w5,w6,ncol = 2,nrow=3,
                 common.legend = T,legend = "bottom"), width = 8, height = 10)

ggsave(paste0("Figures/weightslasso",".png"),
       ggarrange(w3i,ncol = 1,nrow=1,
                 common.legend = T,legend = "bottom"), width = 8, height = 4)



save(weigths_c,file = "Data/weights.RData")

save(TL,file = "Data/tickloss.RData")


