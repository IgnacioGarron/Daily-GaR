library(readxl)            # read excel
library(tidyverse)
library(ggpubr) # ggarrrange


rm(list = ls()) # Limpiar environment
setwd("/Users/ignaciogarronvedia/Documents/GitHub/Daily-GaR")
set.seed(12345)
seed_n<-12345

############### LASSSO and EN lag selection ########################


load("Data/lasso_lags.RData")
load("Data/EN_lags.RData")

plot<-read_csv("Data/EN_select.csv")

lagplot<-function(varname,data){
g=data[[paste0(varname)]] %>% ggplot(aes(x=as.Date(date,origin="1971-01-01"),y=lag))+
                                     geom_point() + theme_bw() + 
              labs(x="", y="Lags", title = paste0(varname)) + theme(legend.position="bottom")
return(g)
}

g1<-lagplot(varname="ISPREAD",data=lasso_lags)
g2<-lagplot(varname="EEFR",data=lasso_lags)
g3<-lagplot(varname="RET",data=lasso_lags)
g4<-lagplot(varname="SMB",data=lasso_lags)
g5<-lagplot(varname="HML",data=lasso_lags)
g6<-lagplot(varname="MOM",data=lasso_lags)
g7<-lagplot(varname="VXO",data=lasso_lags)
g8<-lagplot(varname="CSPREAD",data=lasso_lags)
g9<-lagplot(varname="TERM",data=lasso_lags)
g10<-lagplot(varname="TED",data=lasso_lags)
g11<-lagplot(varname="CISS",data=lasso_lags)
g12<-lagplot(varname="ADS",data=lasso_lags)


ggsave(paste0("Figures/LASSO_lags",".jpg"),
       ggarrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,ncol = 2,nrow=6), width = 8, height = 10)

g1<-lagplot(varname="ISPREAD",data=EN_lags)
g2<-lagplot(varname="EEFR",data=EN_lags)
g3<-lagplot(varname="RET",data=EN_lags)
g4<-lagplot(varname="SMB",data=EN_lags)
g5<-lagplot(varname="HML",data=EN_lags)
g6<-lagplot(varname="MOM",data=EN_lags)
g7<-lagplot(varname="VXO",data=EN_lags)
g8<-lagplot(varname="CSPREAD",data=EN_lags)
g9<-lagplot(varname="TERM",data=EN_lags)
g10<-lagplot(varname="TED",data=EN_lags)
g11<-lagplot(varname="CISS",data=EN_lags)
g12<-lagplot(varname="ADS",data=EN_lags)


ggsave(paste0("Figures/EN_lags",".jpg"),
       ggarrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,ncol = 2,nrow=6), width = 8, height = 10)

