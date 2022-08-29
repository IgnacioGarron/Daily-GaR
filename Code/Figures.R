library(readxl)            # read excel
library(tidyverse)
library(ggpubr) # ggarrrange



########################################################

rm(list = ls()) # Limpiar environment
setwd("/Users/ignaciogarronvedia/Documents/GitHub/Daily-GaR")
set.seed(12345)
seed_n<-12345
load("Data/weights.RData")



recessions.df = read.table(textConnection(
  "Peak, Trough
  2008-10-01, 2009-04-01
  2020-01-01, 2020-07-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)



m<-"LASSO"
y<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,-c(1:3,14,16)]
GDP_real<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("date","GDP_real")]
yb<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("date","CISS")]
w<-weigths_c[[m]][,-1]
GaR<-y[,1]*w[,1]+y[,2]*w[,2]+y[,3]*w[,3]+y[,4]*w[,4]+y[,5]*w[,5]+y[,6]*w[,6]+
  y[,7]*w[,7]+y[,8]*w[,8]+y[,9]*w[,9]+y[,10]*w[,10]+y[,11]*w[,11]

GaR<-cbind(GDP_real,GaR)

g1<-ggplot()+geom_line(aes(y=GDP_real$GDP_real,x=as.Date(GDP_real$date),col="Quarterly GDP growth preliminary estimate (%)"),size=1)+
  geom_line(aes(y=GaR$GaR,x=as.Date(GaR$date),col="Daily combined GaR (%)"))+
  geom_line(aes(y=yb$CISS,x=as.Date(GaR$date),col="Daily GaR - CISS (%)"))+
  theme_bw() + 
  labs(x="", y="", title = "") +
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='red', alpha=0.2)+
  theme_bw() +
  geom_text(aes(x=as.Date("2018-12-01"), y=10), label="Covid-19",col="red") +
  geom_text(aes(x=as.Date("2007-12-01"), y=10), label="GFC",col="red") +
  scale_color_manual(name="",values = c("Quarterly GDP growth preliminary estimate (%)" = "orange", 
                                        "Daily combined GaR (%)" = "blue",
                                        "Daily GaR - CISS (%)" = "forestgreen"))+
  theme(legend.position="bottom")

ggsave(paste0("Figures/GaR_",m,".png"),
       ggarrange(g1,ncol = 1,nrow=1), width = 8, height = 4)




yLASSOPCA<-read.csv("Data/nowcasting_LASSOPCA.csv")[,-c(1)]

g1<-yLASSOPCA %>% pivot_longer(names_to = "Variable",values_to = "values",cols = -c(1,2,15)) %>% 
  ggplot() +
  geom_line(aes(x=as.Date(date,origin="1971-01-01"),y=values,col="Gar (10%)")) +
  geom_line(aes(x=as.Date(date,origin="1971-01-01"),y=GDP_real,col="GDP preliminary release (%)")) + 
  facet_wrap(.~Variable) + theme_bw() +
  scale_color_manual(name="",values = c("Gar (10%)" = "black", 
                                        "GDP preliminary release (%)" = "red"))+
  labs(x="", y="", title = "") +
  theme(legend.position="bottom")
  

ggsave(paste0("Figures/individual_GaR_LASSOPCA",".png"),
       ggarrange(g1,ncol = 1,nrow=1), width = 8, height = 6)


