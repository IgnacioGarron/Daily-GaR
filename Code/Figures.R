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

yASGL<-read.csv(paste0("Data/nowcasting_ASGL.csv"))
GaR<-cbind(GDP_real,GaR,"ASGL"=yASGL$GaR)

g1<-ggplot()+geom_line(aes(y=GDP_real$GDP_real,x=as.Date(GDP_real$date),
                           col="GDP growth preliminary estimate (%)"),
                       size=1)+
  geom_line(aes(y=GaR$GaR,x=as.Date(GaR$date),col="Combined GaR-LASSO (%)"),size=0.5)+
  geom_line(aes(y=yb$CISS,x=as.Date(GaR$date),col="GaR CISS-LASSO (%)"),size=0.5,linetype = "twodash")+
  geom_line(aes(y=yASGL$GaR,x=as.Date(GaR$date),col="GaR-ASGL (%)"),size=0.5,linetype = "dashed")+
  labs(x="", y="", title = "a. GaR(10%) nowcasts") +
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='darkred', alpha=0.2)+
  theme_linedraw() +
  geom_text(aes(x=as.Date("2018-12-01"), y=10), label="Covid-19",col="black") +
  geom_text(aes(x=as.Date("2007-12-01"), y=10), label="GFC",col="black") +
  scale_color_manual(name="",values = c("GDP growth preliminary estimate (%)" = "orange", 
                                        "Combined GaR-LASSO (%)" = "blue",
                                        "GaR CISS-LASSO (%)" = "forestgreen",
                                        "GaR-ASGL (%)" = "RED"))


ggsave(paste0("Figures/GaR_",m,".jpg"),
       ggarrange(g1,ncol = 1,nrow=1,
                 common.legend = T,legend = "bottom"), width = 8, height = 4)




g2<-weigths_c[["LASSO"]] %>% pivot_longer(names_to = "Variables",
                                          values_to = "value",
                                          cols = -c(1)) %>% 
  ggplot(aes(x=as.Date(date),y=value,fill=Variables)) + geom_area()+
  scale_fill_viridis_d()+ theme_linedraw()+
  labs(x="", y="", title = "b. Daily combination weights for LASSO-Q") +
  xlab("")+ylab("")

w<-read.csv(paste0("Data/ASGL_gselect.csv"))[,-1:-2]

names(w)<-c("date","GDP growth (t-1)","ISPREAD" ,"EEFR", "RET",
            "SMB","HML", "MOM","VXO","CSPREAD","TERM","TED","ADS" )

g3<-w[,-c(2)] %>% pivot_longer(names_to = "Variables",
                                           values_to = "value",
                                           cols = -c(1)) %>% 
  ggplot(aes(x=as.Date(date),y=value,fill=Variables)) + geom_area()+
  scale_fill_viridis_d()+ theme_linedraw()+
  labs(x="", y="", title = "c. Daily combination weights for ASGL-Q") +
  xlab("")+ylab("")

ggsave(paste0("Figures/weightsASGL",".jpg"),
       ggarrange(g3,ncol = 1,nrow=1,
                 common.legend = T,legend = "bottom"), width = 8, height = 4)


# Importance around GFC
apply(weigths_c$LASSO[weigths_c$LASSO$date>="2008-10-01" &
                        weigths_c$LASSO$date<= "2009-04-01",-1],2,mean)


ggsave(paste0("Figures/Figure1",".jpg"),
       ggarrange(g1,g2,g3,ncol = 1,nrow=3,
                 common.legend = F,legend = "bottom"), width = 8, height = 10)



################################################################################

# Alternative figures



m<-"MIDAS"
  
y<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,-c(1:3,14,16)]
GDP_real<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("date","GDP_real")]
ybMIDAS<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("date","CISS")]
w<-weigths_c[[m]][,-1]
GaRMIDAS<-y[,1]*w[,1]+y[,2]*w[,2]+y[,3]*w[,3]+y[,4]*w[,4]+y[,5]*w[,5]+y[,6]*w[,6]+
  y[,7]*w[,7]+y[,8]*w[,8]+y[,9]*w[,9]+y[,10]*w[,10]+y[,11]*w[,11]

g1<-ggplot()+geom_line(aes(y=GDP_real$GDP_real,x=as.Date(GDP_real$date),
                           col="GDP growth preliminary estimate (%)"),
                       size=1)+
  geom_line(aes(y=GaRMIDAS,x=as.Date(GDP_real$date),col="Combined GaR (%)"),size=0.5)+
  geom_line(aes(y=ybMIDAS$CISS,x=as.Date(GDP_real$date),col="GaR-CISS (%)"),size=0.5,linetype = "twodash")+
  labs(x="", y="", title = "a. GaR(10%) MIDAS-Q nowcasts") +
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='darkred', alpha=0.2)+
  theme_linedraw() +
  geom_text(aes(x=as.Date("2018-12-01"), y=10), label="Covid-19",col="black") +
  geom_text(aes(x=as.Date("2007-12-01"), y=10), label="GFC",col="black") +
  scale_color_manual(name="",values = c("GDP growth preliminary estimate (%)" = "orange", 
                                        "Combined GaR (%)" = "blue",
                                        "GaR-CISS (%)" = "forestgreen"))

m<-"BMIDAS"

y<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,-c(1:3,14,16)]
GDP_real<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("date","GDP_real")]
ybBMIDAS<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("date","CISS")]
w<-weigths_c[[m]][,-1]
GaRBMIDAS<-y[,1]*w[,1]+y[,2]*w[,2]+y[,3]*w[,3]+y[,4]*w[,4]+y[,5]*w[,5]+y[,6]*w[,6]+
  y[,7]*w[,7]+y[,8]*w[,8]+y[,9]*w[,9]+y[,10]*w[,10]+y[,11]*w[,11]

g2<-ggplot()+geom_line(aes(y=GDP_real$GDP_real,x=as.Date(GDP_real$date),
                           col="GDP growth preliminary estimate (%)"),
                       size=1)+
  geom_line(aes(y=GaRBMIDAS,x=as.Date(GDP_real$date),col="Combined GaR (%)"),size=0.5)+
  geom_line(aes(y=ybBMIDAS$CISS,x=as.Date(GDP_real$date),col="GaR-CISS (%)"),size=0.5,linetype = "twodash")+
  labs(x="", y="", title = "b. GaR(10%) BMIDAS-Q nowcasts") +
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='darkred', alpha=0.2)+
  theme_linedraw() +
  geom_text(aes(x=as.Date("2018-12-01"), y=10), label="Covid-19",col="black") +
  geom_text(aes(x=as.Date("2007-12-01"), y=10), label="GFC",col="black") +
  scale_color_manual(name="",values = c("GDP growth preliminary estimate (%)" = "orange", 
                                        "Combined GaR (%)" = "blue",
                                        "GaR-CISS (%)" = "forestgreen"))

m<-"EN"

y<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,-c(1:3,14,16)]
GDP_real<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("date","GDP_real")]
ybEN<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("date","CISS")]
w<-weigths_c[[m]][,-1]
GaREN<-y[,1]*w[,1]+y[,2]*w[,2]+y[,3]*w[,3]+y[,4]*w[,4]+y[,5]*w[,5]+y[,6]*w[,6]+
  y[,7]*w[,7]+y[,8]*w[,8]+y[,9]*w[,9]+y[,10]*w[,10]+y[,11]*w[,11]

g3<-ggplot()+geom_line(aes(y=GDP_real$GDP_real,x=as.Date(GDP_real$date),
                           col="GDP growth preliminary estimate (%)"),
                       size=1)+
  geom_line(aes(y=GaREN,x=as.Date(GDP_real$date),col="Combined GaR (%)"),size=0.5)+
  geom_line(aes(y=ybEN$CISS,x=as.Date(GDP_real$date),col="GaR-CISS (%)"),size=0.5,linetype = "twodash")+
  labs(x="", y="", title = "c. GaR(10%) EN-Q nowcasts") +
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='darkred', alpha=0.2)+
  theme_linedraw() +
  geom_text(aes(x=as.Date("2018-12-01"), y=10), label="Covid-19",col="black") +
  geom_text(aes(x=as.Date("2007-12-01"), y=10), label="GFC",col="black") +
  scale_color_manual(name="",values = c("GDP growth preliminary estimate (%)" = "orange", 
                                        "Combined GaR (%)" = "blue",
                                        "GaR-CISS (%)" = "forestgreen"))

m<-"LASSOPCA"

y<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,-c(1:3,14,16)]
GDP_real<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("date","GDP_real")]
ybLASSOPCA<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("date","CISS")]
w<-weigths_c[[m]][,-1]
GaRLASSOPCA<-y[,1]*w[,1]+y[,2]*w[,2]+y[,3]*w[,3]+y[,4]*w[,4]+y[,5]*w[,5]+y[,6]*w[,6]+
  y[,7]*w[,7]+y[,8]*w[,8]+y[,9]*w[,9]+y[,10]*w[,10]+y[,11]*w[,11]

g4<-ggplot()+geom_line(aes(y=GDP_real$GDP_real,x=as.Date(GDP_real$date),
                           col="GDP growth preliminary estimate (%)"),
                       size=1)+
  geom_line(aes(y=GaRLASSOPCA,x=as.Date(GDP_real$date),col="Combined GaR (%)"),size=0.5)+
  geom_line(aes(y=ybLASSOPCA$CISS,x=as.Date(GDP_real$date),col="GaR-CISS (%)"),size=0.5,linetype = "twodash")+
  labs(x="", y="", title = "d. GaR(10%) LASSO-PCA-Q nowcasts") +
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='darkred', alpha=0.2)+
  theme_linedraw() +
  geom_text(aes(x=as.Date("2018-12-01"), y=10), label="Covid-19",col="black") +
  geom_text(aes(x=as.Date("2007-12-01"), y=10), label="GFC",col="black") +
  scale_color_manual(name="",values = c("GDP growth preliminary estimate (%)" = "orange", 
                                        "Combined GaR (%)" = "blue",
                                        "GaR-CISS (%)" = "forestgreen"))

m<-"ENPCA"

y<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,-c(1:3,14,16)]
GDP_real<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("date","GDP_real")]
ybENPCA<-read.csv(paste0("Data/nowcasting_",m,".csv"))[,c("date","CISS")]
w<-weigths_c[[m]][,-1]
GaRENPCA<-y[,1]*w[,1]+y[,2]*w[,2]+y[,3]*w[,3]+y[,4]*w[,4]+y[,5]*w[,5]+y[,6]*w[,6]+
  y[,7]*w[,7]+y[,8]*w[,8]+y[,9]*w[,9]+y[,10]*w[,10]+y[,11]*w[,11]

g5<-ggplot()+geom_line(aes(y=GDP_real$GDP_real,x=as.Date(GDP_real$date),
                           col="GDP growth preliminary estimate (%)"),
                       size=1)+
  geom_line(aes(y=GaRENPCA,x=as.Date(GDP_real$date),col="Combined GaR (%)"),size=0.5)+
  geom_line(aes(y=ybENPCA$CISS,x=as.Date(GDP_real$date),col="GaR-CISS (%)"),size=0.5,linetype = "twodash")+
  labs(x="", y="", title = "e. GaR(10%) EN-PCA-Q nowcasts") +
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='darkred', alpha=0.2)+
  theme_linedraw() +
  geom_text(aes(x=as.Date("2018-12-01"), y=10), label="Covid-19",col="black") +
  geom_text(aes(x=as.Date("2007-12-01"), y=10), label="GFC",col="black") +
  scale_color_manual(name="",values = c("GDP growth preliminary estimate (%)" = "orange", 
                                        "Combined GaR (%)" = "blue",
                                        "GaR-CISS (%)" = "forestgreen"))


ggsave(paste0("Figures/FigureA3",".jpg"),
       ggarrange(g1,g2,g3,g4,g5,ncol = 1,nrow=5,
                 common.legend = T,legend = "bottom"), width = 8, height = 10)


