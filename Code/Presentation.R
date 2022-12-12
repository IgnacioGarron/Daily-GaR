library(readxl)            # read excel
library(tidyverse)
library(ggpubr) # ggarrrange



########################################################

rm(list = ls()) # Limpiar environment
setwd("/Users/ignaciogarronvedia/Documents/GitHub/Daily-GaR")

####### Quarterly GDP Data Import #######

data = readxl::read_excel("Data/Data.xlsx", sheet = "BASE 2")
data <- data.frame(data)
data$date <- as.Date(data[,"date"])

GDP_real<-read.csv(paste0("Data/nowcasting_LASSO.csv"))[,c("date","GDP_real")]

data<-cbind(data[data$date>="2007-01-01",],GDP_real[,-1])

recessions.df = read.table(textConnection(
  "Peak, Trough
  2008-10-01, 2009-04-01
  2020-01-01, 2020-07-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)


gg1<- data %>% mutate(VXO=scale(VXO),CSPREAD=scale(CSPREAD),
                ADS=scale(ADS),CISS=scale(CISS)) %>%  ggplot()+
  geom_line(aes(y=`GDP_real[, -1]`,x=as.Date(date),
                       col="Quarterly GDP growth preliminary estimate (%)"),
                   size=1,yaxis=2)+
  geom_line(aes(y=CISS,x=as.Date(date),col="CISS (scaled)"),size=0.5)+
  geom_line(aes(y=VXO,x=as.Date(date),col="VXO (scaled)"),size=0.5)+
  geom_line(aes(y=CSPREAD,x=as.Date(date),col="Credit spreads (scaled)"),size=0.5,linetype = "twodash")+
  theme_bw() + 
  labs(x="", y="", title = "") +
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='gray', alpha=0.2)+
  theme_bw() +
  geom_text(aes(x=as.Date("2018-12-01"), y=10), label="Covid-19",col="black") +
  geom_text(aes(x=as.Date("2007-12-01"), y=10), label="GFC",col="black") +
  scale_color_manual(name="",values = c("Quarterly GDP growth preliminary estimate (%)" = "orange", 
                                        "VXO (scaled)" = "blue",
                                        "CISS (scaled)" = "red",
                                        "Credit spreads (scaled)" = "forestgreen"))

gg2<- data %>% mutate(VXO=scale(VXO),CSPREAD=scale(CSPREAD),
                     ADS=scale(ADS)) %>%  ggplot()+
  geom_line(aes(y=`GDP_real[, -1]`,x=as.Date(date),
                col="Quarterly GDP growth preliminary estimate (%)"),
            size=1,yaxis=2)+
  geom_line(aes(y=ADS,x=as.Date(date),col="ADS index (scaled)"),size=0.5,linetype = "dashed")+
  theme_bw() + 
  labs(x="", y="", title = "") +
  geom_rect(data = recessions.df, inherit.aes=F, 
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='gray', alpha=0.2)+
  theme_bw() +
  geom_text(aes(x=as.Date("2018-12-01"), y=10), label="Covid-19",col="black") +
  geom_text(aes(x=as.Date("2007-12-01"), y=10), label="GFC",col="black") +
  scale_color_manual(name="",values = c("Quarterly GDP growth preliminary estimate (%)" = "orange", 
                                        "ADS index (scaled)" = "purple"))

ggsave(paste0("Figures/Presentation1.jpg"),
       ggarrange(gg1,ncol = 1,nrow=1,
                 common.legend = T,legend = "bottom"), width = 8, height = 4)


ggsave(paste0("Figures/Presentation2.jpg"),
       ggarrange(gg2,ncol = 1,nrow=1,
                 common.legend = T,legend = "bottom"), width = 8, height = 4)


