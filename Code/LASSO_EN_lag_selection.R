library(readxl)            # read excel
library(tidyverse)


rm(list = ls()) # Limpiar environment
setwd("/Users/ignaciogarronvedia/Documents/GitHub/Daily-GaR")
set.seed(12345)
seed_n<-12345

############### LASSSO and EN lag selection ########################



plot<-read_csv("Data/EN_select.csv")

summary(plot[,-c(1,2,3)])

plot[,-c(1)] %>% pivot_longer(names_to = "Variables",values_to = "val",cols = -c(date)) %>% 
  ggplot(aes(x = date, y =val,col=Variables )) + geom_line() + facet_wrap(~Variables)+
  theme_bw() + 
  labs(x="", y="", title = "") + theme(legend.position="bottom")
