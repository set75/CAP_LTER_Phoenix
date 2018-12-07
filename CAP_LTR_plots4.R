# CAP LTER plots

rm(list=ls())
setwd("~/Documents/CAP_LTER_flux_data")
load("new_data4.RData")

library(ggplot2)
library(dplyr)
library(RColorBrewer)

# truncate EF
EF <- new_data$LE/(new_data$LE+new_data$SH)
l <- length(EF)
for(i in 1:l){
  if(EF[i] < 0){EF[i] = 0}
  if(EF[i] > 1){EF[i] = 1}
}

new_data$EF <- EF
df.rain<-new_data[new_data$pm_rain==1,]

# 1D histogram with Rain scatter overlay
ggplot(data=new_data, aes(x=vapor))+
  geom_density()+
  geom_point(data=df.rain, aes(x=vapor, y=0, col="red"))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Total water vapor")
