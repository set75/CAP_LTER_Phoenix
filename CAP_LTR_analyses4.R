# CAP_LTR flux data view morning averages & reseize / recenter data 
# Beginning w/ new_data.RData generated from CSFR_format_data.R

rm(list=ls())
setwd("~/Documents/CAP_LTER_flux_data")
load("new_data4.RData")

# Data -------------------------------------------------------------------
set.seed(27) # for reproducibility
len <- length(new_data$EF)
set <- sample.int(n=len, size=len) # randomized sampling
# read in variables
Rain <- new_data$pm_rain[set]
previous.rain <- c(0,new_data$pm_rain[1:(len-1)])
Vap <- new_data$vapor[set]
SH <- new_data$SH[set]
LE <- new_data$LE[set]
Temp <- new_data$Temp[set]
Uz <- new_data$Uz[set]
EF <- LE/(LE+SH)
Uh <- new_data$wind_speed[set]
num_day <- new_data$num_day[set]
p <- new_data$p[set]
h2o_Uz <- new_data$h2o_Uz[set]
tau <- new_data$tau[set]

# truncate EF
l <- length(EF)
for(i in 1:l){
  if(EF[i] < 0){EF[i] = 0}
  if(EF[i] > 1){EF[i] = 1}
}

new_data2 <- data.frame(num_day,Uz,LE,SH,Vap,Temp,p,EF,Uh,
                        h2o_Uz,tau,Rain,previous.rain)

corr <- cor(new_data2)

# Resize/Recenter ---------------------------------------------------------
# Recenter: Temp, DPT; Resize: All
df.temp <- data.frame(Temp,p)
df.temp <- scale(df.temp)
df.norm <- data.frame(EF,LE,SH,Vap,Uh,Uz,h2o_Uz,tau)
df.norm <- scale(df.norm, center=FALSE, scale=apply(df.norm,2,sd,na.rm=TRUE)) 
df.all <- data.frame(num_day, Rain, previous.rain, df.norm, df.temp)
# check that mean=0, sd=1
colMeans(df.all)
apply(df.all,2,sd)

save(df.all, file = "CAP_LTR_scaled_4.RData")








