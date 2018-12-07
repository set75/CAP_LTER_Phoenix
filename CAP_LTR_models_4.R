# CAP LLTER flux tower full model

rm(list=ls())
setwd("~/Documents/CAP_LTER_flux_data")
load("CAP_LTR_scaled_4.RData")
library(rjags)
library(runjags)
library(statip)
library(boot)

# Read in data ------------------------------------------------------------
num_day <- df.all$num_day
Rain <- df.all$Rain
pRain <- df.all$previous.rain
EF <- df.all$EF
LE <- df.all$LE
SH <- df.all$SH
Vap <- df.all$Vap
Uh <- df.all$Uh
Uz <- df.all$Uz
h2o_Uz <- df.all$h2o_Uz
tau <- df.all$tau
Temp <- df.all$Temp
press <- df.all$p
len <- length(EF)

# Total model -------------------------------------------------------------
line_data <- list(Rain=Rain,pRain=pRain,press=press,h2o_Uz=h2o_Uz,EF=EF,LE=LE,SH=SH,
                  Uh=Uh,Uz=Uz,Temp=Temp,tau=tau,N=len) # 10 parameters
set.seed(27) # for reproducibility
line_inits <- function(){list("alpha"=rnorm(1), "b.1"=rnorm(1), "b.2"=rnorm(1),
                              "b.3"=rnorm(1), "b.4"=rnorm(1), "b.5"=rnorm(1),
                              "b.6"=rnorm(1), "b.7"=rnorm(1), "b.8"=rnorm(1),
                              "b.9"=rnorm(1), "b.10"=rnorm(1))} # random gen of initial values
jags.params <- c("alpha","b.1","b.2","b.3","b.4","b.5","b.6","b.7","b.8","b.9","b.10")
C <- 3
model <- autorun.jags("CAP_LTR_all.bug.txt", data = line_data, inits = line_inits, n.chains = C,
                      monitor = jags.params, method = "rjags")
summary(model)


# Comparison model -------------------------------------------------------------
line_data <- list(Rain=Rain,pRain=pRain,EF=EF,LE=LE,SH=SH,
                  Uh=Uh,Uz=Uz,Temp=Temp,Vap=Vap,N=len) # 10 parameters
set.seed(27) # for reproducibility
line_inits <- function(){list("alpha"=rnorm(1), "b.1"=rnorm(1), "b.4"=rnorm(1), "b.5"=rnorm(1),
                              "b.6"=rnorm(1), "b.7"=rnorm(1), "b.8"=rnorm(1),
                              "b.9"=rnorm(1), "b.10"=rnorm(1))} # random gen of initial values
jags.params <- c("alpha","b.1","b.4","b.5","b.6","b.7","b.8","b.9","b.10")
C <- 3
model <- autorun.jags("CAP_LTR_comp.bug.txt", data = line_data, inits = line_inits, n.chains = C,
                      monitor = jags.params, method = "rjags")
summary(model)


# linear model ------------------------------------------------------------
glm.data <- data.frame(pRain,EF,LE,SH,Uh,Uz,Temp,Vap)
frequentist <- glm(Rain ~., family=binomial(link='logit'), data=glm.data)
summary(frequentist)

