# CAP LTR data pre-processing to match CFSR processing
# re-size all data, re-center ONLY T 

# Settings ----------------------------------------------------------------
rm(list=ls())
library(rjags)
setwd('~/Documents/CAP_LTER_flux_data')
library(chron)
library(lubridate)
library(dplyr)

# Functions ---------------------------------------------------------------
chron_time <- function(d_time){ #returns chron structure of date-time
  d_time <- as.character(d_time)
  d_time <- t(as.data.frame(strsplit(d_time,' ')))
  row.names(d_time) = NULL
  c_time <- chron(dates=d_time[,1],times=d_time[,2], format=c('y-m-d','h:m:s'))
  # time = fractional day from Jan 1, 1970, use as.numeric
  return(c_time)
}

time_of_day <- function(time){ # returns tod from chron structure as hour decimal
  h <- hour(time)
  m <- minute(time)
  tod = h + m/60
  return(tod)
}

# Flux Tower Data --------------------------------------------------------------------
# load flux tower data, begins @ 10/31/2011  11:30:00 AM w/ 30 min increment
data <- read.csv("OPEC.csv", header = TRUE) 
vapor <- unlist(data[,"h2o_Avg"], use.names = FALSE) # water g/m3
SH <- unlist(data[,"Hc"], use.names = FALSE) # W/m2
LE <- unlist(data[,"LE_wpl"], use.names = FALSE) # W/m2
Temp <- unlist(data[,"t_hmp_mean"], use.names = FALSE) # air T degC
wind_speed <- unlist(data[,"wnd_spd"], use.names = FALSE) # m/s
p <- unlist(data[,"press_mean"], use.names = FALSE) # kPa
h2o_Uz <- unlist(data[,"cov_h2o_Uz"], use.names = FALSE) # vapor flux covariance mg/m2/s
tau <- unlist(data[,"tau"], use.names = FALSE) # momentum flux Pa
Uz <- unlist(data[,"Uz_Avg"], use.names = FALSE) # m/s
EF <- LE/(LE+SH)
d_time <- unlist(data[,"date_time"], use.names = FALSE) # string

# Convert date-time ------------------------------------------------------
# convert date-time to useful variables
chron_t <- chron_time(d_time) # chron format structure
tod <- time_of_day(chron_t) # as hour floating point
moy <- month(chron_t) # as month 1-12
num_day <- floor(as.numeric(chron_t)) # day number since 01/01/1970

# subset mornings ----------------------------------------------------------
# daily values from 7-11 am
df1 <- data.frame(num_day,tod,moy,vapor,SH,LE,Temp,wind_speed,p,h2o_Uz,tau,Uz,EF)
# remove bad data points
df1 <- mutate(df1, bad = (vapor<0 | vapor>250 | SH<(-20) | SH>200 | 
                            LE<(-20) | LE>200 | h2o_Uz<(-0.1) | h2o_Uz>0.1))
df1 <- subset(df1, df1$bad==FALSE)
df1$bad <- NULL
df1 <- na.omit(df1)
df1 <- subset(df1, tod>=7)
df1 <- subset(df1, tod<=11)

# daily averages ----------------------------------------------------------
# average daily values from 7-11 am
am_ave <- aggregate(.~num_day, df1, mean) # 1553 observations if no data is omitted
am_ave$tod <- NULL
am_ave <- na.omit(am_ave)

# Rain Data ---------------------------------------------------------------
# load rain data, begins @ 9/1/2010  8:30:00 AM w/ 30 min increment
# IRREGULARLY SPACED DATA!
rain_dat <- read.csv("rad_rain.csv", header = TRUE)
rain_tot <- unlist(rain_dat[,"Rain_mm_TOT"], use.names = FALSE) # mm
rain_tot[is.na(rain_tot)] <- 0 # change NAs to zeros
rain <- ceiling(rain_tot/30) # categorical rain 0/1

# find peak tod for logical rain
rain_d_time <- unlist(rain_dat[,"date_time"], use.names = FALSE) # string
rain_chron_t <- chron_time(rain_d_time) # chron format structure
rain_tod <- time_of_day(rain_chron_t) # tod as hour floating point
rain_moy <- month(rain_chron_t) # as month 1-12
rain_num_day <- floor(as.numeric(rain_chron_t)) # day number since 01/01/1970

# rain daily -----------------------------------------------------
# NO precipitation from 7-11am && precipitation from 11-23
# Identify days with morning rain
df2 <- data.frame(rain_num_day,rain_tod,rain_moy,rain)
df2am <- subset(df2, tod>=7)
df2am <- subset(df2, tod<=11)
rain_am <- aggregate(.~rain_num_day, df2am, max)
rain_am$rain_tod <- NULL
names(rain_am)[names(rain_am)=="rain"] <- "morning"
# Identify days with afternoon rain
df2pm <- subset(df2, tod>=11)
df2pm <- subset(df2, tod<=23)
rain_pm <- aggregate(.~rain_num_day, df2pm, max)
rain_pm$rain_tod <- NULL
names(rain_pm)[names(rain_pm)=="rain"] <- "afternoon"
# combine rain-am & rain_pm and categorize as pm_rain (w/o morning rain)
rain_df <- merge(rain_am, rain_pm, all=FALSE)
rain_df <- mutate(rain_df, pm_rain = (morning == 0 & afternoon == 1))

# Combine data frames -----------------------------------------------------
names(rain_df)[names(rain_df)=="rain_num_day"] <- "num_day"
names(rain_df)[names(rain_df)=="rain_moy"] <- "moy"
new_data <- merge(rain_df, am_ave, all=FALSE)
new_data$pm_rain <- as.integer(new_data$pm_rain)
save(new_data, file = "new_data4.RData")


# Proceed to CAP_LTR_analyses4.R






