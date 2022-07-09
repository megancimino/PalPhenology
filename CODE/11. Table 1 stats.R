require(openxlsx)

data_wd<-"~/Desktop/CodeData/DATA/"
setwd(data_wd)

########################## FLUX ##############################
load( file="flux_max.Rdata")

par(mfrow=c(1,2))
plot(flux_max$season, flux_max$day_of_max, type="o", ylab="Day of Max")
plot(flux_max$season, flux_max$max_flux, type="o", ylab="Flux value at maximum")

dataset_value<-flux_max$max_flux
dataset_day<-flux_max$day_of_max
dataset_season<-flux_max$season

## mean and sd in day of max 
mn1<-round(mean(dataset_day, na.rm=T),1); sd1<-round(sd(dataset_day, na.rm=T),1)
mn_sd<-(paste(mn1, "±", sd1))
range1<-range(dataset_day, na.rm=T); range1<-paste(range1[1], range1[2], sep="-")
Date_stats<-paste(mn_sd, " (", range1 ,")" , sep="")

## mean and sd in value at max 
mn2<-round(mean(dataset_value, na.rm=T),1); sd2<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn2, "±", sd2))
range1<-range(dataset_value, na.rm=T); range1<-paste(range1[1],range1[2], sep="-")
Value_stats<-paste(mn_sd, " (", range1 ,")" , sep="")
print(c(Date_stats, Value_stats))

###Coeffiecent of variation = #SD/Mean 
coeff_of_variation_date<-round(sd1/mn1,2)
coeff_of_variation_value<- round(sd2/mn2,2)
print(paste(coeff_of_variation_date, coeff_of_variation_value, sep="/"))

#Trend in TS 
summary(lm(dataset_day~ dataset_season)) #NO
summary(lm(dataset_value~ dataset_season)) #NO
############################################################################## 





########################## CHL ##############################
load( file="Chl_max.Rdata")

par(mfrow=c(1,2))
plot(Chl_max$season, Chl_max$day_of_max, type="o", ylab="Day of Chl max")
plot(Chl_max$season, Chl_max$max_chl_value, type="o", ylab="Value at CHL max")

dataset_value<-Chl_max$max_chl_value
dataset_day<-Chl_max$day_of_max
dataset_season<-Chl_max$season

## mean and sd in day of max 
mn1<-round(mean(dataset_day, na.rm=T),1); sd1<-round(sd(dataset_day, na.rm=T),1)
mn_sd<-(paste(mn1, "±", sd1))
range1<-range(dataset_day, na.rm=T); range1<-paste(range1[1], range1[2], sep="-")
Date_stats<-paste(mn_sd, " (", range1 ,")" , sep="")

## mean and sd in value at max 
mn2<-round(mean(dataset_value, na.rm=T),1); sd2<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn2, "±", sd2))
range1<-round(range(dataset_value, na.rm=T),2); range1<-paste(range1[1],range1[2], sep="-")
Value_stats<-paste(mn_sd, " (", range1 ,")" , sep="")
print(c(Date_stats, Value_stats))

###Coeffiecent of variation = #SD/Mean 
coeff_of_variation_date<-round(sd1/mn1,2)
coeff_of_variation_value<- round(sd2/mn2,2)
print(paste(coeff_of_variation_date, coeff_of_variation_value, sep="/"))

#Trend in TS 
summary(lm(dataset_day~ dataset_season)) #NO
summary(lm(dataset_value~ dataset_season)) #NO
############################################################################## 





########## WEATHER- AIR TEMP #################################################
load( file="Peaks_wind_temp.Rdata")

air_temp<-subset(Peaks_wind_temp, Peaks_wind_temp$variable=="Air_temp")

par(mfrow=c(1,2))
plot(air_temp$season, air_temp$day_of_peak, type="o", ylab="Day of peak")
plot(air_temp$season, air_temp$value, type="o", ylab="Value")

dataset_value<-air_temp$value
dataset_day<-air_temp$day_of_peak
dataset_season<-air_temp$season

## mean and sd in day of max 
mn1<-round(mean(dataset_day, na.rm=T),1); sd1<-round(sd(dataset_day, na.rm=T),1)
mn_sd<-(paste(mn1, "±", sd1))
range1<-range(dataset_day, na.rm=T); range1<-paste(range1[1], range1[2], sep="-")
Date_stats<-paste(mn_sd, " (", range1 ,")" , sep="")

## mean and sd in value at max 
mn2<-round(mean(dataset_value, na.rm=T),1); sd2<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn2, "±", sd2))
range1<-round(range(dataset_value, na.rm=T),2); range1<-paste(range1[1],range1[2], sep="-")
Value_stats<-paste(mn_sd, " (", range1 ,")" , sep="")
print(c(Date_stats, Value_stats))

###Coeffiecent of variation = #SD/Mean 
coeff_of_variation_date<-round(sd1/mn1,2)
coeff_of_variation_value<- round(sd2/mn2,2)
print(paste(coeff_of_variation_date, coeff_of_variation_value, sep="/"))

#Trend in TS 
summary(lm(dataset_day~ dataset_season)) #NO
summary(lm(dataset_value~ dataset_season)) #NO
############################################################################## 




########## WEATHER- WIND #################################################
load( file="Peaks_wind_temp.Rdata")

wind<-subset(Peaks_wind_temp, Peaks_wind_temp$variable=="wind")

par(mfrow=c(1,2))
plot(wind$season, wind$day_of_peak, type="o", ylab="Day of peak")
plot(wind$season, wind$value, type="o", ylab="Value")

dataset_value<-wind$value
dataset_day<-wind$day_of_peak
dataset_season<-wind$season

## mean and sd in day of max 
mn1<-round(mean(dataset_day, na.rm=T),1); sd1<-round(sd(dataset_day, na.rm=T),1)
mn_sd<-(paste(mn1, "±", sd1))
range1<-range(dataset_day, na.rm=T); range1<-paste(range1[1], range1[2], sep="-")
Date_stats<-paste(mn_sd, " (", range1 ,")" , sep="")

## mean and sd in value at max 
mn2<-round(mean(dataset_value, na.rm=T),1); sd2<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn2, "±", sd2))
range1<-round(range(dataset_value, na.rm=T),2); range1<-paste(range1[1],range1[2], sep="-")
Value_stats<-paste(mn_sd, " (", range1 ,")" , sep="")
print(c(Date_stats, Value_stats))

###Coeffiecent of variation = #SD/Mean 
coeff_of_variation_date<-round(sd1/mn1,2)
coeff_of_variation_value<- round(sd2/mn2,2)
print(paste(coeff_of_variation_date, coeff_of_variation_value, sep="/"))

#Trend in TS 
summary(lm(dataset_day~ dataset_season)) #NO
summary(lm(dataset_value~ dataset_season)) #NO
############################################################################## 







########## WEATHER- SNOW #################################################
load(file="snow_time_duration.Rdata")

snow<-as.data.frame(snow_time_duration)

par(mfrow=c(1,3))
plot(snow$season, snow$day_start0snow, type="o", ylab="first day no snow")
plot(snow$season, snow$day_end0snow, type="o", ylab="last day no snow")
plot(snow$season, snow$duration, type="o", ylab="duration")

dataset_value1<-snow$day_start0snow
dataset_value2<-snow$day_end0snow
dataset_value3<-snow$duration
dataset_season<-wind$season

## mean and sd in day start 
dataset_value<-dataset_value1
mn1<-round(mean(dataset_value, na.rm=T),1); sd1<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn1, "±", sd1))
range1<-round(range(dataset_value, na.rm=T),2); range1<-paste(range1[1],range1[2], sep="-")
Value_stats<-paste(mn_sd, " (", range1 ,")" , sep="")
#start day: 70.1 ± 21.8 (21-112)

## mean and sd in day end 
dataset_value<-dataset_value2
mn2<-round(mean(dataset_value, na.rm=T),1); sd2<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn2, "±", sd2))
range1<-round(range(dataset_value, na.rm=T),2); range1<-paste(range1[1],range1[2], sep="-")
Value_stats<-paste(mn_sd, " (", range1 ,")" , sep="")
#end day : 215.5 ± 21.8 (163-251)

## mean and sd in DURATION 
dataset_value<-dataset_value3
mn3<-round(mean(dataset_value, na.rm=T),1); sd3<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn3, "±", sd3))
range1<-round(range(dataset_value, na.rm=T),2); range1<-paste(range1[1],range1[2], sep="-")
Value_stats<-paste(mn_sd, " (", range1 ,")" , sep="")
#duration: "145.4 ± 34.8 (75-205)"


###Coeffiecent of variation = #SD/Mean 
coeff_of_variation_value1<-round(sd1/mn1,2)
coeff_of_variation_value2<- round(sd2/mn2,2)
coeff_of_variation_value3<- round(sd2/mn2,2)
print(paste(coeff_of_variation_value1, coeff_of_variation_value2,  coeff_of_variation_value3, sep="/"))

#Trend in TS 
summary(lm(dataset_value1~ dataset_season)) #NO
summary(lm(dataset_value2~ dataset_season)) #NO
summary(lm(dataset_value3~ dataset_season)) #NO
############################################################################## 




########## SEA ICE RET  #################################################
ret_adv <- read.xlsx("Pal Datasets.xlsx", sheet = "SeaIce_Advance_Retreat", skipEmptyRows = FALSE)

day_of_seaice_retreat<-ret_adv[13:41,]

par(mfrow=c(1,2))
plot(day_of_seaice_retreat$Season, day_of_seaice_retreat$Retreat_Days.Sept30, type="o", ylab="")

dataset_value1<-day_of_seaice_retreat$Retreat_Days.Sept30
dataset_season<-day_of_seaice_retreat$Season

## mean and sd 
dataset_value<-dataset_value1
mn1<-round(mean(dataset_value, na.rm=T),1); sd1<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn1, "±", sd1))
range1<-round(range(dataset_value, na.rm=T),2); range1<-paste(range1[1],range1[2], sep="-")
Value_stats<-paste(mn_sd, " (", range1 ,")" , sep="")
#"49 ± 20.9 (5-97)"

###Coeffiecent of variation = #SD/Mean 
coeff_of_variation_value1<-round(sd1/mn1,2)
print(coeff_of_variation_value1)

#Trend in TS 
summary(lm(dataset_value1~ dataset_season)) #NO
############################################################################## 



########## SEA ICE ADV #################################################
ret_adv <- read.xlsx("Pal Datasets.xlsx", sheet = "SeaIce_Advance_Retreat", skipEmptyRows = FALSE)

day_of_seaice_advance<-ret_adv[13:41,]

par(mfrow=c(1,2))
plot(day_of_seaice_advance$Season, day_of_seaice_advance$Advance_Days.Sept30, type="o", ylab="")

dataset_value1<-day_of_seaice_advance$Advance_Days.Sept30
dataset_season<-day_of_seaice_advance$Season

## mean and sd 
dataset_value<-dataset_value1
mn1<-round(mean(dataset_value, na.rm=T),1); sd1<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn1, "±", sd1))
range1<-round(range(dataset_value, na.rm=T),2); range1<-paste(range1[1],range1[2], sep="-")
Value_stats<-paste(mn_sd, " (", range1 ,")" , sep="")
#"272 ± 20.5 (215-298)"

###Coeffiecent of variation = #SD/Mean 
coeff_of_variation_value1<-round(sd1/mn1,2)
print(coeff_of_variation_value1)

#Trend in TS 
summary(lm(dataset_value1~ dataset_season)) #NO
############################################################################## 



########## Whales #################################################
load( file="whale_max.Rdata")

par(mfrow=c(1,2))
plot(whale_max$season, whale_max$day_of_max, type="o", ylab="Day of peak")
plot(whale_max$season, whale_max$max_whale_value, type="o", ylab="Value")

dataset_value<-whale_max$max_whale_value
dataset_day<-whale_max$day_of_max
dataset_season<-whale_max$season

## mean and sd in day of max 
mn1<-round(mean(dataset_day, na.rm=T),1); sd1<-round(sd(dataset_day, na.rm=T),1)
mn_sd<-(paste(mn1, "±", sd1))
range1<-range(dataset_day, na.rm=T); range1<-paste(range1[1], range1[2], sep="-")
Date_stats<-paste(mn_sd, " (", range1 ,")" , sep="")

## mean and sd in value at max 
mn2<-round(mean(dataset_value, na.rm=T),1); sd2<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn2, "±", sd2))
range1<-round(range(dataset_value, na.rm=T),2); range1<-paste(range1[1],range1[2], sep="-")
Value_stats<-paste(mn_sd, " (", range1 ,")" , sep="")
print(c(Date_stats, Value_stats))

###Coeffiecent of variation = #SD/Mean 
coeff_of_variation_date<-round(sd1/mn1,2)
coeff_of_variation_value<- round(sd2/mn2,2)
print(paste(coeff_of_variation_date, coeff_of_variation_value, sep="/"))

#Trend in TS 
summary(lm(dataset_day~ dataset_season)) #NO
summary(lm(dataset_value~ dataset_season)) #NO
############################################################################## 




########## pteropods #################################################
pt <- read.xlsx("Pal Datasets.xlsx", sheet = "Pteropods", skipEmptyRows = FALSE)

par(mfrow=c(1,2))
plot(pt$Season.of_Appearance, pt$Days.Sept30, type="o", ylab="Day of peak")

dataset_day<-pt$Days.Sept30
dataset_season<-pt$Season.of_Appearance

## mean and sd in day of max 
mn1<-round(mean(dataset_day, na.rm=T),1); sd1<-round(sd(dataset_day, na.rm=T),1)
mn_sd<-(paste(mn1, "±", sd1))
range1<-range(dataset_day, na.rm=T); range1<-paste(range1[1], range1[2], sep="-")
Date_stats<-paste(mn_sd, " (", range1 ,")" , sep="")

###Coeffiecent of variation = #SD/Mean 
coeff_of_variation_date<-round(sd1/mn1,2)
coeff_of_variation_value<- NA #round(sd2/mn2,2)
print(paste(coeff_of_variation_date, coeff_of_variation_value, sep="/"))

#Trend in TS 
summary(lm(dataset_day~ dataset_season)) #NO
############################################################################## 




############# REPRO DATES ####################################################
mn_repro_dates <- read.xlsx("Pal Datasets.xlsx", sheet = "Adélie", skipEmptyRows = FALSE)

dataset_value1<-mn_repro_dates$lay 
dataset_value2<-mn_repro_dates$hatch
dataset_season<-mn_repro_dates$season

## mean and sd in day start 
dataset_value<-dataset_value1
mn1<-round(mean(dataset_value, na.rm=T),1); sd1<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn1, "±", sd1))
range1<-round(range(dataset_value, na.rm=T),2); range1<-paste(range1[1],range1[2], sep="-")
print(Value_stats<-paste(mn_sd, " (", range1 ,")" , sep=""))

## mean and sd in day end 
dataset_value<-dataset_value2
mn2<-round(mean(dataset_value, na.rm=T),1); sd2<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn2, "±", sd2))
range1<-round(range(dataset_value, na.rm=T),2); range1<-paste(range1[1],range1[2], sep="-")
print(Value_stats<-paste(mn_sd, " (", range1 ,")" , sep=""))

###Coeffiecent of variation = #SD/Mean 
coeff_of_variation_value1<-round(sd1/mn1,2)
coeff_of_variation_value2<- round(sd2/mn2,2)
print(paste(coeff_of_variation_value1, coeff_of_variation_value2, sep="/"))

#Trend in TS 
summary(lm(dataset_value1~ dataset_season)) #NO
summary(lm(dataset_value2~ dataset_season)) #NO
############################################################################## 



########################## Bacteria Biomass ##############################
load( file="BB_max.Rdata")

par(mfrow=c(1,2))
plot(BB_max$season, BB_max$day_of_max, type="o", ylab="Day of Max")
plot(BB_max$season, BB_max$max_BB, type="o", ylab="BB value at maximum")

dataset_value<-BB_max$max_BB
dataset_day<-BB_max$day_of_max
dataset_season<-BB_max$season

## mean and sd in day of max 
mn1<-round(mean(dataset_day, na.rm=T),1); sd1<-round(sd(dataset_day, na.rm=T),1)
mn_sd<-(paste(mn1, "±", sd1))
range1<-range(dataset_day, na.rm=T); range1<-paste(range1[1], range1[2], sep="-")
Date_stats<-paste(mn_sd, " (", range1 ,")" , sep="")

## mean and sd in value at max 
mn2<-round(mean(dataset_value, na.rm=T),1); sd2<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn2, "±", sd2))
range1<-round(range(dataset_value, na.rm=T),2); range1<-paste(range1[1],range1[2], sep="-")
Value_stats<-paste(mn_sd, " (", range1 ,")" , sep="")
print(c(Date_stats, Value_stats))

###Coeffiecent of variation = #SD/Mean 
coeff_of_variation_date<-round(sd1/mn1,2)
coeff_of_variation_value<- round(sd2/mn2,2)
print(paste(coeff_of_variation_date, coeff_of_variation_value, sep="/"))

#Trend in TS 
summary(lm(dataset_day~ dataset_season)) #NO
summary(lm(dataset_value~ dataset_season)) #NO
############################################################################## 





########################## Nutrients - Nitrate ##############################
load( file="Nitrate_min.Rdata")

par(mfrow=c(1,2))
plot(Nitrate_min$season, Nitrate_min$day_of_min, type="o", ylab="Day of Min")
plot(Nitrate_min$season, Nitrate_min$min_Nitrate, type="o", ylab="Nitrate value at min")

dataset_value<-Nitrate_min$min_Nitrate
dataset_day<-Nitrate_min$day_of_min
dataset_season<-Nitrate_min$season

## mean and sd in day of max 
mn1<-round(mean(dataset_day, na.rm=T),1); sd1<-round(sd(dataset_day, na.rm=T),1)
mn_sd<-(paste(mn1, "±", sd1))
range1<-range(dataset_day, na.rm=T); range1<-paste(range1[1], range1[2], sep="-")
Date_stats<-paste(mn_sd, " (", range1 ,")" , sep="")

## mean and sd in value at max 
mn2<-round(mean(dataset_value, na.rm=T),1); sd2<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn2, "±", sd2))
range1<-round(range(dataset_value, na.rm=T),2); range1<-paste(range1[1],range1[2], sep="-")
Value_stats<-paste(mn_sd, " (", range1 ,")" , sep="")
print(c(Date_stats, Value_stats))

###Coeffiecent of variation = #SD/Mean 
coeff_of_variation_date<-round(sd1/mn1,2)
coeff_of_variation_value<- round(sd2/mn2,2)
print(paste(coeff_of_variation_date, coeff_of_variation_value, sep="/"))

#Trend in TS 
summary(lm(dataset_day~ dataset_season)) #NO
summary(lm(dataset_value~ dataset_season)) #NO
############################################################################## 



########################## Nutrients - PHOSPHATE ##############################
setwd("~/Desktop/PAL_Projects/PAL_phenology/DATA_FILES/")
load( file="Phos_min.Rdata")

par(mfrow=c(1,2))
plot(Phos_min$season, Phos_min$day_of_min, type="o", ylab="Day of Max")
plot(Phos_min$season, Phos_min$min_Phos_value, type="o", ylab="Nitrate value at maximum")

dataset_value<- Phos_min$min_Phos_value
dataset_day<-Phos_min$day_of_min
dataset_season<-Phos_min$season

## mean and sd in day of max 
mn1<-round(mean(dataset_day, na.rm=T),1); sd1<-round(sd(dataset_day, na.rm=T),1)
mn_sd<-(paste(mn1, "±", sd1))
range1<-range(dataset_day, na.rm=T); range1<-paste(range1[1], range1[2], sep="-")
Date_stats<-paste(mn_sd, " (", range1 ,")" , sep="")

## mean and sd in value at max 
mn2<-round(mean(dataset_value, na.rm=T),1); sd2<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn2, "±", sd2))
range1<-round(range(dataset_value, na.rm=T),2); range1<-paste(range1[1],range1[2], sep="-")
Value_stats<-paste(mn_sd, " (", range1 ,")" , sep="")
print(c(Date_stats, Value_stats))

###Coeffiecent of variation = #SD/Mean 
coeff_of_variation_date<-round(sd1/mn1,2)
coeff_of_variation_value<- round(sd2/mn2,2)
print(paste(coeff_of_variation_date, coeff_of_variation_value, sep="/"))

#Trend in TS 
summary(lm(dataset_day~ dataset_season)) #NO
summary(lm(dataset_value~ dataset_season)) #NO
############################################################################## 



########## SST ##############################################################
load( file="Peaks_sst.Rdata")

par(mfrow=c(1,2))
plot(Peaks_sst$season, Peaks_sst$day_of_peak, type="o", ylab="Day of Max")
plot(Peaks_sst$season, Peaks_sst$value, type="o", ylab="value at maximum")

dataset_value<-Peaks_sst$value
dataset_day<-Peaks_sst$day_of_peak
dataset_season<-Peaks_sst$season

## mean and sd in day of max 
mn1<-round(mean(dataset_day, na.rm=T),1); sd1<-round(sd(dataset_day, na.rm=T),1)
mn_sd<-(paste(mn1, "±", sd1))
range1<-range(dataset_day, na.rm=T); range1<-paste(range1[1], range1[2], sep="-")
Date_stats<-paste(mn_sd, " (", range1 ,")" , sep="")

## mean and sd in value at max 
mn2<-round(mean(dataset_value, na.rm=T),1); sd2<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn2, "±", sd2))
range1<-round(range(dataset_value, na.rm=T),2); range1<-paste(range1[1],range1[2], sep="-")
Value_stats<-paste(mn_sd, " (", range1 ,")" , sep="")
print(c(Date_stats, Value_stats))

###Coeffiecent of variation = #SD/Mean 
coeff_of_variation_date<-round(sd1/mn1,2)
coeff_of_variation_value<- round(sd2/mn2,2)
print(paste(coeff_of_variation_date, coeff_of_variation_value, sep="/"))

#Trend in TS 
summary(lm(dataset_day~ dataset_season)) #NO
summary(lm(dataset_value~ dataset_season)) #NO
############################################################################## 



###### MLD################################################
load( file="MLD_min.Rdata")

par(mfrow=c(1,2))
plot(MLD_min$season, MLD_min$day_of_min, type="o", ylab="Day of Min")
plot(MLD_min$season, MLD_min$min_MLD_value, type="o", ylab="value at min")

dataset_value<- MLD_min$min_MLD_value
dataset_day<-MLD_min$day_of_min
dataset_season<-MLD_min$season

## mean and sd in day of max 
mn1<-round(mean(dataset_day, na.rm=T),1); sd1<-round(sd(dataset_day, na.rm=T),1)
mn_sd<-(paste(mn1, "±", sd1))
range1<-range(dataset_day, na.rm=T); range1<-paste(range1[1], range1[2], sep="-")
Date_stats<-paste(mn_sd, " (", range1 ,")" , sep="")

## mean and sd in value at max 
mn2<-round(mean(dataset_value, na.rm=T),1); sd2<-round(sd(dataset_value, na.rm=T),1)
mn_sd<-(paste(mn2, "±", sd2))
range1<-round(range(dataset_value, na.rm=T),2); range1<-paste(range1[1],range1[2], sep="-")
Value_stats<-paste(mn_sd, " (", range1 ,")" , sep="")
print(c(Date_stats, Value_stats))

###Coeffiecent of variation = #SD/Mean 
coeff_of_variation_date<-round(sd1/mn1,2)
coeff_of_variation_value<- round(sd2/mn2,2)
print(paste(coeff_of_variation_date, coeff_of_variation_value, sep="/"))

#Trend in TS 
summary(lm(dataset_day~ dataset_season)) #NO
summary(lm(dataset_value~ dataset_season)) #NO
############################################################################## 


