require(openxlsx)
library(tidyverse)

data_wd<-"~/Desktop/CodeData/DATA/"
setwd(data_wd)

############# REPRO DATES ####################################################
mn_repro_dates <- read.xlsx("Pal Datasets.xlsx", sheet = "Adélie", skipEmptyRows = FALSE)

########################## FLUX ##############################
load( file="flux_max.Rdata")
flux_max<-flux_max[,-1]
colnames(flux_max)[2]<-c("flux_day_of_max")

########################## CHL ##############################
load( file="Chl_max.Rdata")
Chl_max<-Chl_max[,-1]
colnames(Chl_max)[2]<-c("Chl_day_of_max")

########## WEATHER- Temp/Wind ################################
load( file="Peaks_wind_temp.Rdata")
air_temp<-subset(Peaks_wind_temp, Peaks_wind_temp$variable=="Air_temp")
air_temp<-air_temp[,-1]
colnames(air_temp)[2:3]<-c("AirTemp_day_of_max","temp_max")

wind<-subset(Peaks_wind_temp, Peaks_wind_temp$variable=="wind")
wind<-wind[,-1]
colnames(wind)[2:3]<-c("Wind_day_of_max","wind_max")

########## WEATHER- SNOW ######################################
load(file="snow_time_duration.Rdata")
snow<-as.data.frame(snow_time_duration)
colnames(snow)[4]<-"no_snow_duration"

########## Whales #################################################
load( file="whale_max.Rdata")
whale_max<-whale_max[,-1]
colnames(whale_max)[2:3]<-c("whale_day_of_max","whale_max")

########## pteropods #################################################
pt <- read.xlsx("Pal Datasets.xlsx", sheet = "Pteropods", skipEmptyRows = FALSE)
pt<-pt[,c(4,2)]
colnames(pt)[1:2]<-c("Pteropod_Day.of.Appearance" ,"season")

##### Phospahte #################################################
load( file="Phos_min.Rdata")
Phos_min<-Phos_min[,-1]
colnames(Phos_min)[2:3]<-c("Phos_day_of_min","Phos_min")

###### bacteria #################################################
load( file="BB_max.Rdata")
BB_max<-BB_max[,-1]
colnames(BB_max)[2:3]<-c("BB_day_of_max","BB_max")

###### MLD #################################################
load( file="MLD_min.Rdata")
MLD_min<-MLD_min[,-1]
colnames(MLD_min)[2:3]<-c("MLD_day_of_min","MLD_min")

####### SST #################################################
load( file="Peaks_sst.Rdata")
Peaks_sst<-Peaks_sst[,-1]
colnames(Peaks_sst)[2:3]<-c("SST_day_of_peak","SST_max")
###################################################



all_vars_dates_values<-
list(mn_repro_dates,  flux_max, Chl_max, air_temp, wind,snow,whale_max, pt, BB_max, Phos_min, MLD_min, Peaks_sst) %>% reduce(full_join, by = "season")
save(all_vars_dates_values, file="all_vars_dates_values.Rdata")


