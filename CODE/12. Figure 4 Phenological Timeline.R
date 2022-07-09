require(lubridate)
require(zoo)
require(ggplot2)
require(reshape2)
require(openxlsx)

#scales the data between 0 and 1 to plot together 
range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

#directories
plot_wd<-"~/Desktop/CodeData/PLOTS/"
data_wd<-"~/Desktop/CodeData/DATA/"
setwd(data_wd)


####### LOAD REPRO DATES 
mn_repro_dates <- read.xlsx("Pal Datasets.xlsx", sheet = "Adélie", skipEmptyRows = FALSE)

######  WEATHER 
load( file="Peaks_wind_temp.Rdata")
load( file="snow_time_duration.Rdata")

summ_wind<-Peaks_wind_temp[which(Peaks_wind_temp[,1]=="wind"),]
summ_temp<- Peaks_wind_temp[which(Peaks_wind_temp[,1]=="Air_temp"),]

summ_wind$ value <- range01(summ_wind$ value)
summ_temp$ value <- range01(summ_temp$ value)

######### FLUX 
load( file="flux_max.Rdata")
flux_max$max_flux <-range01(flux_max$max_flux, na.rm=TRUE)

####### CHL 
load( file="Chl_max.Rdata")
Chl_max$max_chl_value <-range01(Chl_max$max_chl_value, na.rm=TRUE )

####### whales 
load( file="whale_max.Rdata")
whale_max$max_whale_value <-range01(whale_max$max_whale_value, na.rm=TRUE )

######pteropods 
pt <- read.xlsx("Pal Datasets.xlsx", sheet = "Pteropods", skipEmptyRows = FALSE)

##### Phospahte 
load( file="Phos_min.Rdata")
Phos_min$min_Phos_value <-range01(Phos_min$min_Phos_value, na.rm=TRUE )

###### bacteria 
load( file="BB_max.Rdata")
BB_max$max_BB_value <-range01(BB_max$max_BB_value, na.rm=TRUE )

###### MLD
load( file="MLD_min.Rdata")
MLD_min$min_MLD_value <-range01(abs(MLD_min$min_MLD_value), na.rm=TRUE )

####### SST
load( file="Peaks_sst.Rdata")
Peaks_sst$value <-range01(Peaks_sst$value, na.rm=TRUE )

####### daylength
dayleng<- read.xlsx("Pal Datasets.xlsx", sheet = "Daylength", skipEmptyRows = FALSE)
dayleng$length_scaled<- range01(dayleng[,1], na.rm=TRUE )

##### ice retreat advance
ret_adv <- read.xlsx("Pal Datasets.xlsx", sheet = "SeaIce_Advance_Retreat", skipEmptyRows = FALSE)
day_of_seaice_retreat<- ret_adv$Retreat_Days.Sept30
day_of_seaice_advance<- ret_adv$Advance_Days.Sept30



dates<-as.Date(c("2020-10-01", "2020-11-01" ,"2020-12-01" ,"2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01" ,"2021-09-01"))
#day that coresponds to 1st of the month
day_since_sept30<-c(1, 32, 62 ,93, 124, 152, 183, 213, 244, 274, 305, 336)











############## ADD ABOVE MN AND SD TO NICE FIGURE #####################  
setwd(plot_wd)
pdf("Mn_SD_allvars_phenology4.pdf", height=6, width=10)

par(mfrow=c(1,1)); par(oma=c(2,2,2,0))
plot(NA,NA, col="lightgray", xlim=c(0,366), ylim= c(0,1), xlab="", ylab="Scaled Parameter", cex.lab=1.2, cex.axis=1.2,xaxs="i",yaxs="i", xaxt="n") 
axis(1, at=day_since_sept30, label=format(dates, "%b"),cex=1.2)
lines(dayleng$Days.Sept30, dayleng$length_scaled, lwd=9, col="khaki1") #day length
text(320, .3, "Day length", col="black", srt=43, cex=1)

############## and quantiles and mean penguin dates 
id1<-1
id2<-5
col1<-"bisque2" 
rect(quantile(mn_repro_dates[,2], na.rm=TRUE)[id1], -1000, quantile(mn_repro_dates[,2], na.rm=TRUE)[id2], 1000, col=col1, border=F)
rect(quantile(mn_repro_dates[,3], na.rm=TRUE)[id1], -1000, quantile(mn_repro_dates[,3], na.rm=TRUE)[id2], 1000, col=col1, border=F)
rect(quantile(mn_repro_dates[,4], na.rm=TRUE)[id1], -1000, quantile(mn_repro_dates[,4], na.rm=TRUE)[id2], 1000, col=col1, border=F)
rect(quantile(mn_repro_dates[,5], na.rm=TRUE)[id1], -1000, quantile(mn_repro_dates[,5], na.rm=TRUE)[id2], 1000, col=col1, border=F)
id1<-2
id2<-4
col2<-"seashell1" 
rect(quantile(mn_repro_dates[,2], na.rm=TRUE)[id1], -1000, quantile(mn_repro_dates[,2], na.rm=TRUE)[id2], 1000, col=col2, border=F)
rect(quantile(mn_repro_dates[,3], na.rm=TRUE)[id1], -1000, quantile(mn_repro_dates[,3], na.rm=TRUE)[id2], 1000, col=col2, border=F)
rect(quantile(mn_repro_dates[,4], na.rm=TRUE)[id1], -1000, quantile(mn_repro_dates[,4], na.rm=TRUE)[id2], 1000, col=col2, border=F)
rect(quantile(mn_repro_dates[,5], na.rm=TRUE)[id1], -1000, quantile(mn_repro_dates[,5], na.rm=TRUE)[id2], 1000, col=col2, border=F)

# add repro means 
abline(v= mean(mn_repro_dates[,2], na.rm=TRUE), col="bisque4")#lay
abline(v= mean(mn_repro_dates[,3], na.rm=TRUE), col="bisque4")#hatch
abline(v= mean(mn_repro_dates[,4], na.rm=TRUE), col="bisque4")#crit per
abline(v= mean(mn_repro_dates[,5], na.rm=TRUE), col="bisque4")# fledge

text(x = c(mean(mn_repro_dates[,2], na.rm=TRUE),mean(mn_repro_dates[,3], na.rm=TRUE),mean(mn_repro_dates[,4], na.rm=TRUE),mean(mn_repro_dates[,5], na.rm=TRUE)),
     y = par("usr")[4] + 0.03,
     labels = c("Lay", "Hatch", "Hatch + 20d", "Fledge"),
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 30,
     cex = 1,
     adj = 0.1)
############## ############## ############## 
lwd_id<-2; cex_id<-1.5

######## SEA ICE RETREAT 
points(mean(day_of_seaice_retreat[13:41]),.05+.04 , cex=cex_id, pch=15, col="green")
segments(mean(day_of_seaice_retreat[13:41])- sd(day_of_seaice_retreat[13:41]) ,.05+.04,
         mean(day_of_seaice_retreat[13:41])+ sd(day_of_seaice_retreat[13:41]), .05+.04,lwd=lwd_id, col="green")

######## SEA ICE Advance
points(mean(day_of_seaice_advance[13:41]),.05+.04 , cex=cex_id, pch=15, col="green")
segments(mean(day_of_seaice_advance[13:41])- sd(day_of_seaice_advance[13:41]) ,.05+.04,
         mean(day_of_seaice_advance[13:41])+ sd(day_of_seaice_advance[13:41]), .05+.04,lwd=lwd_id, col="green")

######## WIND 
#sd for day of peak 
points(mean(summ_wind$day_of_peak),mean(summ_wind$value) , cex=cex_id, pch=15, col="blue")
segments(mean(summ_wind$day_of_peak)- sd(summ_wind$day_of_peak) ,mean(summ_wind$value),
 mean(summ_wind$day_of_peak)+ sd(summ_wind$day_of_peak), mean(summ_wind$value),lwd=lwd_id, col="blue")
#sd for magnitude of wind speed 
segments(mean(summ_wind$day_of_peak), mean(summ_wind$value)- sd(summ_wind$value),
mean(summ_wind$day_of_peak), mean(summ_wind$value)+ sd(summ_wind$value),lwd=lwd_id, col="blue")

#### SST
points(mean(Peaks_sst$day_of_peak, na.rm=TRUE), mean(Peaks_sst$value, na.rm=TRUE) , cex=cex_id, pch=15, col="salmon")
#sd for day of peak 
segments(mean(Peaks_sst$day_of_peak, na.rm=TRUE)- sd(Peaks_sst$day_of_peak, na.rm=TRUE) ,mean(Peaks_sst$value, na.rm=TRUE),
         mean(Peaks_sst$day_of_peak, na.rm=TRUE)+ sd(Peaks_sst$day_of_peak, na.rm=TRUE), mean(Peaks_sst$value, na.rm=TRUE), col="salmon",lwd=lwd_id+.4)
#sd for magnitude of wind speed 
segments(mean(Peaks_sst$day_of_peak, na.rm=TRUE), mean(Peaks_sst$value, na.rm=TRUE)- sd(Peaks_sst$value, na.rm=TRUE),
         mean(Peaks_sst$day_of_peak, na.rm=TRUE), mean(Peaks_sst$value, na.rm=TRUE)+ sd(Peaks_sst$value, na.rm=TRUE),col="salmon", lwd=lwd_id+.4)

####### AIR TEMP 
points(mean(summ_temp$day_of_peak),mean(summ_temp$value) , cex=cex_id, pch=15, col="cornflowerblue")
#sd for day of peak 
segments(mean(summ_temp$day_of_peak)- sd(summ_temp$day_of_peak) ,mean(summ_temp$value),
 mean(summ_temp$day_of_peak)+ sd(summ_temp$day_of_peak), mean(summ_temp$value),lwd=lwd_id, col="cornflowerblue")
#sd for magnitude of wind speed 
segments(mean(summ_temp$day_of_peak), mean(summ_temp$value)- sd(summ_temp$value),
mean(summ_temp$day_of_peak), mean(summ_temp$value)+ sd(summ_temp$value),lwd=lwd_id, col="cornflowerblue")

###### SNOW
on.x.axis.at<-.05
#sd first day 0 zero snow
points(mean(snow_time_duration[,2]),on.x.axis.at, cex=cex_id, pch=15)
points(mean(snow_time_duration[,3]),on.x.axis.at , cex=cex_id, pch=15)
segments(mean(snow_time_duration[,2])- sd(snow_time_duration[,2]) ,on.x.axis.at,
 mean(snow_time_duration[,2])+ sd(snow_time_duration[,2]), on.x.axis.at,lwd=lwd_id,)
#sd last day 0 zero snowcpoints(mean(snow_time_duration[,3]), 1, cex=2, pch=20)
segments(mean(snow_time_duration[,3])- sd(snow_time_duration[,3]) ,on.x.axis.at,
 mean(snow_time_duration[,3])+ sd(snow_time_duration[,3]), on.x.axis.at,lwd=lwd_id,)

######### FLUX 
points(mean(flux_max$day_of_max, na.rm=TRUE), mean(flux_max$max_flux, na.rm=TRUE) , cex=cex_id, pch=19, col="brown")
#sd for day of peak 
segments(mean(flux_max$day_of_max, na.rm=TRUE)- sd(flux_max$day_of_max, na.rm=TRUE) ,mean(flux_max$max_flux, na.rm=TRUE),
 mean(flux_max$day_of_max, na.rm=TRUE)+ sd(flux_max$day_of_max, na.rm=TRUE), mean(flux_max$max_flux, na.rm=TRUE), col="brown",lwd=lwd_id)
#sd for magnitude of wind speed 
segments(mean(flux_max$day_of_max, na.rm=TRUE), mean(flux_max$max_flux, na.rm=TRUE)- sd(flux_max$max_flux, na.rm=TRUE),
mean(flux_max$day_of_max, na.rm=TRUE), mean(flux_max$max_flux, na.rm=TRUE)+ sd(flux_max$max_flux, na.rm=TRUE),col="brown", lwd=lwd_id)

####### CHL 
points(mean(Chl_max$day_of_max, na.rm=TRUE), mean(Chl_max$max_chl_value, na.rm=TRUE) , cex=cex_id, pch=19, col="green4")
#sd for day of peak 
segments(mean(Chl_max$day_of_max, na.rm=TRUE)- sd(Chl_max$day_of_max, na.rm=TRUE) ,mean(Chl_max$max_chl_value, na.rm=TRUE),
 mean(Chl_max$day_of_max, na.rm=TRUE)+ sd(Chl_max$day_of_max, na.rm=TRUE), mean(Chl_max$max_chl_value, na.rm=TRUE), col="green4",lwd=lwd_id)
#sd for magnitude of wind speed 
segments(mean(Chl_max$day_of_max, na.rm=TRUE), mean(Chl_max$max_chl_value, na.rm=TRUE)- sd(Chl_max$max_chl_value, na.rm=TRUE),
mean(Chl_max$day_of_max, na.rm=TRUE), mean(Chl_max$max_chl_value, na.rm=TRUE)+ sd(Chl_max$max_chl_value, na.rm=TRUE),col="green4", lwd=lwd_id)

####### whales
points(mean(whale_max$day_of_max, na.rm=TRUE), mean(whale_max$max_whale_value, na.rm=TRUE) , cex=cex_id, pch=19, col="yellow3")
#sd for day of peak 
segments(mean(whale_max$day_of_max, na.rm=TRUE)- sd(whale_max$day_of_max, na.rm=TRUE) ,mean(whale_max$max_whale_value, na.rm=TRUE),
         mean(whale_max$day_of_max, na.rm=TRUE)+ sd(whale_max$day_of_max, na.rm=TRUE), mean(whale_max$max_whale_value, na.rm=TRUE), col="yellow3",lwd=lwd_id)
#sd for magnitude
segments(mean(whale_max$day_of_max, na.rm=TRUE), mean(whale_max$max_whale_value, na.rm=TRUE)- sd(whale_max$max_whale_value, na.rm=TRUE),
         mean(whale_max$day_of_max, na.rm=TRUE), mean(whale_max$max_whale_value, na.rm=TRUE)+ sd(whale_max$max_whale_value, na.rm=TRUE),col="yellow3", lwd=lwd_id)

###### pteropods 
points(mean(pt$Days.Sept30,na.rm=T), .05+.02 , cex=cex_id, pch=19, col="darkgray")
#sd for day of peak 
segments(mean(pt$Days.Sept30,na.rm=T)- sd(pt$Days.Sept30,na.rm=T) ,.05+.02,
         mean(pt$Days.Sept30,na.rm=T)+ sd(pt$Days.Sept30,na.rm=T),.05+.02, col="darkgray",lwd=lwd_id)

###### nutrients
points(mean(Phos_min$day_of_min, na.rm=TRUE), mean(Phos_min$min_Phos_value, na.rm=TRUE) , cex=cex_id, pch=19, col="orange")
#sd for day of peak 
segments(mean(Phos_min$day_of_min, na.rm=TRUE)- sd(Phos_min$day_of_min, na.rm=TRUE) ,mean(Phos_min$min_Phos_value, na.rm=TRUE),
         mean(Phos_min$day_of_min, na.rm=TRUE)+ sd(Phos_min$day_of_min, na.rm=TRUE), mean(Phos_min$min_Phos_value, na.rm=TRUE), col="orange",lwd=lwd_id)
#sd for magnitude of wind speed 
segments(mean(Phos_min$day_of_min, na.rm=TRUE), mean(Phos_min$min_Phos_value, na.rm=TRUE)- sd(Phos_min$min_Phos_value, na.rm=TRUE),
         mean(Phos_min$day_of_min, na.rm=TRUE), mean(Phos_min$min_Phos_value, na.rm=TRUE)+ sd(Phos_min$min_Phos_value, na.rm=TRUE),col="orange", lwd=lwd_id)

####### bacteria 
points(mean(BB_max$day_of_max, na.rm=TRUE), mean(BB_max$max_BB_value, na.rm=TRUE) , cex=cex_id, pch=19, col="purple")
#sd for day of peak 
segments(mean(BB_max$day_of_max, na.rm=TRUE)- sd(BB_max$day_of_max, na.rm=TRUE) ,mean(BB_max$max_BB_value, na.rm=TRUE),
         mean(BB_max$day_of_max, na.rm=TRUE)+ sd(BB_max$day_of_max, na.rm=TRUE), mean(BB_max$max_BB_value, na.rm=TRUE), col="purple",lwd=lwd_id)
#sd for magnitude 
segments(mean(BB_max$day_of_max, na.rm=TRUE), mean(BB_max$max_BB_value, na.rm=TRUE)- sd(BB_max$max_BB_value, na.rm=TRUE),
         mean(BB_max$day_of_max, na.rm=TRUE), mean(BB_max$max_BB_value, na.rm=TRUE)+ sd(BB_max$max_BB_value, na.rm=TRUE),col="purple", lwd=lwd_id)

#### MLD 
points(mean(MLD_min$day_of_min, na.rm=TRUE), mean(MLD_min$min_MLD_value, na.rm=TRUE) , cex=cex_id, pch=15, col="pink")
#sd for day of peak 
segments(mean(MLD_min$day_of_min, na.rm=TRUE)- sd(MLD_min$day_of_min, na.rm=TRUE) ,mean(MLD_min$min_MLD_value, na.rm=TRUE),
         mean(MLD_min$day_of_min, na.rm=TRUE)+ sd(MLD_min$day_of_min, na.rm=TRUE), mean(MLD_min$min_MLD_value, na.rm=TRUE), col="pink",lwd=lwd_id)
#sd for magnitude of wind speed 
segments(mean(MLD_min$day_of_min, na.rm=TRUE), mean(MLD_min$min_MLD_value, na.rm=TRUE)- sd(MLD_min$min_MLD_value, na.rm=TRUE),
         mean(MLD_min$day_of_min, na.rm=TRUE), mean(MLD_min$min_MLD_value, na.rm=TRUE)+ sd(MLD_min$min_MLD_value, na.rm=TRUE),col="pink", lwd=lwd_id)

box()
legend("topright", c("Sea Ice Retreat/Advance","Min. Wind Speed", "Max. Air Temp", "First/Last Day Snow Depth=0",  "Min. MLD", "Max. SST","Max. POC Flux", "Max. Chlorophyll", "Max. Whales","Pteropod Appearance","Min. Phosphate", "Max. Bacteria"), lwd=2, col=c("green","blue", "cornflowerblue", "black","pink","salmon" , "brown", "green4","yellow3", "darkgray","orange","purple"), bty="n",pch=c(rep(15,6),rep(19,6)), cex=.9)

dev.off()
########################################

