require(openxlsx)
require(weathermetrics)

####### early vs lat ice yrs. 
late <- c(1991, 1994, 2004, 2013, 2015) 
early<- c (1992, 1998, 2007, 2008, 2010)

#### directories 
data_wd<-"~/Desktop/CodeData/DATA/"
plot_wd<-"~/Desktop/CodeData/PLOTS/"

####### Load Data ########
setwd(data_wd)
load("all_vars_dates_values.Rdata")
ret_adv <- read.xlsx("Pal Datasets.xlsx", sheet = "SeaIce_Advance_Retreat", skipEmptyRows = FALSE)
day_of_seaice_retreat<- ret_adv[,c(1,7)] 
day_of_seaice_advance<- ret_adv[,c(1,4)]

### Add sea ice to dataset
all_vars_dates_values<-merge(all_vars_dates_values, day_of_seaice_retreat, by.x ="season", by.y ="Season")
colnames(all_vars_dates_values)[28]<-"sea_ice_retreat"

all_vars_dates_values<-merge(all_vars_dates_values, day_of_seaice_advance, by.x ="season", by.y ="Season")
colnames(all_vars_dates_values)[29]<-"sea_ice_advance"

#convert C to kelvin - appropriate for calculating CV 
all_vars_dates_values$temp_max <- celsius.to.kelvin(all_vars_dates_values$temp_max, round = 6)
all_vars_dates_values$SST_max <- celsius.to.kelvin(all_vars_dates_values$SST_max, round = 6)

all_vars_dates_values$MLD_min <-abs(all_vars_dates_values$MLD_min)




############# Calculate and plot coefficient of variation ####################

######################## LATE ICE YEARS ################################
calculated_cv<-as.data.frame(sapply(all_vars_dates_values[which(all_vars_dates_values$season %in% late),], function(x) round(sd(x,na.rm=T)/mean(x,na.rm=T),4)*100))

dates_cv <- subset(calculated_cv, rownames(calculated_cv) %in% c(  "flux_day_of_max" ,"Chl_day_of_max" ,"AirTemp_day_of_max", "Wind_day_of_max",   "whale_day_of_max" ,"BB_day_of_max"  ,"Phos_day_of_min" ,    "MLD_day_of_min" , "SST_day_of_peak"   ))

values_cv <-  subset(calculated_cv, rownames(calculated_cv) %in% c( "max_flux" ,   "max_chl_value"  ,"temp_max" ,"wind_max" ,   "whale_max",  "BB_max" , "Phos_min" , "MLD_min", "SST_max"   ))

###PLOT ###### 
setwd(plot_wd)
pdf("Cv_dates_vs_values_HighICE.pdf", height=6, width=6)

par(mfrow=c(1,1)); par(oma=c(2,2,2,0))

plot(dates_cv[,1], values_cv[,1],  xlab="Coefficient of Variation for Date", ylab="Coefficient of Variation for Value", xlim=c(0,80), ylim=c(0,100), col=c( "brown", "green4" ,"cornflowerblue" ,  "blue", "yellow3" ,"purple"  , "orange","pink" , "salmon"), cex=1.5, las=1 ,cex.lab=1.4, cex.axis=1.4, pch=c(19,19,15,15,19,19,19,15,15))
rect(-10,-10,20,20, col="lightgray", density=30, lwd=1)
#vertical lines for date only variables 
abline(v= calculated_cv[which(rownames(calculated_cv)=="sea_ice_retreat"),1], col="green", lwd=2)
abline(v= calculated_cv[which(rownames(calculated_cv)=="sea_ice_advance"),1], col="green", lwd=2,lty=3)
abline(v= calculated_cv[which(rownames(calculated_cv)=="lay"),1], col="wheat2", lwd=2)
abline(v= calculated_cv[which(rownames(calculated_cv)=="Pteropod_Day.of.Appearance"),1], col= "darkgray" , lwd=2)
abline(v= calculated_cv[which(rownames(calculated_cv)=="day_start0snow"),1], col= "black"   , lwd=2)
abline(v= calculated_cv[which(rownames(calculated_cv)=="day_end0snow"),1], col= "black"   , lwd=2,lty=3)

abline(a=0,b=1,lty=2)
points(dates_cv[,1], values_cv[,1], col=c( "brown", "green4" ,"cornflowerblue" ,  "blue", "yellow3" ,"purple"  , "orange","pink" , "salmon"), cex=1.5,pch=c(19,19,15,15,19,19,19,15,15))

legend("topright", c( "First Day Snow Depth=0","Last Day Snow Depth=0", "Pteropod Appearance", "Clutch Initiation", "Sea Ice Retreat", "Sea Ice Advance"),  col=c( "black","black", "darkgray","wheat2","green","green" ), pch=c(NA), cex=1.1, pt.cex=1.4, lwd=c(3),lty=c(1,3,1,1,1,3), bg="white")
box()
dev.off()
################################################################################################






######################## EARLY ICE YEARS ################################
calculated_cv<-as.data.frame(sapply(all_vars_dates_values[which(all_vars_dates_values$season %in% early),], function(x) round(sd(x,na.rm=T)/mean(x,na.rm=T),4)*100))

dates_cv <- subset(calculated_cv, rownames(calculated_cv) %in% c( "flux_day_of_max" ,"Chl_day_of_max" ,"AirTemp_day_of_max", "Wind_day_of_max",   "whale_day_of_max" ,"BB_day_of_max"  ,"Phos_day_of_min" ,    "MLD_day_of_min" , "SST_day_of_peak"      ))

values_cv <-  subset(calculated_cv, rownames(calculated_cv) %in% c( "max_flux" ,   "max_chl_value"  ,"temp_max" ,"wind_max" ,   "whale_max",  "BB_max" , "Phos_min" , "MLD_min", "SST_max"   ))

###PLOT ######  
pdf("Cv_dates_vs_values_LowICE.pdf", height=6, width=6) 
par(mfrow=c(1,1))
par(oma=c(2,2,2,0))

plot(dates_cv[,1], values_cv[,1],  xlab="Coefficient of Variation for Date", ylab="Coefficient of Variation for Value", xlim=c(0,80), ylim=c(0,100), col=c( "brown", "green4" ,"cornflowerblue" ,  "blue", "yellow3" ,"purple"  , "orange","pink" , "salmon"), cex=1.5 , las=1,cex.lab=1.4, cex.axis=1.4,pch=c(19,19,15,15,19,19,19,15,15)) 
rect(-10,-10,20,20, col="lightgray", density=30, lwd=1)
#vertical lines for date only variables 
abline(v= calculated_cv[which(rownames(calculated_cv)=="sea_ice_retreat"),1], col="green", lwd=2)
abline(v= calculated_cv[which(rownames(calculated_cv)=="sea_ice_advance"),1], col="green", lwd=2,lty=3)
abline(v= calculated_cv[which(rownames(calculated_cv)=="lay"),1], col="wheat2", lwd=2)
abline(v= calculated_cv[which(rownames(calculated_cv)=="Pteropod_Day.of.Appearance"),1], col= "darkgray" , lwd=2)
abline(v= calculated_cv[which(rownames(calculated_cv)=="day_start0snow"),1], col= "black"   , lwd=2)
abline(v= calculated_cv[which(rownames(calculated_cv)=="day_end0snow"),1], col= "black"   , lwd=2,lty=3)

points(dates_cv[,1], values_cv[,1], col=c( "brown", "green4" ,"cornflowerblue" ,  "blue", "yellow3" ,"purple"  , "orange","pink" , "salmon"), cex=1.5,pch=c(19,19,15,15,19,19,19,15,15))
abline(a=0,b=1,lty=2)
box()
dev.off()
################################################################################################













