require(lubridate)
require(zoo)
require(ggplot2)
require(reshape2)

#scales the data between 0 and 1 
range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

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

all_vars_dates_values$MLD_min <-abs(all_vars_dates_values$MLD_min)










######## DATES #############
dates_cv <-all_vars_dates_values[,which(colnames(all_vars_dates_values) %in% c("season", "sea_ice_day_of_max",  "flux_day_of_max" ,"Chl_day_of_max" ,"AirTemp_day_of_max", "Wind_day_of_max",   "whale_day_of_max" ,"BB_day_of_max"  ,"Phos_day_of_min" ,    "MLD_day_of_min" , "SST_day_of_peak", "lay", "Pteropod_Day.of.Appearance",  "day_start0snow" ,   "day_end0snow","sea_ice_retreat","sea_ice_advance" ))]

sample_size_dates_lowice<-sapply(dates_cv[which(dates_cv$season %in% early),], function(x)(length(which(is.na(x)==F))))
sample_size_dates_highice<-sapply(dates_cv[which(dates_cv$season %in% late),], function(x)(length(which(is.na(x)==F))))
sample_size_dates<-rbind(sample_size_dates_highice, sample_size_dates_lowice)

Mn_dates_allyrs<-colMeans(dates_cv,na.rm=T)
Mn_dates_highice<- colMeans(dates_cv[which(dates_cv$season %in% late),],na.rm=T)
Mn_dates_lowice <-colMeans(dates_cv[which(dates_cv$season %in% early),],na.rm=T)
HighIce_anom<-  Mn_dates_highice-Mn_dates_allyrs
LowIce_anom<- Mn_dates_lowice-Mn_dates_allyrs
#bind all together
Dates_lowHigh<-cbind(Mn_dates_allyrs,Mn_dates_highice,Mn_dates_lowice,HighIce_anom,LowIce_anom)

dates_plot<-t(Dates_lowHigh[c(-1),4:5])
colnames(dates_plot)<-c("Clutch Initiation", "Max. Flux","Max. Chl", "Max. Air Temp", "Min. Wind Speed", "First Day Snow Depth=0", "Last Day Snow Depth=0", "Max. Whales", "Pteropod Appearance", "Max. Bacteria", "Min. Phosphate", "Min. MLD", "Max. SST","Ice Retreat", "Ice Advance")
rownames(dates_plot)<-c("Late Retreat", "Early Retreat")

###### manually reorder so it is in the order of phenological events
dates_plot<-dates_plot[,c(1,14,6 ,5,3,12,13,4,2,8,11,10,7,9,15)]

ssd<-sample_size_dates[,-c(1)]
ssd<-ssd[,c(1,14,6 ,5,3,12,13,4,2,8,11,10,7,9,15)]
  
######### barplot  ####################
setwd(plot_wd)
pdf("PhenoAnomalies_Days_earlylateretreat.pdf", height=6, width=10)

par(mfrow=c(1,1))
par(mar = c(9.1, 4.1, 4.1, 2.1))
par(oma=c(3,2,0,0))
 barplot(dates_plot, 
        col=c("blue", "salmon") , 
        border="black", 
        font.axis=1, 
        beside=T, 
     #   legend=rownames(dates_plot), 
     #   args.legend = list(x = 7, y=-25),
        font.lab=1, ylim=c(-70,70),
        las=2, cex.lab=1.2, cex.axis = 1.2,
        ylab="Anomaly (days)", xaxt="n")
abline(h=c(-50,-30,-10,10,30,50), col="gray")

text(seq(2.3,44,length.out=15), par("usr")[3]-0.25, 
     srt = 40, adj = 1.1, xpd = TRUE,
     labels = paste(colnames(dates_plot)), cex = 1.2)

barplot(dates_plot, 
        col=c("blue", "salmon") , 
        border="black", 
        font.axis=1, 
        beside=T, 
      #  legend=rownames(dates_plot), 
       # args.legend = list(x = 7, y=-25,bg="white"),
        font.lab=1, ylim=c(-65,65),
        las=2,cex.lab=1.2, cex.axis = 1.2,xaxt="n",
        ylab="Anomaly (days)", add=T)
box();abline(h=0, lwd=1.5)
axis(1, at=seq(2.3,44,length.out=15),labels=NA, tck=.03)

text(seq(2.3,44,length.out=15), rep(65, 13), paste0("n=",ssd[1,]),col="blue" ,cex=.9)
text(seq(2.3,44,length.out=15), rep(-60, 13), paste0("n=",ssd[2,]),col="salmon" ,cex=.9)
dev.off()
#############################################












####### VALUES ##############################
values_cv <-  all_vars_dates_values[,which(colnames(all_vars_dates_values) %in% c("season","ice_SIC" , "max_flux" ,   "max_chl_value"  ,"temp_max" ,"wind_max" ,   "whale_max",  "BB_max" , "Phos_min" , "MLD_min", "SST_max"   ))]

#scale all values btw 0 and 1 
values_scaled<-as.data.frame(sapply(values_cv , function(x) range01(x,na.rm=T) ) )
values_scaled$season<-values_cv$season

sample_size_values_lowice<-sapply(values_cv[which(values_cv$season %in% early),], function(x)(length(which(is.na(x)==F))))
sample_size_values_highice<-sapply(values_cv[which(values_cv$season %in% late),], function(x)(length(which(is.na(x)==F))))
sample_size_values<-rbind(sample_size_values_highice, sample_size_values_lowice)


Mn_values_allyrs<-colMeans(values_scaled,na.rm=T)
Mn_values_highice<- colMeans(values_scaled[which(values_scaled$season %in% late),],na.rm=T)
Mn_values_lowice <-colMeans(values_scaled[which(values_scaled$season %in% early),],na.rm=T)
HighIce_anom_v<-  Mn_values_highice-Mn_values_allyrs
LowIce_anom_v<- Mn_values_lowice-Mn_values_allyrs
#bind all together
Values_lowHigh<-cbind(Mn_values_allyrs,Mn_values_highice,Mn_values_lowice,HighIce_anom_v,LowIce_anom_v)

values_plot<-t(Values_lowHigh[c(-1),4:5])
colnames(values_plot)<-c( "Max. Flux","Max. Chl", "Max. Air Temp", "Min. Wind Speed", "Max. Whales",  "Max. Bacteria", "Min. Phosphate", "Min. MLD", "Max. SST")

#merge 
require(plyr)
sample_size_dates<-sample_size_dates[,c(-1)]
colnames(sample_size_dates)<- colnames(dates_plot)
sample_size_values<-sample_size_values[,c(-1)]
colnames(sample_size_values)<-colnames(values_plot)
new_samp_size<-rbind.fill( as.data.frame(sample_size_dates), as.data.frame(sample_size_values))[3:4,]

#orders colnames same as last plot 
new_frame<-rbind.fill( as.data.frame(dates_plot), as.data.frame(values_plot))[3:4,]
values_plot<-new_frame
values_plot<-replace(values_plot, is.na(values_plot)==T, 0)



######### barplot  ####################
pdf("PhenoAnomalies_Values_earlylateretreat.pdf", height=6, width=10)

par(mfrow=c(1,1))
par(mar = c(9.1, 4.1, 4.1, 2.1))
par(oma=c(3,2,0,0))
barplot(as.matrix(values_plot), 
        col=c("blue", "salmon") , 
        border="black", 
        font.axis=1, 
        beside=T, cex.lab=1.2, cex.axis = 1.2,
        legend=rownames(dates_plot),  #comment out for no legend
        args.legend = list(x = "topleft",bg="white"),  #comment out for no legend
        font.lab=1, ylim=c(-.6,.6),
        las=2,xaxt="n",
        ylab="Anomaly (scaled values)")
abline(h=c(-.5, -.1,-.3,.1,.3,.5), col="gray")

text(seq(2.3,44,length.out=15), par("usr")[3]-0.06, 
     srt = 30, adj = c(1), xpd = TRUE,
     labels = paste(colnames(values_plot)), cex = 1.2)

barplot(as.matrix(values_plot), 
        col=c("blue", "salmon") , 
        border="black", cex.lab=1.2, cex.axis = 1.2,
        font.axis=1, 
        beside=T, xaxt="n",
        legend=rownames(dates_plot), 
        args.legend = list(x = "topleft",bg="white"),
        font.lab=1,las=2,add=T)
box();abline(h=0, lwd=1.5)
axis(1, at=seq(2.3,44,length.out=15),labels=NA, tck=0.03)

no_samp_size<-which(is.na(new_samp_size[1,])==T)

dev.off()
#############################################










