require(lubridate)
require(zoo)
require(ggplot2)
require(reshape2)
require(openxlsx)

####### early vs lat ice yrs. 
late <- c(1991, 1994, 2004, 2013, 2015) 
early<- c (1992, 1998, 2007, 2008, 2010)

#### directories 
data_wd<-"~/Desktop/CodeData/DATA/"
plot_wd<-"~/Desktop/CodeData/PLOTS/"

####### Load Data ########
setwd(data_wd)
mn_repro_dates <- read.xlsx("Pal Datasets.xlsx", sheet = "Adélie", skipEmptyRows = FALSE)
weather <- read.xlsx("Pal Datasets.xlsx", sheet = "Weather", skipEmptyRows = FALSE)


####### Loop through each season to pull out min/max for variable of interest #####
Peaks_wind_temp<-NULL
all_data<-NULL
snow_time_duration<-NULL

for(g in 1:length(unique(weather$Season))){

this_yr<-unique(weather$Season)[g]; print(this_yr)
v1<-subset(weather, weather$Season == this_yr)
v1<-v1[order(v1$Days.Sept30),]
if(g == 25){v1<-v1[-366,]}

#missing big chunks in some years. 
if(length(v1$Days.Sept30)< 365){
newT<-1:max(v1$Days.Sept30)
v1<-v1[match(newT,v1[,"Days.Sept30"]),]
v1$Days.Sept30<-newT
} 

####### the approx if there are NAs
v1$Depth.at.Snowstake..cm.<-na.approx(v1$Depth.at.Snowstake..cm.)
v1$Temperature.Average..C.<-na.approx(v1$Temperature.Average..C.)
v1$Windspeed.Average<-na.approx(v1$Windspeed.Average)
all_data<-rbind(all_data, v1)

###### calculate peaks and valleys for each  yr and each vairable. 
temp1<- rollmean(v1$Temperature.Average..C., k=10,fill=NA)
wind1<-rollmean(v1$Windspeed.Average, k=10,fill=NA)
snow0<-which(v1$Depth.at.Snowstake..cm.==0)

#for wind speed focus on spring-summer-fall season
Peaks_wind_temp<-rbind(Peaks_wind_temp, rbind(
c("Air_temp",  this_yr, v1$Days.Sept30[which.max(temp1)], temp1[which.max(temp1)]),
c("wind",  this_yr, v1$Days.Sept30[30:150][which.min(wind1[30:150])] ,wind1[30:150][which.min(wind1[30:150])])))

snow_time_duration<-rbind(snow_time_duration, c( this_yr,min(v1$Days.Sept30[snow0]), max(v1$Days.Sept30[snow0]), max(v1$Days.Sept30[snow0])-min(v1$Days.Sept30[snow0]) ))


###check plot 
#dev.new()
#temp
#plot(v1$Days.Sept30, v1$Temperature.Average..C., type="o", main=g, ylim=c(-20,10))
#lines(v1$Days.Sept30,temp1, col="green",lwd=2)
#abline(v= v1$Days.Sept30[which.max(temp1)], col="green")

#wind
#plot(v1$Days.Sept30, v1$Windspeed.Average, type="o", main=g,ylim=c(0,25))
#lines(v1$Days.Sept30,wind1, col="green",lwd=2)
#abline(v= v1$Days.Sept30[30:150][which.min(wind1[30:150])], col="green")

#snow 
#plot(v1$Days.Sept30, v1$Depth.at.Snowstake..cm., type="o", main=g)
#abline(v= min(v1$Days.Sept30[snow0]), col="green")
#abline(v= max(v1$Days.Sept30[snow0]), col="green")
#########
}


 ######### save
colnames(Peaks_wind_temp)<-c("variable", "season", "day_of_peak","value")
Peaks_wind_temp<-as.data.frame( Peaks_wind_temp)
 Peaks_wind_temp[,2]<-as.numeric(as.character( Peaks_wind_temp[,2]))
 Peaks_wind_temp[,3]<-as.numeric( as.character( Peaks_wind_temp[,3]))
 Peaks_wind_temp[,4]<-as.numeric( as.character( Peaks_wind_temp[,4]))
 Peaks_wind_temp[30,3]<-100#in the case of ties choose first value
# save(Peaks_wind_temp, file="Peaks_wind_temp.Rdata")
 
 colnames(snow_time_duration)<-c("season", "day_start0snow", "day_end0snow", "duration")
 #save(snow_time_duration, file="snow_time_duration.Rdata")
 

##############  FIGURE #####################  
 
######## Labeling dates
dates<-as.Date(c("2020-10-01", "2020-11-01" ,"2020-12-01" ,"2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01" ,"2021-09-01"))
#day that coresponds to 1st of the month
day_since_sept30<-c(1, 32, 62 ,93, 124, 152, 183, 213, 244, 274, 305, 336)

setwd(plot_wd)
pdf("Weather_summary_phenology_highlight earlylate.pdf", height=10, width=6)

colnames_id<-c( "Air Temperature (C)", "Wind speed (m/s)", "Snow Depth (cm)")
par(mfrow=c(3,1)); par(oma=c(2,2,2,0))

for(z in 1:3){  # temp, wind, snow depth
  
  column <- c(6:8)[z] 
  if(column==7){
    limz<- c(0, 13)
  }else{
    limz<-range(weather[,column],na.rm=TRUE)
  }
  
  plot(NA,NA, col="lightgray", xlim=c(0,366), ylim= limz, xlab="", ylab=colnames_id[z], cex.lab=1.5, cex.axis=1.3,xaxs="i",yaxs="i", xaxt="n")
  axis(1, at=day_since_sept30, label=format(dates, "%b"),cex.axis=1.3)
  
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
  ############## ############## ############## 
  
#plot line for each year 
  for(q in 1:length(unique(all_data$Season))){
    yr1<-subset(all_data, all_data$Season == unique(all_data$Season)[q])
    yr1<-yr1[order(yr1$Days.Sept30),]
      VAR<-rollmean( yr1[,column], k=5,fill=NA)
    lines(time(yr1$Days.Sept30),VAR, col="gray")
  }
  
   mn_yrs<-aggregate(all_data[,column], by=list(all_data$Days.Sept30),FUN=mean, na.rm=TRUE )
  VAR_mn<-ksmooth(time(  mn_yrs$Group.1),   mn_yrs[,2], "normal", bandwidth = 50)
  lines(VAR_mn, col="black", lwd=3)
  box()
  
  ####### add early and late ice years
  ice_reorg_late<-all_data[which(all_data$Season %in% late==T),]
  ice_reorg_late<-aggregate(ice_reorg_late, by=list(ice_reorg_late$Days.Sept30),FUN=mean, na.rm=TRUE )
  agg_ice<-ice_reorg_late[order(ice_reorg_late$Days.Sept30),]
  lines(ksmooth(time(agg_ice$Group.1), agg_ice[,column+1], "normal", bandwidth = 50), col="blue", lwd=2)
  
  ice_reorg_early<-all_data[which(all_data$Season %in% early==T),]
  ice_reorg_early<-aggregate(ice_reorg_early, by=list(ice_reorg_early$Days.Sept30),FUN=mean, na.rm=TRUE )
  agg_ice<-ice_reorg_early[order(ice_reorg_early$Days.Sept30),]
  lines(ksmooth(time(agg_ice$Group.1), agg_ice[,column+1], "normal", bandwidth = 50), col="salmon", lwd=2)
  
  ##### ad mn/sd in peak/valley
  summ_wind<-Peaks_wind_temp[which(Peaks_wind_temp[,1]=="wind"),]
  summ_temp<- Peaks_wind_temp[which(Peaks_wind_temp[,1]=="Air_temp"),]
  
  pt_COL<-"green4"
  lwd_id<-3
  cex_id<-3
  if(column==6){ #Air temp 
    points(mean(summ_temp$day_of_peak), mean(summ_temp$value) , cex=cex_id, pch=20, col=  pt_COL)
    #sd for day of peak 
    segments(mean(summ_temp$day_of_peak)- sd(summ_temp$day_of_peak) ,mean(summ_temp$value),
             mean(summ_temp$day_of_peak)+ sd(summ_temp$day_of_peak), mean(summ_temp$value), col=  pt_COL,lwd=lwd_id)
    #sd for magnitude of wind speed 
    segments(mean(summ_temp$day_of_peak), mean(summ_temp$value)- sd(summ_temp$value),
             mean(summ_temp$day_of_peak), mean(summ_temp$value)+ sd(summ_temp$value),col=  pt_COL, lwd=lwd_id)
  }
  if(column==7){#wind
    points(mean(summ_wind$day_of_peak), mean(summ_wind$value) , cex=cex_id, pch=20,col=  pt_COL)
    #sd for day of peak 
    segments(mean(summ_wind$day_of_peak)- sd(summ_wind$day_of_peak) ,mean(summ_wind$value),
             mean(summ_wind$day_of_peak)+ sd(summ_wind$day_of_peak), mean(summ_wind$value),col=  pt_COL, lwd=lwd_id)
    #sd for magnitude of wind speed 
    segments(mean(summ_wind$day_of_peak), mean(summ_wind$value)- sd(summ_wind$value),
             mean(summ_wind$day_of_peak), mean(summ_wind$value)+ sd(summ_wind$value),col=  pt_COL, lwd=lwd_id)
    
  }
  if(column==8){ #snow 
    xxx<-5
    points(mean(snow_time_duration[,2]), xxx, cex=cex_id, pch=20,col=  pt_COL)
    #sd first day 0 zero snow
    segments(mean(snow_time_duration[,2])- sd(snow_time_duration[,2]) ,xxx,
             mean(snow_time_duration[,2])+ sd(snow_time_duration[,2]), xxx,col=  pt_COL, lwd=lwd_id)
    #sd last day 0 zero snow
    points(mean(snow_time_duration[,3]), xxx, cex=cex_id, pch=20,col=  pt_COL)
    segments(mean(snow_time_duration[,3])- sd(snow_time_duration[,3]) ,xxx,
             mean(snow_time_duration[,3])+ sd(snow_time_duration[,3]), xxx,col=  pt_COL, lwd=lwd_id)
  }
}
legend("topright", c("1991-2019", "1991-2019 Average" ), lwd=c(2,3), col=c("gray", "black"), bty="n", cex=1.5)

dev.off()
#################################################################










