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
nut <- read.xlsx("Pal Datasets.xlsx", sheet = "Nutrients", skipEmptyRows = FALSE)


####### Loop through each season to pull out min/max for variable of interest #####
Phos_min<-NULL
Nitrate_min<-NULL

for(g in 1:length(unique(nut$Season))){
  this_yr<-unique(nut$Season)[g]
  v1<-subset(nut, nut$Season == this_yr)
  v1<-v1[order(v1$Days.Sept30 ),]
  ###### calculate min
  Nitrate_min<-rbind(Nitrate_min, rbind(
    c("Nitrate",  this_yr, v1$Days.Sept30[which.min(v1[,7])], v1[,7][which.min(v1[,7])])))
  Phos_min<-rbind(Phos_min, rbind(
    c("Phosphate",  this_yr, v1$Days.Sept30[which.min(v1[,6])], v1[,6][which.min(v1[,6])])))
  
 # dev.new()
  #par(mfrow=c(1,2))
 # plot(v1$Days.Sept30, v1[,6], type="o", main=c(g, this_yr), xlim=c(1, 200))
 # abline(v= v1$Days.Sept30[which.min(v1[,6])], col="green")
 # abline(v=c(26,151)) 
 # abline(v=c(50), lty=2, col="gray")
  
 # plot(v1$Days.Sept30, v1[,7], type="o", main=c(g, this_yr), xlim=c(1, 200))
 # abline(v= v1$Days.Sept30[which.min(v1[,7])], col="green")
 # abline(v=c(26,151)) 
#  abline(v=c(50), lty=2, col="gray")
}


colnames( Nitrate_min)<-c("variable","season","day_of_min", "min_Nitrate_value")
Nitrate_min<-as.data.frame(Nitrate_min)
Nitrate_min[,2]<-as.numeric(as.character(Nitrate_min[,2]))
Nitrate_min[,3]<-as.numeric(as.character(Nitrate_min[,3]))
Nitrate_min[,4]<-as.numeric(as.character(Nitrate_min[,4]))

colnames( Phos_min)<-c("variable","season","day_of_min", "min_Phos_value")
Phos_min<-as.data.frame(Phos_min)
Phos_min[,2]<-as.numeric(as.character(Phos_min[,2]))
Phos_min[,3]<-as.numeric(as.character(Phos_min[,3]))
Phos_min[,4]<-as.numeric(as.character(Phos_min[,4]))

#remove years with gaps 
Nitrate_min[Nitrate_min$season %in% c(2001,2000,1995:1991),3]<-NA
Nitrate_min[Nitrate_min$season %in% c(2001,2000,1995:1991),4]<-NA

Phos_min[Phos_min$season %in% c( 2001,2000,1995:1991),3]<-NA
Phos_min[Phos_min$season %in% c(2001,2000,1995:1991),4]<-NA

#save(Nitrate_min, file="Nitrate_min.Rdata")
#save(Phos_min, file="Phos_min.Rdata")




#############  FIGURE #####################  

######## Labeling dates
dates<-as.Date(c("2020-10-01", "2020-11-01" ,"2020-12-01" ,"2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01" ,"2021-09-01"))
day_since_sept30<-c(1, 32, 62 ,93, 124, 152, 183, 213, 244, 274, 305, 336)

setwd(plot_wd)
pdf("Phosphate_summary_phenology.pdf", height=10, width=6)

par(mfrow=c(3,1)); par(oma=c(2,2,2,0))
plot(NA,NA, col="lightgray", xlim=c(0,366), ylim=c(10,130) , xlab="", ylab="Phosphate (mmol m^-3)", cex.lab=1.5, cex.axis=1.3,,xaxs="i",yaxs="i", xaxt="n")
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
for(q in 1:length(unique(nut$Season))){
  yr1<-subset(nut, nut$Season == unique(nut$Season)[q])
  if((yr1$Season[1] %in% c(2001,2000,1995:1991))==F ){
    yr1<-yr1[order(yr1$Days.Sept30),]
    lines(yr1$Days.Sept30 ,yr1[,6], col="gray")
  }
}
box()


zzz<-nut[order(nut$Season),]
#remove years with incomplete data 
zzz<-zzz[-which(zzz$Season %in% c(2001,2000,1995:1991)==T),]
roundJD<-  round(zzz$Days.Sept30, digits=-1)#rounds to nearest 10    
aaa<-aggregate(zzz[,6], by=list(roundJD), FUN=mean, na.rm=T)
lines(aaa[,1], aaa[,2], col="black", lwd=3)

##### ad mn/sd in peak/valley
lwd_id<-3; cex_id<-3; pt_COL<-"green4"

points(mean(Phos_min$day_of_min, na.rm=TRUE), mean(Phos_min$min_Phos_value, na.rm=TRUE) , cex=cex_id, pch=20, col=pt_COL)
#sd for day of peak 
segments(mean(Phos_min$day_of_min, na.rm=TRUE)- sd(Phos_min$day_of_min, na.rm=TRUE) ,mean(Phos_min$min_Phos_value, na.rm=TRUE),
         mean(Phos_min$day_of_min, na.rm=TRUE)+ sd(Phos_min$day_of_min, na.rm=TRUE), mean(Phos_min$min_Phos_value, na.rm=TRUE), col=pt_COL,lwd=lwd_id)
#sd for magnitude of wind speed 
segments(mean(Phos_min$day_of_min, na.rm=TRUE), mean(Phos_min$min_Phos_value, na.rm=TRUE)- sd(Phos_min$min_Phos_value, na.rm=TRUE),
         mean(Phos_min$day_of_min, na.rm=TRUE), mean(Phos_min$min_Phos_value, na.rm=TRUE)+ sd(Phos_min$min_Phos_value, na.rm=TRUE),col=pt_COL, lwd=lwd_id)

legend("topright", c("1991-2018", "1991-2018 Average" ), lwd=c(2,3), col=c("gray", "black"), bty="n", cex=1.5)

dev.off()
#################################################################






############## FIGURE  ##################### 
pdf("Nitrate_summary_phenology.pdf", height=10, width=6)
par(mfrow=c(3,1)); par(oma=c(2,2,2,0))
plot(NA,NA, col="lightgray", xlim=c(0,366), ylim=c(400,1800) , xlab="", ylab="Nitrate (mmol m^-3)", cex.lab=1.5, cex.axis=1.3,,xaxs="i",yaxs="i", xaxt="n")
axis(1, at=day_since_sept30, label=format(dates, "%b"),cex.axis=1.3)

############## and quantiles and mean penguin dates 
id1<-1
id2<-5
col1<-"bisque2" 
rect(quantile(mn_repro_dates[,2], na.rm=TRUE)[id1], -2000, quantile(mn_repro_dates[,2], na.rm=TRUE)[id2], 2000, col=col1, border=F)
rect(quantile(mn_repro_dates[,3], na.rm=TRUE)[id1], -2000, quantile(mn_repro_dates[,3], na.rm=TRUE)[id2], 2000, col=col1, border=F)
rect(quantile(mn_repro_dates[,4], na.rm=TRUE)[id1], -2000, quantile(mn_repro_dates[,4], na.rm=TRUE)[id2], 2000, col=col1, border=F)
rect(quantile(mn_repro_dates[,5], na.rm=TRUE)[id1], -2000, quantile(mn_repro_dates[,5], na.rm=TRUE)[id2], 2000, col=col1, border=F)
id1<-2
id2<-4
col2<-"seashell1" 
rect(quantile(mn_repro_dates[,2], na.rm=TRUE)[id1], -2000, quantile(mn_repro_dates[,2], na.rm=TRUE)[id2], 2000, col=col2, border=F)
rect(quantile(mn_repro_dates[,3], na.rm=TRUE)[id1], -2000, quantile(mn_repro_dates[,3], na.rm=TRUE)[id2], 2000, col=col2, border=F)
rect(quantile(mn_repro_dates[,4], na.rm=TRUE)[id1], -2000, quantile(mn_repro_dates[,4], na.rm=TRUE)[id2], 2000, col=col2, border=F)
rect(quantile(mn_repro_dates[,5], na.rm=TRUE)[id1], -2000, quantile(mn_repro_dates[,5], na.rm=TRUE)[id2], 2000, col=col2, border=F)

# add repro means 
abline(v= mean(mn_repro_dates[,2], na.rm=TRUE), col="bisque4")#lay
abline(v= mean(mn_repro_dates[,3], na.rm=TRUE), col="bisque4")#hatch
abline(v= mean(mn_repro_dates[,4], na.rm=TRUE), col="bisque4")#crit per
abline(v= mean(mn_repro_dates[,5], na.rm=TRUE), col="bisque4")# fledge
############## ############## ############## 

#plot line for each year 
for(q in 1:length(unique(nut$Season))){
  yr1<-subset(nut, nut$Season == unique(nut$Season)[q])
  if((yr1$Season[1] %in% c(2001,2000,1995:1991))==F ){
    yr1<-yr1[order(yr1$Days.Sept30),]
    lines(yr1$Days.Sept30 ,yr1[,7], col="gray")
  }
}
box()


zzz<-nut[order(nut$Season),]
#remove years with incomplete data 
zzz<-zzz[-which(zzz$Season %in% c(2001,2000,1995:1991)==T),]
roundJD<-  round(zzz$Days.Sept30, digits=-1)#rounds to nearest 10    
aaa<-aggregate(zzz[,7], by=list(roundJD), FUN=mean, na.rm=T)
lines(aaa[,1], aaa[,2], col="black", lwd=3)

##### ad mn/sd in peak/valley
lwd_id<-3; cex_id<-3; pt_COL<-"green4"

points(mean(Nitrate_min$day_of_min, na.rm=TRUE), mean(Nitrate_min$min_Nitrate_value, na.rm=TRUE) , cex=cex_id, pch=20, col=pt_COL)
#sd for day of peak 
segments(mean(Nitrate_min$day_of_min, na.rm=TRUE)- sd(Nitrate_min$day_of_min, na.rm=TRUE) ,mean(Nitrate_min$min_Nitrate_value, na.rm=TRUE),
         mean(Nitrate_min$day_of_min, na.rm=TRUE)+ sd(Nitrate_min$day_of_min, na.rm=TRUE), mean(Nitrate_min$min_Nitrate_value, na.rm=TRUE), col=pt_COL,lwd=lwd_id)
#sd for magnitude of wind speed 
segments(mean(Nitrate_min$day_of_min, na.rm=TRUE), mean(Nitrate_min$min_Nitrate_value, na.rm=TRUE)- sd(Nitrate_min$min_Nitrate_value, na.rm=TRUE),
         mean(Nitrate_min$day_of_min, na.rm=TRUE), mean(Nitrate_min$min_Nitrate_value, na.rm=TRUE)+ sd(Nitrate_min$min_Nitrate_value, na.rm=TRUE),col=pt_COL, lwd=lwd_id)

legend("topright", c("1991-2018", "1991-2018 Average" ), lwd=c(2,3), col=c("gray", "black"), bty="n", cex=1.5)

dev.off()
#################################################################



















