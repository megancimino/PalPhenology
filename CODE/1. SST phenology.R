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
SST <- read.xlsx("Pal Datasets.xlsx", sheet = "SST", skipEmptyRows = FALSE)


####### Loop through each season to pull out min/max for variable of interest #####
Peaks_sst<-NULL
approximated_SST<-NULL

for(g in 1:length(unique(SST$Season))){
  
  this_yr<-unique(SST$Season)[g]; print(this_yr)
  v1<-subset(SST, SST$Season == this_yr)
  v1<-v1[order(v1$Days.Sept30),]
  
  if(g == 25){ v1<-v1[-366,]}
  
  #missing dates in some years  
  if(length(v1$Days.Sept30)< 365){
    newT<-1:max(v1$Days.Sept30)
    v1<-v1[match(newT,v1[,"Days.Sept30"]),]
    v1$Days.Sept30<-newT
  }

  ### fill in the few values with NA 
  # In a few years the data starts/ends with NAs; ignore those points
  if(this_yr == 1991){
    v1$Sea.Surface.Temperature..C.[32:366]<-na.approx(v1$Sea.Surface.Temperature..C.)
  }
  
  if(this_yr == 1992){
    v1$Sea.Surface.Temperature..C.[4:362]<-na.approx(v1$Sea.Surface.Temperature..C.)
  }
  
  if(this_yr == 1994){
    v1$Sea.Surface.Temperature..C.[5:365]<-na.approx(v1$Sea.Surface.Temperature..C.)
  }
  
  if(this_yr == 2012){
    v1$Sea.Surface.Temperature..C.[1:358]<-na.approx(v1$Sea.Surface.Temperature..C.)
  }
  
  if( this_yr != 2012 & this_yr != 1991 & this_yr != 1992 & this_yr != 1994 ){
    v1$Sea.Surface.Temperature..C.<-na.approx(v1$Sea.Surface.Temperature..C.)
  }
  approximated_SST<-rbind(approximated_SST, v1)
  
  ###### calculate peaks/valleys for each  yr and each variable. 
  sst1<-rollmean(v1$Sea.Surface.Temperature..C., k=5,fill=NA)
  Peaks_sst<-rbind(Peaks_sst, rbind(c("SST",  this_yr, v1$Days.Sept30[which.max(sst1)],  sst1[which.max(sst1)])))
  
  #check plot
  dev.new()
  plot(v1$Days.Sept30, v1$Sea.Surface.Temperature..C., type="o", main="SST", ylim=c(-3,4))
    abline(v= v1$Days.Sept30[which.max(sst1)], col="red")
    lines(v1$Days.Sept30,sst1, col="green",lwd=2)
}
############################### DONE ##############################


####save ##############################
colnames(Peaks_sst)<-c("variable", "season", "day_of_peak","value")
Peaks_sst<-as.data.frame( Peaks_sst)
Peaks_sst[,2]<-as.numeric(as.character( Peaks_sst[,2]))
Peaks_sst[,3]<-as.numeric( as.character( Peaks_sst[,3]))
Peaks_sst[,4]<-as.numeric( as.character( Peaks_sst[,4]))
#save(Peaks_sst, file="Peaks_sst.Rdata")



############## FIGURE #####################  

######## Labeling dates
dates<-as.Date(c("2020-10-01", "2020-11-01" ,"2020-12-01" ,"2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01" ,"2021-09-01"))
#day that corresponds to 1st of the month
day_since_sept30<-c(1, 32, 62 ,93, 124, 152, 183, 213, 244, 274, 305, 336)


setwd(plot_wd)
pdf("SST_summary_phenology_highlight earlylate.pdf", height=10, width=6)
par(mfrow=c(3,1)); par(oma=c(2,2,2,0))
limz<-range(approximated_SST$Sea.Surface.Temperature..C.,na.rm=TRUE)

plot(NA,NA, col="lightgray", xlim=c(0,366), ylim= limz, xlab="", ylab="SST (C)", cex.lab=1.5, cex.axis=1.3,,xaxs="i",yaxs="i", xaxt="n")
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

#add line for each yr 
for(q in 1:length(unique(approximated_SST$Season))){
  yr1<-subset(approximated_SST, approximated_SST$Season == unique(approximated_SST$Season)[q])
  yr1<-yr1[order(yr1$Days.Sept30),]
   VAR<-rollmean(yr1$Sea.Surface.Temperature..C., k=5, fill=NA)
  lines(time(yr1$Days.Sept30), VAR, col="gray")
}

### plot mean all years 
all_data_cleaned2<-aggregate(approximated_SST$Sea.Surface.Temperature..C., by=list(approximated_SST$Days.Sept30), FUN="mean", na.rm=T)
VAR_mn<-ksmooth(time(all_data_cleaned2$Group.1), all_data_cleaned2$x, "normal", bandwidth = 50)
lines(VAR_mn, col="black", lwd=3)

# Mean for late and early ice years 
reorg_late<-approximated_SST[which(approximated_SST$Season %in% late==T),]
reorg_late<-aggregate(reorg_late, by=list(reorg_late$Days.Sept30),FUN=mean, na.rm=TRUE )
agg<-reorg_late[order(reorg_late$Days.Sept30),]
lines(ksmooth(time(agg$Group.1), agg$Sea.Surface.Temperature..C., "normal", bandwidth = 50), col="blue", lwd=2)

reorg_early<-approximated_SST[which(approximated_SST$Season %in% early==T),]
reorg_early<-aggregate(reorg_early, by=list(reorg_early$Days.Sept30),FUN=mean, na.rm=TRUE )
agg<-reorg_early[order(reorg_early$Days.Sept30),]
lines(ksmooth(time(agg$Group.1), agg$Sea.Surface.Temperature..C., "normal", bandwidth = 50), col="salmon", lwd=2)
box()

##### add mn/sd in peak/valley
pt_COL<-"green4"
lwd_id<-3; cex_id<-3
points(mean(Peaks_sst$day_of_peak), mean(Peaks_sst$value) , cex=cex_id, pch=20, col=pt_COL)
#sd for day of peak 
segments(mean(Peaks_sst$day_of_peak)- sd(Peaks_sst$day_of_peak) ,mean(Peaks_sst$value),
         mean(Peaks_sst$day_of_peak)+ sd(Peaks_sst$day_of_peak), mean(Peaks_sst$value), col=pt_COL,lwd=lwd_id)
#sd for magnitude
segments(mean(Peaks_sst$day_of_peak), mean(Peaks_sst$value)- sd(Peaks_sst$value),
         mean(Peaks_sst$day_of_peak), mean(Peaks_sst$value)+ sd(Peaks_sst$value),col=pt_COL, lwd=lwd_id)

legend("topright", c("1991-2019", "1991-2019 Average" ), lwd=c(2,3), col=c("gray", "black"), bty="n", cex=1.5)
dev.off()
#################################################################


