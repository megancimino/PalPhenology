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
concent <- read.xlsx("Pal Datasets.xlsx", sheet = "SeaIce_Concentration", skipEmptyRows = FALSE)
ret_adv <- read.xlsx("Pal Datasets.xlsx", sheet = "SeaIce_Advance_Retreat", skipEmptyRows = FALSE)





##############  FIGURE #####################  

######## Labeling dates
dates<-as.Date(c("2020-10-01", "2020-11-01" ,"2020-12-01" ,"2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01" ,"2021-09-01"))
#day that corresponds to 1st of the month
day_since_sept30<-c(1, 32, 62 ,93, 124, 152, 183, 213, 244, 274, 305, 336)

setwd(plot_wd)
pdf("Sea ice_summary_phenology_Anvers200km_DAILY_SIC_dateRET_ADV_highlight earlylate.pdf", height=10, width=6)

par(mfrow=c(3,1)); par(oma=c(2,2,2,0))
plot(NA,NA, col="lightgray", xlim=c(0,366), ylim= range(concent$SeaIce_concentration, na.rm=TRUE), xlab="", ylab="Sea Ice Concentration (%)", cex.lab=1.5, cex.axis=1.3,,xaxs="i",yaxs="i", xaxt="n")
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
concent2<-concent[which(concent$Season %in% unique(concent$Season)[13:41]==T),]

for(q in 1:length(unique(concent2$Season))){
  yr1<-subset(concent2, concent2$Season == unique(concent2$Season)[q])
  yr1<-yr1[order(yr1$Days.Sept30),]
   VAR<-rollmean( yr1$SeaIce_concentration, k=5,fill=NA) #rollmean to be consisten with weather variables
    lines(time(yr1$Days.Sept30),VAR, col="gray")
}
agg_ice<-aggregate(concent2, by=list(concent2$Days.Sept30),FUN=mean, na.rm=TRUE )
agg_ice<-agg_ice[order(agg_ice$Days.Sept30),]
lines(ksmooth(time(agg_ice$Days.Sept30), agg_ice$SeaIce_concentration, "normal", bandwidth = 50), col="black", lwd=3)

####### add early and late ice years
ice_reorg_late<-concent2[which(concent2$Season %in% late==T),]
ice_reorg_late<-aggregate(ice_reorg_late, by=list(ice_reorg_late$Days.Sept30),FUN=mean, na.rm=TRUE )
agg_ice<-ice_reorg_late[order(ice_reorg_late$Days.Sept30),]
lines(ksmooth(time(agg_ice$Days.Sept30), agg_ice$SeaIce_concentration, "normal", bandwidth = 50), col="blue", lwd=2)
  
ice_reorg_early<-concent2[which(concent2$Season %in% early==T),]
ice_reorg_early<-aggregate(ice_reorg_early, by=list(ice_reorg_early$Days.Sept30),FUN=mean, na.rm=TRUE )
agg_ice<-ice_reorg_early[order(ice_reorg_early$Days.Sept30),]
lines(ksmooth(time(agg_ice$Days.Sept30), agg_ice$SeaIce_concentration, "normal", bandwidth = 50), col="salmon", lwd=2)
box()

##### add mn/sd in date
lwd_id<-3; cex_id<-3; pt_COL<-"green4"
mn_ret<- mean(ret_adv$Retreat_Days.Sept30[13:41])
sd_ret<- sd(ret_adv$Retreat_Days.Sept30[13:41])
mn_adv<-mean(ret_adv$Advance_Days.Sept30[13:41])
sd_adv<-sd(ret_adv$Advance_Days.Sept30[13:41])

points(mn_ret  , 5, cex=cex_id, pch=20, col=pt_COL)
points(mn_adv  , 5, cex=cex_id, pch=20, col=pt_COL)
segments(mn_ret- sd_ret, 5, mn_ret+sd_ret, 5, col=pt_COL,lwd=lwd_id)
segments(mn_adv- sd_adv, 5, mn_adv+sd_adv, 5, col=pt_COL,lwd=lwd_id)
dev.off()
#################################################################










