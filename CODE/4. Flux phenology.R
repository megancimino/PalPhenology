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
flux <- read.xlsx("Pal Datasets.xlsx", sheet = "Flux", skipEmptyRows = FALSE)


####### Loop through each season to pull out min/max for variable of interest #####
flux_max<-NULL
for(g in 1:length(unique(flux$Season))){

this_yr<-unique(flux$Season)[g]
v1<-subset(flux, flux$Season == this_yr)
v1<-v1[order(v1$Days.Sept30),]

###### calculate max
flux_max<-rbind(flux_max, rbind(
c("Flux",  this_yr, v1$Days.Sept30[which.max(v1[,6])], v1[,6][which.max(v1[,6])])))
#dev.new()
#plot(v1$Days.Sept30, v1[,6], type="o", main=paste("z=", g, this_yr, sep=" "))
#abline(v= v1$Days.Sept30[which.max(v1[,6])], col="green")
}

colnames(flux_max)<-c("variable","season","day_of_max", "max_flux")
flux_max<-as.data.frame(flux_max)
flux_max[,2]<-as.numeric(as.character(flux_max[,2]))
flux_max[,3]<-as.numeric(as.character(flux_max[,3]))
flux_max[,4]<-as.numeric(as.character(flux_max[,4]))

#remove yrs that likely miss peak flux 
flux_max[flux_max$season %in% c(2000,2001, 2008, 2009,2013, 2014, 2015),3]<-NA
flux_max[flux_max$season %in% c(2000, 2001, 2008, 2009, 2013,2014, 2015),4]<-NA
#save(flux_max, file="flux_max.Rdata")






##############  FIGURE #####################  

######## Labeling dates
dates<-as.Date(c("2020-10-01", "2020-11-01" ,"2020-12-01" ,"2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01" ,"2021-09-01"))
#day that coresponds to 1st of the month
day_since_sept30<-c(1, 32, 62 ,93, 124, 152, 183, 213, 244, 274, 305, 336)

setwd(plot_wd)
pdf("FLUX_summary_phenology.pdf", height=10, width=6)

par(mfrow=c(3,1))
par(oma=c(2,2,2,0))

plot(NA,NA, col="lightgray", xlim=c(0,366), ylim= range(flux[,6], na.rm=TRUE), xlab="", ylab="POC Flux (mg C/m^2/day)", cex.lab=1.5, cex.axis=1.3,xaxs="i",yaxs="i", xaxt="n")
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
for(q in 1:length(unique(flux$Season))){
yr1<-subset(flux, flux$Season == unique(flux$Season)[q])
if((yr1$Season[1] %in% c(2000, 2001, 2008, 2009, 2013,2014, 2015))==F ){
  print(yr1$Season[1] )
yr1<-yr1[order(yr1$Days.Sept30),]
lines(yr1$Days.Sept30, yr1[,6], col="gray")
}}
box()


zzz<-flux[order(flux$Days.Sept30),]
#remove years with incomplete data 
zzz<-zzz[-which(zzz$Season %in% c(2000, 2001, 2008, 2009, 2013,2014, 2015)==T),]
roundJD<-  round(zzz$Days.Sept30, digits=-1) #rounds to nearest 10 
aaa<-aggregate(zzz[,c(6)], by=list(roundJD), FUN=mean, na.rm=T)
lines(aaa[,1], aaa[,2], col="black", lwd=3)

##### add peak/valley
lwd_id<-3; cex_id<-3; pt_COL<-"green4"

points(mean(flux_max$day_of_max, na.rm=TRUE), mean(flux_max$max_flux, na.rm=TRUE) , cex=cex_id, pch=20, col=pt_COL)
#sd for day of peak 
segments(mean(flux_max$day_of_max, na.rm=TRUE)- sd(flux_max$day_of_max, na.rm=TRUE) ,mean(flux_max$max_flux, na.rm=TRUE),
 mean(flux_max$day_of_max, na.rm=TRUE)+ sd(flux_max$day_of_max, na.rm=TRUE), mean(flux_max$max_flux, na.rm=TRUE), col=pt_COL,lwd=lwd_id)
#sd for magnitude of wind speed 
segments(mean(flux_max$day_of_max, na.rm=TRUE), mean(flux_max$max_flux, na.rm=TRUE)- sd(flux_max$max_flux, na.rm=TRUE),
mean(flux_max$day_of_max, na.rm=TRUE), mean(flux_max$max_flux, na.rm=TRUE)+ sd(flux_max$max_flux, na.rm=TRUE),col=pt_COL, lwd=lwd_id)

legend("topright", c("1992-2012", "1992-2012 Average" ), lwd=c(2,3), col=c("gray", "black"), bty="n", cex=1.5)

dev.off()
#################################################################















