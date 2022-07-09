require(reshape2)

#### directories 
data_wd<-"~/Desktop/CodeData/DATA/"
plot_wd<-"~/Desktop/CodeData/PLOTS/"
code_wd<-"~/Desktop/CodeData/CODE/"

setwd(code_wd)
source('boxtext.R')
####### Load Data ########
setwd(data_wd)
load("all_vars_dates_values.Rdata"); all<-all_vars_dates_values

peng <- read.xlsx("Pal Datasets.xlsx", sheet = "Adélie", skipEmptyRows = FALSE)
cfm <- read.xlsx("Pal Datasets.xlsx", sheet = "Adélie_CFM", skipEmptyRows = FALSE)
##cfm$Season[order(cfm[,2])]

ret_adv <- read.xlsx("Pal Datasets.xlsx", sheet = "SeaIce_Advance_Retreat", skipEmptyRows = FALSE)
day_of_seaice_retreat<- ret_adv[,c(1,7)] 
day_of_seaice_advance<- ret_adv[,c(1,4)]

### Add sea ice to dataset
all<-merge(all, day_of_seaice_retreat, by.x ="season", by.y ="Season")
colnames(all)[28]<-"sea_ice_retreat"

all<-merge(all, day_of_seaice_advance, by.x ="season", by.y ="Season")
colnames(all)[29]<-"sea_ice_advance"

all_c<-merge(all, cfm, by.x="season", by.y="Season", all=T)
################

#subset
all_dates<-all_c[,c(1:2,6,8,10,12,14,15,17,19:20,22,24,26,28:29)]



######### PLOT CID minus each variable ################################

#subtract dates from CID 
all_dates[3:ncol(all_dates)] <- all_dates[3:ncol(all_dates)]-all_dates[,2]
peng[3:ncol(peng)] <- peng[3:ncol(peng)]-peng[,2]

# take mean and sd of all dates for eahc variable 
mns<-melt(colMeans(all_dates[,3:ncol(all_dates)], na.rm=T))
sds<-melt(apply(all_dates[,3:ncol(all_dates)], 2, sd, na.rm=T))

mns_peng<-melt(colMeans(peng[,3:ncol(peng)], na.rm=T))

mn_sd<-cbind(mns, sds)
colnames(mn_sd)<-c("mn", "sd")
mn_sd2<-mn_sd[order(mn_sd[,1]),]

#rownames(mn_sd2)
NICE_labels <-c( "Day of Sea Ice Retreat", "First Day without Snow" ,  
   "Day of Min. Wind" ,    "Day of Max. Chl" ,
  "Day of Min. MLD","Day of Max. Air Temp." ,
  "Day of Max. SST" , "Day of Max. Flux"   , 
  "Day of Min Phosphate",    "Day of Max. Bacteria Biomass" ,   
  "Day of Max. Whales" , "Final Day without Snow",             
  "Day of Pteropod Appearance", "Day of Sea Ice Advance" )



############ PLOT ############
setwd(plot_wd)
pdf("Pheno_penguin_perspective_CFMhighlow.pdf", height=6, width=10)

par(mar=c(5,12,1,2)); par(oma=c(2,2,2,6))
plot( mn_sd2[,1],1:length(rownames(mn_sd2)), xlab="Number of Days after Clutch Initiation", ylab="", yaxt="n", xlim=c(-20,250),pch=20, col="black",cex=1.4)
segments(mn_sd2[,1]+mn_sd2[,2],1:length(rownames(mn_sd2)),   mn_sd2[,1]-mn_sd2[,2],1:length(rownames(mn_sd2)),col="black",lwd=2.2)
axis(2, at=1:length(rownames(mn_sd2)), labels= NICE_labels, las=2, tck=.01)

#abline for hatch, hatch+20 and fledge mean days
abline(v=c(0,mns_peng[,1]), lty=2, col="bisque4",lwd=2)
text( x=c(0,mns_peng[,1]),
     y = par("usr")[4] + 0.27,
     labels = c("Lay", "Hatch", "Hatch + 20d", "Fledge"),
     xpd = NA, 
     srt = 30,  cex = .8, adj = 0.1)

boxtext(x = c(0,mns_peng[,1])-c(10,12,7,10), y = c(14,14,14,14), labels = c("(Nov. 14)", "(Dec. 19)", "(Jan. 8)","(Feb. 11)"),cex=.7 ,  col.bg = "white", pos = 4, padding = 0.1)

#top high 3 CFM yrs:2001 1995 2008 
#top low 3 CFM yrs: 2003 2002 2013 
low<-melt(colMeans(all_dates[which(all_dates$season %in% c(2001 ,1995, 2008)),3:length(all_dates)],na.rm=T))
low[c(1:2,6:11,14),1]<-NA #only keep vars in CFM model 
low<-low[order(mn_sd[,1]),]
high<-melt(colMeans(all_dates[which(all_dates$season %in% c(2003 ,2002, 2013 )),3:length(all_dates)],na.rm=T))
high[c(1:2,6:11,14),1]<-NA #only keep vars in CFM model 
high<-high[order(mn_sd[,1]),]

points( low,1:length(rownames(mn_sd2))+.3, col="darkseagreen", pch =2,lwd=1.7,cex=1.4 )
points( high,1:length(rownames(mn_sd2))+.3, col="hotpink", pch=2,lwd=1.7 ,cex=1.4 )

legend("bottomright", c("Low CFM (1995, 2001, 2008)","High CFM (2002, 2003, 2013)"), pch=c(2),bty="n", col=c( "darkseagreen", "hotpink"), text.col=c("black"),lwd=1.5, lty=0)
box()
dev.off()
########################################################################################

