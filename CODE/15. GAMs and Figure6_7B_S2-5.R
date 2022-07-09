require(reshape2)
require(mgcv)
require(visreg)
library(MuMIn)
library(caret)
require(RColorBrewer)

###########
STATS = function(x,y,DATA){
   MODEL = summary(lm(DATA[,y]~DATA[,x]))
  data.frame(
    VAR=x,
    SLOPE = MODEL$coefficients[2,1],
    Model_Pval = MODEL$coefficients[2,4],
    Model_Tval = MODEL$coefficients[2,3],
    stringsAsFactors=FALSE
  )}
############

#### directories 
data_wd<-"~/Desktop/CodeData/DATA/"
plot_wd<-"~/Desktop/CodeData/PLOTS/"

####### Load Data ########
setwd(data_wd)
load("all_vars_dates_values.Rdata"); all<-all_vars_dates_values

peng <- read.xlsx("Pal Datasets.xlsx", sheet = "Adélie", skipEmptyRows = FALSE)
cfm <- read.xlsx("Pal Datasets.xlsx", sheet = "Adélie_CFM", skipEmptyRows = FALSE)

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
all_dates<-all_c[,c(1:2,6,8,10,12,14:17,19:20,22,26,28:30)]

## stats & plot retreat day vs variables
for(x in 2:ncol(all_dates)){
  dev.new()
  plot(all_dates[,x], all_dates$sea_ice_retreat, main=colnames(all_dates)[x])
  lm<-lm(all_dates$sea_ice_retreat~ all_dates[,x])
  pval<-summary(lm)$coefficients[,4] [[2]]
  legend("topright", c(as.character(pval)), pch=20)
  abline(lm)
}






############################### GAMS #############################################


###########################################################################################
#################### CFM #########################################################
###########################################################################################
v<-all_dates[,c(4,5,6,7,14,15,17)]
cor(na.omit(v))
 v2<-na.omit(v)
 
#models
 m<-gam(Weight.g~ s(SST_day_of_peak)+ (Wind_day_of_max)+(sea_ice_retreat )+ (day_start0snow)+ (Chl_day_of_max)+AirTemp_day_of_max, data=v2,family=gaussian, na.action=na.fail )
 
mod<- dredge(m,extra =list(r2 = function(x)  summary(x)$r.sq,
      dev.expl = function(x) summary(x)$dev.expl
))

 best<-subset(mod, delta <= 2)
 
 best_formula<-NULL
 for(y in 1:nrow(best)){
   formulas1<- as.character(get.models(mod, subset = y)[[1]]$formula)[3]
   best_formula<-rbind( best_formula, formulas1)
 }

 CFM_models<-cbind(best_formula, best[,8:14])
 CFM_models$dev.expl<- CFM_models$dev.expl*100
 CFM_models[,2:8]<-round( CFM_models[,2:8],2)
 setwd(data_wd)
 #write.csv(CFM_models, file="CFM_models.csv")
 
##### check and plot best models 
m1<-gam(Weight.g~ Wind_day_of_max, data=v2,family=gaussian, na.action=na.fail)
m2<-gam(Weight.g~ day_start0snow + s(SST_day_of_peak) + sea_ice_retreat, data=v2,family=gaussian, na.action=na.fail)
m3<-gam(Weight.g~ day_start0snow + Wind_day_of_max, data=v2,family=gaussian, na.action=na.fail)
m4<-gam(Weight.g~ AirTemp_day_of_max + day_start0snow + s(SST_day_of_peak) + sea_ice_retreat, data=v2,family=gaussian, na.action=na.fail)

#############for each check
mod<-m1 
summary(mod) #mod summary
par(mfrow=c(1,4)); plot.gam(mod,all.terms=T, shade=F) #partial plots 
par(mfrow=c(1,4)); gam.check(mod) #check mod 
#VARIABLE IMPORTANCE 
y<-varImp(mod)
cbind(y, (y$Overall/ sum(y$Overall)*100))

# PLOT FITTED VS OBSERVED
plot(v2$Weight.g, predict(mod), xlab = "Observed", ylab = "Fitted")
 cor(v2$Weight.g, predict(mod))
 
### as example plot m1 and m4 response curves 
setwd(plot_wd)
 pdf("CFM_model_partial_plots.pdf", height=3, width=11)
par(mfrow=c(1,5))
plot.gam(m1,all.terms=T, shade=F, main="")
plot.gam(m4,all.terms=T, shade=F, main="")
dev.off()

############## MAGNITUDE OF EFFECT OF EACH VARIBLAE IN MODEL ##########
var1_eff<- NULL
  for(y in 1:nrow(best)){
   mn_1<- mean(best$AirTemp_day_of_max[y]*v2$AirTemp_day_of_max)
   sd_1<- sd(best$AirTemp_day_of_max[y]*v2$AirTemp_day_of_max)
   var1_eff<-  rbind( var1_eff, cbind(mn_1, sd_1))
  } 

var2_eff<- NULL
for(y in 1:nrow(best)){
  mn_1<- mean(best$day_start0snow[y]*v2$day_start0snow)
  sd_1<- sd(best$day_start0snow[y]*v2$day_start0snow)
  var2_eff<-  rbind( var2_eff, cbind(mn_1, sd_1))
} 

var3_eff<- NULL
for(y in 1:nrow(best)){
  mn_1<- mean(best$sea_ice_retreat[y]*v2$sea_ice_retreat)
  sd_1<- sd(best$sea_ice_retreat[y]*v2$sea_ice_retreat)
  var3_eff<-  rbind( var3_eff, cbind(mn_1, sd_1))
} 

#run models using linear term for SST
m2_redo<-gam(WT~ day_start0snow+(SST_day_of_peak) + sea_ice_retreat, data=v2,family=gaussian)
m4_redo<-gam(WT~ AirTemp_day_of_max + day_start0snow + (SST_day_of_peak) + sea_ice_retreat, data=v2,family=gaussian)

###### results nearly identical 
var4_eff<- NULL
best_SST<-c(NA, -2.2456,NA, -2.8775 )
for(y in 1:length(best_SST)){
  mn_1<- mean(best_SST[y]*v2$SST_day_of_peak)
  sd_1<- sd(best_SST[y]*v2$SST_day_of_peak)
  var4_eff<-  rbind( var4_eff, cbind(mn_1, sd_1))
} 

var5_eff<- NULL
for(y in 1:nrow(best)){
  mn_1<- mean(best$Wind_day_of_max[y]*v2$Wind_day_of_max)
  sd_1<- sd(best$Wind_day_of_max[y]*v2$Wind_day_of_max)
  var5_eff<-  rbind( var5_eff, cbind(mn_1, sd_1))
} 

wordwrap<-function(x,len) paste(strwrap(x,width=len),collapse="\n")

####### PLOT NICE
pdf("CFM_model_effects_plots.pdf", height=6, width=10)
par(mar=c(5,12,1,2)); par(oma=c(2,1,2,16))

plot(NA,NA, ylim=c(1,20), xlim= c(-400,400),xlab="Effect on Chick Fledging Mass (CFM, g)", ylab="", yaxt="n")
abline(v=0, col="gray",lwd=2,lty=2)
palette<- "BrBG" #"Dark2"
points( var1_eff[,1],13:16, col=brewer.pal(4, palette), pch=20,cex=1.4)
points( var2_eff[,1],5:8, col=brewer.pal(4, palette), pch=20,cex=1.4)
points( var3_eff[,1],1:4, col=brewer.pal(4, palette), pch=20,cex=1.4)
points( var4_eff[,1],17:20, col=brewer.pal(4, palette), pch=20,cex=1.4)
points( var5_eff[,1],9:12, col=brewer.pal(4, palette), pch=20,cex=1.4)
abline(h=(seq(0,16,by=4)+.5)[-1], col="gray" )

segments(var1_eff[,1]-var1_eff[,2],13:16,var1_eff[,1]+var1_eff[,2],13:16, col=brewer.pal(4, palette), pch=20,lwd=2.2)
segments(var2_eff[,1]-var2_eff[,2],5:8,var2_eff[,1]+var2_eff[,2],5:8, col=brewer.pal(4, palette), pch=20,lwd=2.2)
segments(var3_eff[,1]-var3_eff[,2],1:4,var3_eff[,1]+var3_eff[,2],1:4, col=brewer.pal(4, palette), pch=20,lwd=2.2)
segments(var4_eff[,1]-var4_eff[,2],17:20,var4_eff[,1]+var4_eff[,2],17:20, col=brewer.pal(4, palette), pch=20,lwd=2.2)
segments(var5_eff[,1]-var5_eff[,2],9:12,var5_eff[,1]+var5_eff[,2],9:12, col=brewer.pal(4, palette), pch=20,lwd=2.2)

legend("topright", c("Model 7","Model 8","Model 9","Model 10" ),col=brewer.pal(4, palette), pch=20, lwd=2.2, cex=.9, bty="n")

axis(2,at= seq(2.5,18.5,by=4), sapply(c("Day of Sea Ice Retreat", "First Day without Snow", "Day of Min. Wind","Day of Max. Air Temp" , "Day of Max. SST"),wordwrap,len=15),las=2, tck=.01)
box()
dev.off()
################################################################################################




###########################################################################################
#################### NUTRIENTS #########################################################
###########################################################################################
v<-all_dates[, c(4, 6, 13:15)]
v2<-na.omit(v)
cor(v2)

#model 
m<-gam(Phos_day_of_min~(sea_ice_retreat)+(Chl_day_of_max)+Wind_day_of_max +s(SST_day_of_peak), data=v2,family=gaussian, na.action=na.fail)

mod<- dredge(m,extra =list(r2 = function(x)  summary(x)$r.sq,
   dev.expl = function(x) summary(x)$dev.expl))
best<-subset(mod, delta <= 2)

formulas1<- as.character(get.models(mod, subset = 1)[[1]]$formula)[3]

Phosphate_models<-cbind(formulas1, as.data.frame(best[,6:12])) 
Phosphate_models$dev.expl<- Phosphate_models$dev.expl*100
Phosphate_models[,2:8]<-round( Phosphate_models[,2:8],2)
setwd(data_wd)
#write.csv(Phosphate_models, file="Phosphate_models.csv")

##### check and plot best models 
m1<-gam(Phos_day_of_min~ Chl_day_of_max, data=v2,family=gaussian, na.action=na.fail)

#############for each check
mod<-m1
summary(mod) #mod summary
par(mfrow=c(1,4));plot.gam(mod,all.terms=T, shade=F)#partial plots 
par(mfrow=c(1,4)); gam.check(mod) #check mod 
#VARIABLE IMPORTANCE 
y<-varImp(mod)
cbind(y, (y$Overall/ sum(y$Overall)*100))

# PLOT FITTED VS OBSERVED
plot(v2$Phos_day_of_min, predict(mod), xlab = "Observed", ylab = "Fitted")
cor(v2$Phos_day_of_min, predict(mod))

### for paper plot m1 and m4 response curves 
setwd(plot_wd)
 pdf("Phosphate_model_partial_plots.pdf", height=5, width=5)
par(mfrow=c(1,1))
plot.gam(m1,all.terms=T, shade=F)#partial plots 
dev.off()
###########################################################################################



###########################################################################################
#################### CHL #########################################################
###########################################################################################
v<-all_dates[,c(4,6,14,15)]    
v2<-na.omit(v)
cor(v2)
m<-gam(Chl_day_of_max~s(Wind_day_of_max,k=3)+(SST_day_of_peak)+sea_ice_retreat, data=v2,family=gaussian,na.action=na.fail) 

mod<- dredge(m,extra =list(r2 = function(x)  summary(x)$r.sq,
                           dev.expl = function(x) summary(x)$dev.expl))
best<-subset(mod, delta <= 2)

best_formula<-NULL
for(y in 1:nrow(best)){
  formulas1<- as.character(get.models(mod, subset = y)[[1]]$formula)[3]
  best_formula<-rbind( best_formula, formulas1)
}

CHL_models<-cbind(best_formula, as.data.frame(best)[,5:11])
CHL_models$dev.expl<- CHL_models$dev.expl*100
CHL_models[,2:8]<-round( CHL_models[,2:8],2)
setwd(data_wd)
#write.csv(CHL_models, file="CHL_models.csv")

##### manually check and plot best models 
m1<-gam(Chl_day_of_max~ sea_ice_retreat, data=v2,family=gaussian, na.action=na.fail)
m2<-gam(Chl_day_of_max~ s(Wind_day_of_max, k = 3), data=v2,family=gaussian, na.action=na.fail)

#############for each check
mod<-m1 
summary(mod) #mod summary
par(mfrow=c(1,4));plot.gam(mod,all.terms=T, shade=F)#partial plots 
par(mfrow=c(1,4)); gam.check(mod) #check mod 
#VARIABLE IMPORTANCE 
y<-varImp(mod)
cbind(y, (y$Overall/ sum(y$Overall)*100))

# PLOT FITTED VS OBSERVED
plot(v2$Chl_day_of_max, predict(mod), xlab = "Observed", ylab = "Fitted")
cor(v2$Chl_day_of_max, predict(mod))

### for paper plot m1 and m4 response curves 
setwd(plot_wd)
pdf("CHL_models_partial_plots.pdf", height=5, width=8.5)
par(mfrow=c(1,2))
plot.gam(m1,all.terms=T, shade=F)
plot.gam(m2,all.terms=T, shade=F)
dev.off()
###########################################################################################






###########################################################################################
#################### BACTERIA #########################################################
###########################################################################################
v<-all_dates[,c(4,14,15,12)] 
v2<-na.omit(v); cor(v2)
m<-gam(BB_day_of_max~s(Chl_day_of_max)+(SST_day_of_peak)+sea_ice_retreat, data=v2, na.action=na.fail,family=gaussian) 

mod<- dredge(m,extra =list(r2 = function(x)  summary(x)$r.sq,
                           dev.expl = function(x) summary(x)$dev.expl))

best<-subset(mod, delta <= 2)

best_formula<-NULL
for(y in 1:nrow(best)){
  formulas1<- as.character(get.models(mod, subset = y)[[1]]$formula)[3]
  best_formula<-rbind( best_formula, formulas1)
}

BB_models<-cbind(best_formula, as.data.frame(best)[,5:11])
BB_models$dev.expl<- BB_models$dev.expl*100
BB_models[,2:8]<-round(BB_models[,2:8],2)
setwd(data_wd)
#write.csv(BB_models, file="BB_models.csv")

##### manually check and plot best models 
m1<-gam( BB_day_of_max~ sea_ice_retreat + SST_day_of_peak, data=v2,family=gaussian, na.action=na.fail)
m2<-gam( BB_day_of_max~ SST_day_of_peak , data=v2,family=gaussian, na.action=na.fail)

#############for each check
mod<-m1 
summary(mod) #mod summary
par(mfrow=c(1,4));plot.gam(mod,all.terms=T, shade=F)#partial plots 
par(mfrow=c(1,4)); gam.check(mod) #check mod 
#VARIABLE IMPORTANCE 
y<-varImp(mod)
cbind(y, (y$Overall/ sum(y$Overall)*100))

# PLOT FITTED VS OBSERVED
plot(v2$ BB_day_of_max, predict(mod), xlab = "Observed", ylab = "Fitted")
cor(v2$ BB_day_of_max, predict(mod))

### for paper plot m1 and m4 response curves
setwd(plot_wd)
pdf("BB_models_partial_plots.pdf", height=5, width=8.5)
par(mfrow=c(1,2))
plot.gam(m1,all.terms=T, shade=F)#partial plots 
dev.off()
###########################################################################################







################ RELATIONSHIP BETWEEN VARIABLES OF INTEREST #############################
setwd(plot_wd)
pdf("ChlvPhos_SSTvBB.pdf", height=5, width=8.5)
par(mfrow=c(1,2))
plot(all$Chl_day_of_max, all$Phos_day_of_min,xlab= "Day of Max. Chl.", ylab="Day of Min. Phosphate",pch=20)
abline(lm(all$Phos_day_of_min~all$Chl_day_of_max))
legend("bottomright", c("Adj. R^2=0.27", "p=0.02" ), bty="n")
legend("topleft", "a", bty="n")

plot(all$SST_day_of_peak, all$BB_day_of_max, xlab= "Day of Max. SST", ylab="Day of Max. Bacteria Biomass",pch=20)
abline(lm(all$BB_day_of_max~all$SST_day_of_peak))
#summary(lm(all$BB_day_of_max~all$SST_day_of_peak))
legend("bottomleft", c("Adj. R^2=0.44", "p=0.008" ), bty="n")
legend("topright", "b", bty="n")
dev.off()
###############################################################################################