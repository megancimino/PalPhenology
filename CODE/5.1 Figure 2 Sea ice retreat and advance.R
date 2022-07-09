require(lubridate)
require(zoo)
require(ggplot2)
require(reshape2)

####### early vs lat ice yrs. 
late <- c(1991, 1994, 2004, 2013, 2015) 
early<- c (1992, 1998, 2007, 2008, 2010)

#### directories 
data_wd<-"~/Desktop/CodeData/DATA/"
plot_wd<-"~/Desktop/CodeData/PLOTS/"

####### Load Data ########
setwd(data_wd)
concent <- read.xlsx("Pal Datasets.xlsx", sheet = "SeaIce_Concentration", skipEmptyRows = FALSE)
ret_adv <- read.xlsx("Pal Datasets.xlsx", sheet = "SeaIce_Advance_Retreat", skipEmptyRows = FALSE)






############### Plot - sea ice retreat and advance #################
setwd(plot_wd)

pdf("Sea Ice Retreat and Duration Trends.pdf",height=9, width=6)
par(mfrow=c(2,1))
par(mar=c(2.1, 4.1, 5.1, 2.1))

#ADVANCE
plot(ret_adv$Season, as.Date(ret_adv$Advance_DayofYear), type="o",pch=20, xlab="", ylab="Date of Sea Ice Advance", ylim=c(85,225))
     rect(2009, 100, 2016, 500, col="lightgray",border=NA)
     points(ret_adv$Season, as.Date(ret_adv$Advance_DayofYear), type="o",pch=20)
     
     trend_all_yrs<-lm( ret_adv$Advance_DayofYear ~ ret_adv$Season)
     trend_79_10<-lm( ret_adv$Advance_DayofYear[1:32] ~ ret_adv$Season[1:32])
     trend_91_19<-lm(ret_adv$Advance_DayofYear[13:41] ~ ret_adv$Season[13:41])
     
     #coef(trend_all_yrs)[2] 
     #coef(trend_79_10)[2] 
     #coef(trend_91_19)[2] 
     legend("bottomright", c("1979-2019:  0.70 days/yr",
                             "1979-2010:  1.48 days/yr",
                             "1991-2019:  0.20 days/yr"),lty=c(1:3), bg="white") #,border=NA)
     
     abline(trend_all_yrs)# all years
     clip(1979,2010,140,400)
     abline(trend_79_10, lty=2)# 1979 - 2010
     clip(1991,2019,140,400)
     abline(trend_91_19,lty=3)# 20
     
     segments(1991,  220,2022, 220, lwd=3 )
     box()

############### REATREAT
 par(mar=c(6.1, 4.1, 1.1, 2.1))
    
plot(ret_adv$Season, as.Date(ret_adv$Retreat_DayofYear), type="o",pch=20, xlab="Season", ylab="Date of Sea Ice Retreat", ylim=c(140,400))
rect(2009, 100, 2016, 500, col="lightgray",border=NA)
points(ret_adv$Season, as.Date(ret_adv$Retreat_DayofYear), type="o",pch=20)

trend_all_yrs<-lm( ret_adv$Retreat_DayofYear ~ ret_adv$Season)
trend_79_10<-lm( ret_adv$Retreat_DayofYear[1:32] ~ ret_adv$Season[1:32])
trend_91_19<-lm(ret_adv$Retreat_DayofYear[13:41] ~ ret_adv$Season[13:41])

#coef(trend_all_yrs)[2]
#coef(trend_79_10)[2] 
#coef(trend_91_19)[2] 
legend("bottomright", c("1979-2019:  -0.16 days/yr",
                        "1979-2010:  -1.08 days/yr",
                        "1991-2019:  0.17 days/yr"),lty=c(1:3), bg="white") #,border=NA)

abline(trend_all_yrs)# all years
clip(1979,2010,140,400)
abline(trend_79_10, lty=2)# 1979 - 2010
clip(1991,2019,140,400)
abline(trend_91_19,lty=3)# 20

text("Ice Rebuild", x=2012.5, y=250, cex=.9)
text("LTER study period", x=2000, y=390, cex=.9)
segments(1991,  380,2022, 380, lwd=3 )
box()
dev.off()
###########################################################################







