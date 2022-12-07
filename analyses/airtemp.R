#######################################################
### Script to look at GRIT airtemp data ###
################# August 24, 2022 #####################
################ ailene.ettinger@tnc.org ##############
#######################################################

#Currently, this script looks at pulls together all temperature data from from March through August 2022
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries
library(dplyr)
library(lme4)
library(tidyr)
library(car)
# set working directory
setwd("~/GitHub/grit/analyses")

#Read in data on all temp logger locations
locs<-read.csv("../data/HoboLocations_PoleLocations.csv", header=TRUE)
source("sourced_files/clean_locs.R")

#Read in tree data 
treedat<-read.csv("../data/HoboLocations_TreeData.csv", header=TRUE)
source("sourced_files/summarize_grittreedat.R")

#Put together all air temperature data that we have
airtempdat<-NULL
tdir<-"../data/temp_data/"
tfolds<-list.files(tdir)
for(i in tfolds){
  tempdatdir<-paste(tdir,i,sep="")
  tempfiles<-list.files(tempdatdir)
  tempfiles<-tempfiles[which(substr(tempfiles,nchar(tempfiles)-3,nchar(tempfiles))==".csv")]
  for (j in tempfiles){
    dat<-read.csv(paste(tempdatdir,"/",j,sep=""), skip=1,header=TRUE)
    colnames(dat)[2:3]<-c("date.time","airtemp_c")
    dat<-dat[,-1]
    dat$date<-substr(dat$date,1,8)
    dat$time<-substr(dat$date.time,10,20)
    dat$time<-format(strptime(dat$time, "%I:%M:%S %p"), "%H:%M:%S")
    dat$Hobo_SN<-paste(substr(j,1,nchar(j)-4))
    if(substr(j,nchar(j)-5,nchar(j)-5)=="_"){dat$Hobo_SN<-paste(substr(j,1,nchar(j)-6))}
    adat<-subset(dat,select=c(Hobo_SN,date,time, airtemp_c))
    airtempdat<-rbind(airtempdat,adat)
  }
}

dim(airtempdat)
unique(airtempdat$Hobo_SN)  
head(airtempdat)

#now format  blue tooth data
bttempdat<-read.csv("../data/BT_temp_data/AllBTLoggers_start_to_06Dec202_2022_12_06_15_49_34_PST_1.csv", header=TRUE)
#dim(bttempdat)
#colnames(bttempdat)
#bttempdat[1:5,1:3]
bttempdat<-bttempdat[,-1]
colnames(bttempdat)[2:length(colnames(bttempdat))]<-substr(colnames(bttempdat)[2:length(colnames(bttempdat))],50,nchar(colnames(bttempdat)[2:length(colnames(bttempdat))]))
#reshape to long format
bttempdat_long<-gather(bttempdat, logger, airtemp_c, colnames(bttempdat)[2:length(colnames(bttempdat))],factor_key=TRUE)
bttempdat_long$Hobo_SN<-paste("BT",bttempdat_long$logger,sep="")
bttempdat_long$date<-substr(bttempdat_long$Date,1,8)
bttempdat_long$time<-substr(bttempdat_long$Date,10,20)

btdat<-subset(bttempdat_long,select=c("Hobo_SN","date","time","airtemp_c"))
#remove the many, many rows with airtemp=NA
btdat<-btdat[-which(is.na(btdat$airtemp_c)),]
#keep only data on the hour
btdat<-btdat[which(substr(btdat$time,4,8)=="00:00"),]

#now add btdat to airtemp
allairdat<-rbind(airtempdat,btdat)

#merge in locdata
locdat2 <- locs %>% 
  select(Hobo_SN, Location, Latitude, Longitude, Elevation, Trees.) %>% 
  distinct(Location, .keep_all= TRUE)

alldat<-left_join(allairdat,locdat2, by="Hobo_SN", copy=TRUE)
alldat<-alldat[-(which(is.na(alldat$airtemp_c))),]
alldat<-alldat[-which(alldat$Hobo_SN=="21223113"),]#messed up data!
alldat<-alldat[-which(alldat$airtemp_c>36),]#inaccurate temperature readings

###some serial numbers do not line up (e.g., we ave temp data, but no location matches up to them).
###These are: unique(aldat$Hobo_SN[which(is.na(aldat$Longitude))])
# "21162467"   "21223102"   "21223125"   "BT21302946" "BT21302957" "BT21302964" "BT21302976"
#needs to be corrected but for now just removing these

alldat<-alldat[-(which(is.na(alldat$Longitude))),]


#Merge in tree data
alldat2<-left_join(alldat,sumba,copy=TRUE)
#check that locations with trees have more bai:
boxplot(as.numeric(aldat2$TotalBA_cm2)~Trees., data=aldat)
#Make some plots
boxplot(as.numeric(airtemp_c)~Trees., data=aldat)
alldat2$airtemp_c<-as.numeric(alldat2$airtemp_c)
alldat2$hour<-as.integer(substr(alldat2$time,1,2))
alldat2$Trees.<-as.factor(alldat2$Trees.)
alldat2$TotalBA_cm2<-as.numeric(alldat2$TotalBA_cm2)
aldat2$TotalBA_m2<-aldat2$TotalBA_cm2/10000
alldat2$Hobo_SN <-as.factor(alldat2$Hobo_SN)
alldat2$month<-as.factor(as.character(substr(alldat2$date,1,2)))
alldat2$year<-as.factor(as.character(paste("20",substr(alldat2$date,7,8), sep="")))
alldat2$day<-0
alldat2$day[alldat2$hour>6 & alldat2$hour<19]<-1
juldat<-alldat2[alldat2$month=="07",]
jundat<-alldat2[alldat2$month=="06",]

boxplot(jundat$airtemp_c[jundat$day==1]~jundat$Trees.[jundat$day==1])
boxplot(jundat$airtemp_c[jundat$day==0]~jundat$Trees.[jundat$day==0])
#Which logger recorded the hottest temperature and when was it?
jundat[which(jundat$airtemp_c==max(jundat$airtemp_c)),]
juldat[which(juldat$airtemp_c==max(juldat$airtemp_c)),]

head(jundat[jundat$date=="06/27/22",])
#Fit some models
junm1<-lm(airtemp_c~Trees.*day, data=jundat)
junm1a<-lm(airtemp_c~TotalBA_cm2*day, data=jundat)

junm2<-lm(airtemp_c~Trees.*hour, data=jundat)
junm2a<-lm(airtemp_c~TotalBA_cm2*hour, data=jundat)

junm3<-lm(airtemp_c~Trees.*hour+Elevation, data=jundat)
junm3a<-lm(airtemp_c~TotalBA_cm2*hour+Elevation, data=jundat)

AIC(junm1,junm2,junm3,junm1a,junm2a,junm3a)


cols<-c("gray","darkgreen")
shapes<-c(24,21)
png("figs/tempblitzdat_surfair.png", width=10, height=6, units="in", res=220)
par(mfrow=c(1,2))
plot(asldat_long2$hour[asldat_long2$temptype=="airtemp_c"],asldat_long2$temp_c[asldat_long2$temptype=="airtemp_c"], 
     pch=21,bg=cols[as.factor(asldat_long2$Trees.)],
     ylim=c(15,55),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Time of day (hr)",ylab=c("Temperature (C)"), main="Air temperature",bty="l")
legend("topleft",legend=c("Trees","No Trees"),pch=24, pt.bg=rev(cols))

plot(asldat_long2$hour[asldat_long2$temptype=="surftemp_c"],asldat_long2$temp_c[asldat_long2$temptype=="surftemp_c"], 
     pch=24,bg=cols[as.factor(asldat_long2$Trees.)],
     ylim=c(15,55),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Time of day (hr)",ylab="Temperature (C)", main="Surface temperature", bty="l")
dev.off()
cols<-c("gray","darkgreen")
shapes<-c(24,21)
png("figs/tempblitzdat_BA.png", width=10, height=6, units="in", res=220)
par(mfrow=c(1,2))

plot(asldat_long2$TotalBA_cm2[asldat_long2$temptype=="surftemp_c"],asldat_long2$temp_c[asldat_long2$temptype=="surftemp_c"], 
     pch=24,bg=cols[as.factor(asldat_long2$Trees.)],
     xlab="Tree abundance (total basal area, cm2)",ylab="Temperature (C)", main="Surface temp", bty="l")
legend("topright",legend=c("Trees","No Trees"),pch=24, pt.bg=rev(cols))
plot(asldat_long2$TotalBA_cm2[asldat_long2$temptype=="airtemp_c"],asldat_long2$temp_c[asldat_long2$temptype=="airtemp_c"], 
     pch=21,bg=cols[as.factor(asldat_long2$Trees.)],
     xlab="Tree abundance (total basal area, cm2)",ylab=c("Temperature (C)"), main="Air temp",bty="l")
dev.off()

asldat_long2<-asldat_long2[-which(is.na(asldat_long2$TotalBA_cm2)),]
summary(mm12b)
cbind(coef(m12b), fixef(mm12b))
plot(mm12b)
summary(m12b)

#plot conditional effects
# 
# interplot(
#   mm12b,
#   "TotalBA_m2",
#  "temptype", data=asldat_long2)
# 
# interplot(m =  mm12b, var1 ="temptype", var2 = "TotalBA_m2", )
# 

png("figs/tempblitzdat_BA_mod.png", width=10, height=6, units="in", res=220)
par(mfrow=c(1,2))

plot(asldat_long2$TotalBA_m2[asldat_long2$temptype=="airtemp_c"],asldat_long2$temp_c[asldat_long2$temptype=="airtemp_c"], 
     pch=24,bg=cols[as.factor(asldat_long2$Trees.)],
     ylim=c(15,55),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Tree abundance",ylab=c("Temperature (C)"), main="Air temperature",bty="l")
abline(coef(m12b)[1]+coef(m12b)[3]+coef(m12b)[10],coef(m12b)[2]+coef(m12b)[8], lwd=2)#effect of trees in sun, impervious surface

plot(asldat_long2$TotalBA_m2[asldat_long2$temptype=="surftemp_c"],asldat_long2$temp_c[asldat_long2$temptype=="surftemp_c"], 
     pch=24,bg=cols[as.factor(asldat_long2$Trees.)],
     ylim=c(15,55),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Tree abundance",ylab="Temperature (C)", main="Surface temperature", bty="l")

abline(coef(m12b)[1]+coef(m12b)[10],coef(m12b)[2], lwd=2)#effect of trees in sun, impervious surface

dev.off()

