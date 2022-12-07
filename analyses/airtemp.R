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
library(scales)
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
boxplot(as.numeric(alldat2$TotalBA_cm2)~Trees., data=alldat)
#Make some plots
boxplot(as.numeric(airtemp_c)~Trees., data=alldat)
alldat2$airtemp_c<-as.numeric(alldat2$airtemp_c)
alldat2$hour<-as.integer(substr(alldat2$time,1,2))
alldat2$Trees.<-as.factor(alldat2$Trees.)
alldat2$TotalBA_cm2<-as.numeric(alldat2$TotalBA_cm2)
alldat2$TotalBA_m2<-alldat2$TotalBA_cm2/10000
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
junm1a<-lm(airtemp_c~TotalBA_m2*day, data=jundat)

junm2<-lm(airtemp_c~Trees.*hour, data=jundat)
junm2a<-lm(airtemp_c~TotalBA_m2*hour, data=jundat)

junm3<-lm(airtemp_c~Trees.*hour+Elevation, data=jundat)
junm3a<-lm(airtemp_c~TotalBA_m2*hour+Elevation, data=jundat)

AIC(junm1,junm2,junm3,junm1a,junm2a,junm3a)
summary(junm3a)
summary(junm3)

#Fit some mixed effects models
junmm1<-lmer(airtemp_c~Trees.*day + (1|Hobo_SN), data=jundat)
junmm1a<-lmer(airtemp_c~TotalBA_m2*day+ (1|Hobo_SN), data=jundat)

junmm2<-lmer(airtemp_c~Trees.*hour+ (1|Hobo_SN), data=jundat)
junmm2a<-lmer(airtemp_c~TotalBA_m2*hour+ (1|Hobo_SN), data=jundat)

junmm3<-lmer(airtemp_c~Trees.*hour+Elevation+ (1|Hobo_SN), data=jundat)
junmm3a<-lmer(airtemp_c~TotalBA_m2*hour+Elevation+ (1|Hobo_SN), data=jundat)

AIC(junmm1,junmm2,junmm3,junmm1a,junmm2a,junmm3a)
summary(junmm3a)
summary(junm1a)
jundat$dom<- as.numeric(substr(jundat$date,4,5))

cols<-c("gray","seagreen3")

png("figs/airtempbvsBA.png", width=10, height=6, units="in", res=220)
plot(jundat$dom,jundat$airtemp_c, 
     pch=21,bg=alpha(cols[as.factor(jundat$Trees.)],.5),
     ylim=c(10,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Day of the month",ylab=c("Temperature (C)"), main="June Air temperature",bty="l")
legend("topleft",legend=c("Trees","No Trees"),pch=21, pt.bg=rev(cols))
dev.off()

domnames<-c("",1,"",2,"",3,"",4,"",5,"",6,"",7,"",8,"",9,"",
           10,"",11,"",12,"",13,"",14,"",15,"",16,"",17,"",18,"",19,"",
           20,"",21,"",22,"",23,"",24,"",25,"",26,"",27,"",28,"",29,"",30)
png("figs/airtempvdayJuneboxplot.png", width=10, height=6, units="in", res=220)

x<-boxplot(airtemp_c ~ Trees.+dom, beside=TRUE,data = jundat,
           col = cols, xaxs = n, names=FALSE,
           xlab="Day in June", ylab="Air Temperature (C)")

axis(1,seq(from =4, to=30, by=4), at=seq(from =8,to=60, by=8))

legend("topleft", legend = c("Trees","No Trees"),fill = rev(cols), bty="n")

dev.off()

png("figs/airtempvdaynightboxplot.png", width=8, height=8, units="in", res=220)

x<-boxplot(airtemp_c ~ Trees.+day, beside=TRUE,data = jundat,
           col = cols, xaxs = n, names=c("No Trees","Trees","No Trees","Trees"),
           xlab="", ylab="Air Temperature (C)")
axis(1,c("Night","Day"), at=c(1.5,4), tick=FALSE, line=2)
legend("topleft", legend = c("Trees","No Trees"),fill = rev(cols), bty="n")

dev.off()



jundat2<-jundat[-which(is.na(jundat$TotalBA_m2)),]
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

png("figs/temp_BA_mod.png", width=10, height=6, units="in", res=220)

plot(jundat2$TotalBA_m2,jundat2$airtemp_c, 
     pch=21,bg=alpha(cols[as.factor(jundat2$Trees.)], .5),
     ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Tree abundance (basal area)",ylab=c("Temperature (C)"), main="",bty="l")
abline(a=coef(junm3a)[1]+5, b=coef(junm3a)[2], lwd=2)#effect of trees in sun, impervious surface

dev.off()

