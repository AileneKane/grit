#######################################################
### Script to look at temp blitz data (Aug 11, 2022)###
################# August 24, 2022 #####################
################ ailene.ettinger@tnc.org ##############
#######################################################


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

#read in data inputs from people (w/ surface temp loggers) from temperature blitz day (August 8, 2022 from 8am-4pm)
surflogs<-read.csv("../data/TempBlitzFormData.csv", header=TRUE)
source("sourced_files/clean_tempblitzformdata.R")

#read in surface temperature logger data (exported from hoblink website)
surftemps<-read.csv("../data/BT_temp_data/tempblitz/Temperature_blitz_2022_08_31_20_14_42_PDT_surfacetemploggers.csv", header=TRUE)
source("sourced_files/clean_surftemps.R")

#merge the location data with the surf logger locations, using the logger sn
tblocdat<-left_join(surflogs,locs, copy=TRUE, keep = FALSE)
surfsns<-sort(unique(tblocdat$Your.Temperature.Logger..))

#First, build the surface logger dataset by reading in temperature data from surface temperature loggers used by volunteers
surftimes<-subset(tblocdat, select=c(Your.Temperature.Logger..,End.time..hh.mm.AM.PM.,Hobo_SN))
colnames(surftimes)<-c("surfloggersn","time","Hobo_SN")

#merge in surftemp data
surftempdat<-NULL

for(i in surfsns){
   #select the times for which this temp loggers was aligned with an air temp logger
  thissurftime<-surftimes[surftimes$surfloggersn==i,]
  thissurftime$time<-as.character(thissurftime$time)
  #skip 
  if(length(unique(colnames(surftemps)==i))==1){next}
  thissurftemp<-as.data.frame(cbind(surftemps$Date,surftemps$time,surftemps[,which(colnames(surftemps)==i)]))
  colnames(thissurftemp)<-c("date","time",paste(i))
  thissurftemp$time<-as.character(thissurftemp$time)
  #get rid of weird space
  thissurftemp$time[grep(" 8:",thissurftemp$time,)]<-gsub(" 8:","8:",thissurftemp$time[grep(" 8:",thissurftemp$time,)])
  thissurftemp$time[grep(" 9:",thissurftemp$time,)]<-gsub(" 9:","9:",thissurftemp$time[grep(" 9:",thissurftemp$time,)])
  allthisdat<-left_join(thissurftime,thissurftemp,by="time")
  colnames(allthisdat)[5]<-"surftemp_c"
  surftempdat<-rbind(surftempdat,allthisdat)
  }

#now put together all relevant air temperature data on 8/11/22
airtempdat<-NULL

airsns<-sort(unique(surftempdat$Hobo_SN))
for(j in airsns){
if(substr(j,1,2)=="BT"){
  tempdatdir<-"../data/BT_temp_data/tempblitz"
  focalsn<-substr(j,3,nchar(j))
  tempfiles<-list.files(tempdatdir)
  tempfiles<-tempfiles[which(substr(tempfiles,nchar(tempfiles)-3,nchar(tempfiles))==".csv")]
  focalsnfile<-tempfiles[grep(focalsn,tempfiles)]
  if(length(focalsnfile)>1){focalsnfile<-focalsnfile[2]}
  dat<-read.csv(paste(tempdatdir,"/",focalsnfile,sep=""), skip=1,header=TRUE)
  colnames(dat)[2:3]<-c("date.time","airtemp_c")
  dat<-dat[,-1]
  dat$date<-substr(dat$date,1,10)
  dat$time<-substr(dat$date.time,12,20)
  #head(dat)
  dat<-dat[dat$date=="08/11/2022",]
  if(dim(dat[grep("08/11/2022",dat$date.time),])[1]==0){next}
    #tempdatdir<-"../data/temp_data/2022_08_12"
    #focalsn<-j
    
  }
if(substr(j,1,2)!="BT"){
  tempdatdir<-"../data/temp_data/2022_08_10"
  focalsn<-j
tempfiles<-list.files(tempdatdir)
tempfiles<-tempfiles[which(substr(tempfiles,nchar(tempfiles)-3,nchar(tempfiles))==".csv")]
#if no data file exists with this sn, try a different folder
if(length(tempfiles[grep(focalsn,tempfiles)])==0){
  tempdatdir<-"../data/temp_data/2022_08_12"
  focalsn<-j
  tempfiles<-list.files(tempdatdir)
  tempfiles<-tempfiles[which(substr(tempfiles,nchar(tempfiles)-3,nchar(tempfiles))==".csv")]
}
focalsnfile<-tempfiles[grep(focalsn,tempfiles)]
dat<-read.csv(paste(tempdatdir,"/",tempfiles[grep(focalsnfile,tempfiles)],sep=""), skip=1,header=TRUE)
colnames(dat)[2:3]<-c("date.time","airtemp_c")
#if no data from temp blitz in this datafile, try a different folder
if(dim(dat[grep("08/11/2022",dat$date.time),])[1]==0){
  tempdatdir<-"../data/temp_data/2022_08_12"
  focalsn<-j
  tempfiles<-list.files(tempdatdir)
  tempfiles<-tempfiles[which(substr(tempfiles,nchar(tempfiles)-3,nchar(tempfiles))==".csv")]
  focalsnfile<-tempfiles[grep(focalsn,tempfiles)]
  dat<-read.csv(paste(tempdatdir,"/",tempfiles[grep(focalsnfile,tempfiles)],sep=""), skip=1,header=TRUE)
  colnames(dat)[2:3]<-c("date.time","airtemp_c")
  }
dat<-dat[,-1]
dat$date<-substr(dat$date,1,8)
dat$time<-substr(dat$date.time,10,20)
dat$time<-format(strptime(dat$time, "%I:%M:%S %p"), "%H:%M:%S")
#head(dat)
dat<-dat[dat$date=="08/11/22",]
}
dat$Hobo_SN<-paste(j)
adat<-subset(dat,select=c(Hobo_SN,time, airtemp_c))
airtempdat<-rbind(airtempdat,adat)
}
#get time in same format
airtempdat$time<-format(strptime(airtempdat$time, "%H:%M:%S"), "%H:%M")
surftempdat$time<-format(strptime(surftempdat$time, "%H:%M"), "%H:%M")

#now merge air temp into surf temp
airsurdat<-left_join(surftempdat,airtempdat)
locdat2 <- tblocdat %>% 
  select(Hobo_SN, Location, Latitude, Longitude, Elevation, Trees.,surftype, sunshade)%>% 
  distinct(Location, .keep_all= TRUE)
asldat<-left_join(airsurdat,locdat2, by="Hobo_SN", copy=TRUE)
asldat<-asldat[-(which(is.na(asldat$airtemp_c))),]
asldat<-asldat[-which(asldat$Hobo_SN=="21223113"),]

#need to figure out why row45 is NA...for now, remove it
asldat<-asldat[-(which(is.na(asldat$Latitude))),]

asldat_long<-gather(asldat, temptype, temp_c, surftemp_c:airtemp_c,factor_key=TRUE)
asldat_long$trees.temptype<-paste(asldat_long$Trees.,asldat_long$temptype, sep=".")

#remove data from one logger that failed to accurately record temperature:
#Make some plots
boxplot(as.numeric(surftemp_c)~Trees., data=asldat)
boxplot(as.numeric(airtemp_c)~Trees., data=asldat)
boxplot(as.numeric(temp_c)~trees.temptype, data=asldat_long)

asldat_long$hour<-as.integer(substr(asldat_long$time,1,2))
asldat_long$temp_c<-as.numeric(asldat_long$temp_c)
asldat_long$Trees.<-as.factor(asldat_long$Trees.)
cols<-c("gray","darkgreen")
shapes<-c(24,21)
png("figs/tempblitzdat.png", width=4, height=6, units="in", res=220)

plot(asldat_long$hour,asldat_long$temp_c, 
     pch=shapes[as.factor(asldat_long$temptype)],bg=cols[as.factor(asldat_long$Trees.)],
     xlab="Time of day (hr)",ylab=c("Temperature (C)"), bty="l")
dev.off()
#fit some models
m1<-lm(temp_c~Trees.*temptype, data=asldat_long)
m2<-lm(temp_c~Trees.*temptype+hour, data=asldat_long)
m3<-lm(temp_c~Trees.*temptype+hour +surftype, data=asldat_long)
m4<-lm(temp_c~Trees.*temptype+hour +surftype+sunshade, data=asldat_long)
m4a<-lm(temp_c~Trees.+temptype+hour +surftype+sunshade, data=asldat_long)
m5<-lm(temp_c~Trees.+temptype+hour+surftype+sunshade+Trees.:temptype+Trees.:hour, data=asldat_long)
m6<-lm(temp_c~Trees.+temptype+hour+surftype+sunshade+Trees.:temptype+Trees.:hour + Trees.:surftype, data=asldat_long)

AIC(m1,m2,m3,m4,m5,m6)#m4 wins based on AIC
summary(m4)
plot(m4)
Anova(m4)
anova(m4)

#next step is to add in the amount of trees (basal area)- not just yes/no for trees
