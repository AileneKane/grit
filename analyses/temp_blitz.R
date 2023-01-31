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

#read in surface temperature logger data (exported from hobolink website)
surftemps<-read.csv("../data/BT_temp_data/tempblitz/Temperature_blitz_2022_08_31_20_14_42_PDT_surfacetemploggers.csv", header=TRUE)
source("sourced_files/clean_surftemps.R")

#Read in tree data 
treedat<-read.csv("../data/HoboLocations_TreeData.csv", header=TRUE)
source("sourced_files/summarize_grittreedat.R")

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
  select(Hobo_SN, Location, Latitude, Longitude, Elevation, Trees.,surftype, sunshade, impervious) %>% 
  distinct(Location, .keep_all= TRUE)

asldat<-left_join(airsurdat,locdat2, by="Hobo_SN", copy=TRUE)
asldat<-asldat[-(which(is.na(asldat$airtemp_c))),]
asldat<-asldat[-which(asldat$Hobo_SN=="21223113"),]

#need to figure out why row45 is NA...for now, remove it
asldat<-asldat[-(which(is.na(asldat$Latitude))),]

#Merge in tree data
asldat_long<-gather(asldat, temptype, temp_c, surftemp_c:airtemp_c,factor_key=TRUE)
asldat_long$trees.temptype<-paste(asldat_long$Trees.,asldat_long$temptype, sep=".")
asldat_long2<-left_join(asldat_long,sumba,copy=TRUE)
#check that locations with trees have more bai:
boxplot(as.numeric(asldat_long2$TotalBA_cm2)~Trees., data=asldat_long2)
#remove data from one logger that failed to accurately record temperature:
#Make some plots
boxplot(as.numeric(surftemp_c)~Trees., data=asldat)
boxplot(as.numeric(airtemp_c)~Trees., data=asldat)
boxplot(as.numeric(temp_c)~trees.temptype, data=asldat_long)

asldat_long2$hour<-as.integer(substr(asldat_long2$time,1,2))
asldat_long2$temp_c<-as.numeric(asldat_long2$temp_c)
asldat_long2$Trees.<-as.factor(asldat_long2$Trees.)
asldat_long2$TotalBA_cm2<-as.numeric(asldat_long2$TotalBA_cm2)
asldat_long2$TotalBA_m2<-asldat_long2$TotalBA_cm2/10000
asldat_long2$Hobo_SN <-as.factor(asldat_long2$Hobo_SN)
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
#fit some models
m1<-lm(temp_c~Trees.*temptype, data=asldat_long2)
m1a<-lm(temp_c~TotalBA_m2*temptype, data=asldat_long2)

m2<-lm(temp_c~Trees.*temptype+hour, data=asldat_long2)
m2a<-lm(temp_c~TotalBA_m2*temptype+hour, data=asldat_long2)

m3<-lm(temp_c~Trees.*temptype+hour +surftype, data=asldat_long2)
m3a<-lm(temp_c~TotalBA_m2*temptype+hour +surftype, data=asldat_long2)
m3b<-lm(temp_c~TotalBA_m2*temptype+hour +impervious, data=asldat_long2)

m4<-lm(temp_c~Trees.*temptype+hour +surftype+sunshade, data=asldat_long2)
m4a<-lm(temp_c~TotalBA_m2*temptype+hour +surftype+sunshade, data=asldat_long2)
m4b<-lm(temp_c~TotalBA_m2*temptype+hour  +impervious+sunshade, data=asldat_long2)

m5<-lm(temp_c~Trees.+temptype+hour+surftype+sunshade+Trees.:temptype+Trees.:hour, data=asldat_long2)
m5a<-lm(temp_c~TotalBA_m2+temptype+hour+surftype+sunshade+TotalBA_m2:temptype+TotalBA_m2:hour, data=asldat_long2)
m5b<-lm(temp_c~TotalBA_m2+temptype+hour+impervious+sunshade+TotalBA_m2:temptype+TotalBA_m2:hour, data=asldat_long2)

m6<-lm(temp_c~Trees.+temptype+hour+surftype+sunshade+Trees.:temptype+Trees.:hour + Trees.:surftype, data=asldat_long2)
m6a<-lm(temp_c~TotalBA_m2+temptype+hour+surftype+sunshade+TotalBA_m2:temptype+TotalBA_m2:hour + TotalBA_m2:surftype, data=asldat_long2)
m6b<-lm(temp_c~TotalBA_m2+temptype+hour +impervious+sunshade+Trees.:temptype+TotalBA_m2:hour + TotalBA_m2:impervious, data=asldat_long2)


m7<-lm(temp_c~Trees.+temptype+hour+surftype+surftype:temptype+sunshade+Trees.:temptype+Trees.:hour + Trees.:surftype, data=asldat_long2)
m7a<-lm(temp_c~TotalBA_m2+temptype+hour+surftype+surftype:temptype+sunshade+TotalBA_m2:temptype+TotalBA_m2:hour + TotalBA_m2:surftype, data=asldat_long2)
m7b<-lm(temp_c~TotalBA_m2+temptype+hour +impervious +impervious:temptype+sunshade+TotalBA_m2:temptype+TotalBA_m2:hour + TotalBA_m2:impervious, data=asldat_long2)

m8b<-lm(temp_c~TotalBA_m2+temptype+hour +impervious +impervious:temptype+sunshade+TotalBA_m2:temptype+TotalBA_m2:hour + TotalBA_m2:impervious+impervious:hour, data=asldat_long2)
m9b<-lm(temp_c~TotalBA_m2+temptype+hour +impervious +impervious:temptype+sunshade+TotalBA_m2:temptype + TotalBA_m2:impervious+impervious:hour, data=asldat_long2)
m10b<-lm(temp_c~TotalBA_m2+temptype+hour +impervious +impervious:temptype+sunshade+TotalBA_m2:temptype + TotalBA_m2:impervious+impervious:hour+impervious:sunshade, data=asldat_long2)
m11b<-lm(temp_c~TotalBA_m2+temptype+hour +impervious +impervious:temptype+sunshade+TotalBA_m2:temptype + TotalBA_m2:impervious+impervious:hour+impervious:sunshade+impervious:sunshade:hour, data=asldat_long2)
m12b<-lm(temp_c~TotalBA_m2+temptype+hour +impervious +impervious:temptype+sunshade+TotalBA_m2:temptype + impervious:hour+impervious:sunshade+impervious:sunshade:hour, data=asldat_long2)
mm12b<-lmer(temp_c~TotalBA_m2+temptype+hour +impervious +impervious:temptype+sunshade+TotalBA_m2:temptype + impervious:hour+impervious:sunshade+impervious:sunshade:hour+(1|Hobo_SN), data=asldat_long2)

AIC(m1,m2,m3,m4,m5,m6,m1a,m2a,m3a,m3b, m4a,m4b, m5a,m5b,m6a,m6b, m7, m7a, m7b, m8b, m9b,m10b, m11b, m12b, mm12b)#m4a wins based on AIC
summary(mm12b)
cbind(coef(m12b), fixef(mm12b))
plot(mm12b)
summary(m12b)
tab_model(m12b, digits=3)
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

#Fit a model that predicts surface temperature from air temperature
#use the wide formatted data for this: 
asldat2<-left_join(asldat,sumba,copy=TRUE)

asldat2$hour<-as.integer(substr(asldat2$time,1,2))
asldat2$surftemp_c<-as.numeric(asldat2$surftemp_c)
asldat2$airtemp_c<-as.numeric(asldat2$airtemp_c)
asldat2$Trees.<-as.factor(asldat2$Trees.)
asldat2$TotalBA_cm2<-as.numeric(asldat2$TotalBA_cm2)
asldat2$TotalBA_m2<-asldat2$TotalBA_cm2/10000
asldat2$Hobo_SN <-as.factor(asldat2$Hobo_SN)
asldat2<-asldat2[-which(is.na(asldat2$TotalBA_cm2)),]

surfm1<-lm(surftemp_c~Trees.*airtemp_c, data=asldat2)
surfm1a<-lm(surftemp_c~TotalBA_m2*airtemp_c, data=asldat2)

surfm2<-lm(surftemp_c~Trees.*airtemp_c+hour, data=asldat2)
surfm2a<-lm(surftemp_c~TotalBA_m2*airtemp_c+hour, data=asldat2)

surfm3<-lm(surftemp_c~Trees.*airtemp_c+hour +surftype, data=asldat2)
surfm3a<-lm(surftemp_c~TotalBA_m2*airtemp_c+hour +surftype, data=asldat2)
surfm3b<-lm(surftemp_c~TotalBA_m2*airtemp_c+hour +impervious, data=asldat2)

surfm4<-lm(surftemp_c~Trees.*airtemp_c+hour +surftype+sunshade, data=asldat2)
surfm4a<-lm(surftemp_c~TotalBA_m2*airtemp_c+hour +surftype+sunshade, data=asldat2)
surfm4b<-lm(surftemp_c~TotalBA_m2*airtemp_c+hour  +impervious+sunshade, data=asldat2)

surfm5<-lm(surftemp_c~Trees.+airtemp_c+hour+surftype+sunshade+Trees.:airtemp_c+Trees.:hour, data=asldat2)
surfm5a<-lm(surftemp_c~TotalBA_m2+airtemp_c+hour+surftype+sunshade+TotalBA_m2:airtemp_c+TotalBA_m2:hour, data=asldat2)
surfm5b<-lm(surftemp_c~TotalBA_m2+airtemp_c+hour+impervious+sunshade+TotalBA_m2:airtemp_c+TotalBA_m2:hour, data=asldat2)

surfm6<-lm(surftemp_c~Trees.+airtemp_c+hour+surftype+sunshade+Trees.:airtemp_c+Trees.:hour + Trees.:surftype, data=asldat2)
surfm6a<-lm(surftemp_c~TotalBA_m2+airtemp_c+hour+surftype+sunshade+TotalBA_m2:airtemp_c+TotalBA_m2:hour + TotalBA_m2:surftype, data=asldat2)
surfm6b<-lm(surftemp_c~TotalBA_m2+airtemp_c+hour +impervious+sunshade+Trees.:airtemp_c+TotalBA_m2:hour + TotalBA_m2:impervious, data=asldat2)


surfm7<-lm(surftemp_c~Trees.+airtemp_c+hour+surftype+surftype:airtemp_c+sunshade+Trees.:airtemp_c+Trees.:hour + Trees.:surftype, data=asldat2)
surfm7a<-lm(surftemp_c~TotalBA_m2+airtemp_c+hour+surftype+surftype:airtemp_c+sunshade+TotalBA_m2:airtemp_c+TotalBA_m2:hour + TotalBA_m2:surftype, data=asldat2)
surfm7b<-lm(surftemp_c~TotalBA_m2+airtemp_c+hour +impervious +impervious:airtemp_c+sunshade+TotalBA_m2:airtemp_c+TotalBA_m2:hour + TotalBA_m2:impervious, data=asldat2)

surfm8b<-lm(surftemp_c~TotalBA_m2+airtemp_c+hour +impervious +impervious:airtemp_c+sunshade+TotalBA_m2:airtemp_c+TotalBA_m2:hour + TotalBA_m2:impervious+impervious:hour, data=asldat2)
surfm9b<-lm(surftemp_c~TotalBA_m2+airtemp_c+hour +impervious +impervious:airtemp_c+sunshade+TotalBA_m2:airtemp_c + TotalBA_m2:impervious+impervious:hour, data=asldat2)
surfm10b<-lm(surftemp_c~TotalBA_m2+airtemp_c+hour +impervious +impervious:airtemp_c+sunshade+TotalBA_m2:airtemp_c + TotalBA_m2:impervious+impervious:hour+impervious:sunshade, data=asldat2)
surfm11b<-lm(surftemp_c~TotalBA_m2+airtemp_c+hour +impervious +impervious:airtemp_c+sunshade+TotalBA_m2:airtemp_c + TotalBA_m2:impervious+impervious:hour+impervious:sunshade+impervious:sunshade:hour, data=asldat2)

surfm12b<-lm(surftemp_c~airtemp_c+TotalBA_m2+hour +impervious +impervious:airtemp_c+sunshade+TotalBA_m2:airtemp_c + impervious:hour+impervious:sunshade+impervious:sunshade:hour, data=asldat2)
surfmm12b<-lmer(surftemp_c~airtemp_c+TotalBA_m2+hour +impervious +impervious:airtemp_c+sunshade+TotalBA_m2:airtemp_c + impervious:hour+impervious:sunshade+impervious:sunshade:hour+(1|Hobo_SN), data=asldat2)

surfm15<-lm(surftemp_c~airtemp_c+TotalBA_m2+hour+impervious +sunshade+TotalBA_m2:airtemp_c +TotalBA_m2:hour + impervious:hour+impervious:sunshade+impervious:sunshade:hour, data=asldat2)
AIC(surfm1,surfm2,surfm3,surfm4,surfm5,surfm6,surfm1a,surfm2a,surfm3a,surfm3b, surfm4a,surfm4b, surfm5a,surfm5b,surfm6a,surfm6b, surfm7, surfm7a, surfm7b, surfm8b, surfm9b,surfm10b, surfm11b, surfm12b, surfmm12b, surfm15)#m4a wins based on AIC

summary(surfm12b)

