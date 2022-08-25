#################################################
### Script to look at heat wave July 2022 data###
############### August 22, 2022 ##################
############ ailene.ettinger@tnc.org ############
#################################################


# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries
library(dplyr)
library(lme4)
# set working directory
setwd("~/GitHub/grit/analyses")

#Read in data on hobo locatoins
locs<-read.csv("../data/HoboLocations_PoleLocations.csv", header=TRUE)
locs<-locs[,1:18]

#Select out the temp loggers that are on S Tacoma way (with and wthout trees) and in the parks, for comparison:

focalsns<-locs$Hobo_SN[locs$Location=="S. Tacoma Way between 38th and 42th"|locs$Location=="Park at warner"]
#now pull the temp data from Jul at those locations
tempdatdir<-"../data/temp_data/2022_08_10"
focalsnfiles<-paste(focalsns,".csv", sep="")
#for now, skip the blue tooth loggers
focalsnfiles<-focalsnfiles[-which(substr(focalsnfiles,1,2)=="BT")]

loggerdat<-NULL

for(i in focalsnfiles){
    dat<-read.csv(paste(tempdatdir,"/",i,sep=""), skip=1,header=TRUE)
    colnames(dat)[2:3]<-c("date.time","temp_c")
    dat$date<-substr(dat$date.time,1,8)
    tmins<-aggregate(dat$temp_c,by=list(dat$date),min,na.rm=TRUE)
    colnames(tmins)<-c("date","tmin")
    tmaxs<-aggregate(dat$temp_c,by=list(dat$date),max,na.rm=TRUE)
    colnames(tmaxs)<-c("date","tmax")
    tmntmx<-cbind(tmins,tmaxs$tmax)
    tmntmx$loggersn<-as.factor(substr(i,1,8))
    loggerdat<-rbind(loggerdat,as.data.frame(tmntmx))
  }

#now merge the logger info (i.e. trees or not, locations), with the logger data:
colnames(locs)[2]<-"loggersn"
colnames(loggerdat)[3]<-"tmax"

logdatlocs<-left_join(loggerdat,locs)
#colnames(logdatlocs)[3]<-"tmax"

logdatlocs$type<-"cpark"
logdatlocs$type[logdatlocs$loggersn=="21223131"]<-"ano trees"
logdatlocs$type[logdatlocs$loggersn=="21223102"]<-"btrees"

#get max temp for each logger

maxtemps<-aggregate(logdatlocs$tmax, by=list(logdatlocs$loggersn,logdatlocs$Location,logdatlocs$Trees.), max)
colnames(maxtemps)<-c("SN","Location","Trees","Temp_C")
maxtemps$order<-c(3,1,2)
maxtemps<-maxtemps[order(maxtemps$order),]# all on on July 31

#on the hottest measurements:
png("figs/STacomamaxtemps.png", width=4, height=6, units="in", res=220)

barplot(maxtemps$Temp_C, ylim=c(30,38),col=c("gray","seagreen3","darkgreen"),
        names.arg=c("No trees","Trees","Forested Park"), ylab="Temperature (C)", xlab="Location",
        main="Maximum temperature, July 2022", xpd=FALSE)
abline(h=30)
mtext ("(Occurred on July 31)", side=3, line=0)
dev.off()
maxtemps$Temp_C
maxtemps$Temp_C[1]-maxtemps$Temp_C[3]#difference between no trees and park# 0.855
maxtemps$Temp_C[1]-maxtemps$Temp_C[2]#difference between no trees and and trees

boxplot(logdatlocs$tmax~logdatlocs$type)
boxplot(logdatlocs$tmin~logdatlocs$type)
summary(lm(logdatlocs$tmin~logdatlocs$type))
summary(lm(logdatlocs$tmax~logdatlocs$type))
logdatlocs$month<-substr(logdatlocs$date,1,2)
augdat<-logdatlocs[logdatlocs$month=="07",]
maxmod<-lmer(tmax~-1+type+ (1|as.factor(augdat$date)), dat=augdat)
minmod<-lmer(tmin~-1+type+ (1|as.factor(augdat$date)), dat=augdat)
png("figs/STacomaaugmaxtemps.png", width=4, height=6, units="in", res=220)

maxplot<-barplot(fixef(maxmod), ylim=c(20,28),col=c("gray","seagreen3","darkgreen"),
        names.arg=c("No trees","Trees","Forested Park"), ylab="Temperature (C)",xlab="Location",
        main="Daily maximum temperatures", xpd=FALSE)
abline(h=20)
for(i in 1:length(fixef(maxmod))){
arrows(maxplot[i],fixef(maxmod)[i]-summary(maxmod)$coef[i,2],maxplot[i],fixef(maxmod)[i]+summary(maxmod)$coef[i,2], length=.1, angle=90, code=3)
}
mtext ("July 2022", side=3, line=0)
dev.off()
fixef(maxmod)[1]-fixef(maxmod)[3]#difference between no trees and park
fixef(maxmod)[1]-fixef(maxmod)[2]#difference between no trees and and trees
