#################################################
### Script to summarize GRIT temperature data ###
########## Started by Ailene Ettinger ###########
############### April 29, 2022 ##################
############ ailene.ettinger@tnc.org ############
#################################################


# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries
library(lubridate)
library(ggplot2)
library(gridExtra)
library(scales)
library(ggthemes)
library(brms)
library(rstan)
library(dplyr)
library(lme4)
# set working directory
setwd("~/GitHub/grit/analyses")

tempdatdir<-"../data/temp_data/"

#Read in data on hobo locatoins
locs<-read.csv("../data/HoboLocations_PoleLocations.csv", header=TRUE)
locs<-locs[,1:16]

#Get Daily min and max temperatures for each logger
tempdatfolders<-list.files(tempdatdir)
loggerdat<-NULL
for(i in tempdatfolders){
  tempfol<-paste(tempdatdir,i, sep="")
  tempfiles<-list.files(tempfol)[grep(".csv",list.files(tempfol))]
  for(j in tempfiles){
    dat<-read.csv(paste(tempfol,"/",j,sep=""), skip=1,header=TRUE)
    colnames(dat)[2:3]<-c("date.time","temp_c")
    dat$date<-substr(dat$date.time,1,8)
    tmins<-aggregate(dat$temp_c,by=list(dat$date),min,na.rm=TRUE)
    colnames(tmins)<-c("date","tmin")
    tmaxs<-aggregate(dat$temp_c,by=list(dat$date),max,na.rm=TRUE)
    colnames(tmaxs)<-c("date","tmax")
    tmntmx<-cbind(tmins,tmaxs$tmax)
    tmntmx$loggersn<-as.factor(substr(j,1,8))
    loggerdat<-rbind(loggerdat,as.data.frame(tmntmx))
  }
}

loggerdat$mon<-as.character(substr(loggerdat$date,1,2))
loggerdat$yr<-as.character(paste("20",substr(loggerdat$date,7,8),sep=""))
loggerdat$day<-as.character(substr(loggerdat$date,4,5),sep="")
loggerdat$Date<-as.Date(paste(loggerdat$yr,loggerdat$mon,loggerdat$day,sep="-"))
colnames(loggerdat)[3]<-"tmax"

#Now add in BT logger data
btdat<-read.csv("../data/BT_temp_data/GRIT_loggers_AllBTdat_06_30_07_26_18_UTC_1.csv")
btdat<-btdat[,1:18]
colnames(btdat)[3:18]<-substr(colnames(btdat)[3:18],50,nchar(colnames(btdat)[3:18]))
btdat$date<-as.character(btdat$Date)
btdat<-btdat %>% separate(date, c("mon", "day","yr"),"/")
btdat<-btdat %>% separate(yr, c("yr", "time")," ")
btdat$Date2<-as.factor(as.Date(paste(btdat$yr,btdat$mon,btdat$day,sep="-")))


btmins<-aggregate(cbind("21302946","21302947","21302950","21302953","21302957",
                        "21302967" ,"21302974","21302976","21302977","21302980", "21302959","21302964")~Date2,data=btdat,FUN=min)
colnames(dat)[2:3]<-c("date.time","temp_c")
dat$date<-substr(dat$date.time,1,8)
tmins<-aggregate(dat$temp_c,by=list(dat$date),min,na.rm=TRUE)
colnames(tmins)<-c("date","tmin")
tmaxs<-aggregate(dat$temp_c,by=list(dat$date),max,na.rm=TRUE)
colnames(tmaxs)<-c("date","tmax")
tmntmx<-cbind(tmins,tmaxs$tmax)

#get a sense of how much data for each logger
#table(loggerdat$mon,loggerdat$loggersn)
#remove the loggers without radiation shields for now
pilotlogs<-which(loggerdat$loggersn=="9784509."|loggerdat$loggersn=="9784558."|loggerdat$loggersn=="9768700.")
logdatnp<-loggerdat[-pilotlogs,]
#table(logdatnp$mon,logdatnp$loggersn)
colnames(locs)[2]<-"loggersn"
logdat<-left_join(logdatnp,locs, by="loggersn", copy=TRUE)

logdat.feb<-logdat[logdat$mon=="02",]
unique(logdat.feb$loggersn[which(is.na(logdat.feb$Trees.))])

#for now remove sites without trees
logdat.feb<-logdat.feb[-which(is.na(logdat.feb$Trees.)),]
logdat.feb$loggersn<-as.factor(logdat.feb$loggersn)
logdat.feb$Trees.<-as.factor(logdat.feb$Trees.)
boxplot(logdat.feb$tmin~logdat.feb$Trees.)
logdat$date<-as.factor(logdat$date)

tminm<-lmer(tmin~Trees.+(1|date)+(1|loggersn), data=logdat)
tmaxm<-lmer(tmax~Trees.+(1|date)+(1|loggersn), data=logdat)
logdat$trange<-logdat$tmax-logdat$tmin
trange<-lmer(trange~Trees.+(1|date)+(1|loggersn), data=logdat)
summary(trange)
summary(tmaxm)
summary(tminm)
logdat$doy<-yday(logdat$Date)
logdat$trees<-0
logdat$trees[which(logdat$Trees.=="Y")]<-1
logdat2022<-subset(logdat,select=c(loggersn,doy,yr,tmin,tmax,trange,trees),yr==2022)

tminmod <- brm(tmin ~ trees +s(doy) + (1|loggersn),
         data=logdat2022, cores = 2,
         iter = 4000, warmup = 1000, thin = 10)

save(tminmod, file="analyses/output/k.brms.Rda")

# Time series of average daily temperature, with smoother curve
logdat<-logdat[-which(is.na(logdat$Trees.)),]

ggplot(logdat,aes(x = date,y = tmin)) +
  geom_point(colour = "blue") +
  geom_smooth(colour = "red",size = 5) +
  scale_y_continuous(limits = range(logdat$tmin), breaks = seq(5,30,5)) +
  ggtitle ("Daily minimum temperature") +
  xlab("Date") +  ylab ("Minimum Temperature ( ºC )")

tmin<- qplot(Date,tmin, data = logdat, colour = Trees.) +
  geom_smooth(method="gam")+
  geom_point(aes(color=Trees.)) +
  labs(title = "Tmin", y= "Minimum Temperature ( ºC )", x = "Date")

tmax<- qplot(Date,tmax, data = logdat, colour = Trees.) +
  geom_smooth(method="gam")+
  geom_point(aes(color=Trees.)) +
  labs(title = "Tmax", y= "Maximum Temperature ( ºC )", x = "Date")

png(file="figs/tmin.png",width = 1000, height =800)
#pdf(file="figs/tmin.pdf",width = 10, height =8)
tmin+ scale_color_manual(labels = c("No trees","Trees"),
                       values=c("gray","darkgreen"))+ theme_bw()+
                        scale_x_date(labels=date_format("%b %y"))+
 theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))
dev.off()

png(file="figs/tmax.png",width = 1000, height =800)
#pdf(file="figs/tmax.pdf",width = 10, height =8)

tmax+ scale_color_manual(labels = c("No trees","Trees"),
                         values=c("gray","darkgreen"))+ theme_bw()+
  scale_x_date(labels=date_format("%b %y"))+
  
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))
dev.off()

logdat[which(logdat$tmax==(max(logdat$tmax))),]

table(logdat$Trees.,logdat$mon)
