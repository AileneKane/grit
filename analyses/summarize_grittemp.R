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

tminm<-lmer(tmin~Trees.+(1|loggersn), data=logdat.feb)

tmaxm<-lm(tmax~Trees., data=logdat.feb)
summary(tmaxm)




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
  labs(title = "Tmin", y= "Minimum Temoerature ( ºC )", x = "Date")

tmax<- qplot(Date,tmax, data = logdat, colour = Trees.) +
  geom_smooth(method="gam")+
  geom_point(aes(color=Trees.)) +
  labs(title = "Tmax", y= "Maximum Temoerature ( ºC )", x = "Date")

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


