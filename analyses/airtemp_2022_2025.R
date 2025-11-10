#################################################
### Script to look at air temperature pre- & ####
#### post- tree planting in madison district ####
################ July 11, 2024 ##################
############ ailene.ettinger@tnc.org ############
#################################################

# temperature data collection began in 2022 
# trees were planted in January-May 2024
# thus: 
# pre-tree planting summers: 2022, 2023
# post- tree planting summers: 2024, 2025

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries
library(dplyr)
library(lme4)
# set working directory
setwd("~/GitHub/grit/analyses")

#Read in data on all temp logger locations
locs<-read.csv("../data/HoboLocations_PoleLocations.csv", header=TRUE)
source("sourced_files/clean_locs.R")

#Read in tree data 
treedat<-read.csv("../data/HoboLocations_TreeData.csv", header=TRUE)

source("sourced_files/summarize_grittreedat.R")

#Read in data on temp loggers locations
logs<-read.csv("../data/HoboLocations_TempLoggerInfo2024.csv", header=TRUE)
source("sourced_files/clean_temploggers.R")

#Read in remote-sensed land cover estimates, derived from stormwater heat map lc data
lc<-read.csv("output/grit_logger_lc.csv", header=TRUE)

#add lc to tree dat
source("sourced_files/combine_lc_grittreedat.R")

#head(sumba)

#Read in temperature data from Visual crossings for Tacoma
vc<-read.csv("../data/TacomaWA2022-01-01to2023-01-01.csv", header=TRUE)
#load helper functions
source("sourced_files/helper_funcs.R")

#Select out the temp loggers that are on S Tacoma way (with and without trees) and in the parks, for comparison:

focallocs<-locs3$Location[locs3$Location== "S. Tacoma Way between 48th and 49th"|
                           locs3$Location=="S. Tacoma Way between 38th and 42th"|
                           locs3$Location=="Park at warner"|
                           locs3$Location=="S. 50th at Asia Pacific Center" |
                           locs3$Location=="S. Tacoma Way at 38th"|
                           locs3$Location=="Pine between 45th and 47th"|
                           locs3$Location=="Pine between 45th and 43rd"]
#now pull the temp data from Jul at those locations
tempdatdir<-"../data/BT_temp_data/2024_07_11"
focalsnfiles<-list.files(tempdatdir)

loggerdat<-NULL

for(i in focalsnfiles){
    dat<-read.csv(paste(tempdatdir,"/",i,sep=""),header=TRUE)
    colnames(dat)[2:3]<-c("date.time","temp_c")
    dat$date<-substr(dat$date.time,1,10)
    tmins<-aggregate(dat$temp_c,by=list(dat$date),min,na.rm=TRUE)
    colnames(tmins)<-c("date","tmin")
    tmaxs<-aggregate(dat$temp_c,by=list(dat$date),max,na.rm=TRUE)
    colnames(tmaxs)<-c("date","tmax")
    tmntmx<-cbind(tmins,tmaxs$tmax)
    tmntmx$loggersn<-as.factor(substr(i,1,8))
    loggerdat<-rbind(loggerdat,as.data.frame(tmntmx))
  }

#now merge the logger info (i.e. trees or not, locations), with the logger data:
colnames(locs3)[16]<-"loggersn"
colnames(loggerdat)[3]<-"tmax"
focallocs3<-locs3[locs3$Location== "S. Tacoma Way between 48th and 49th"|
                                        locs3$Location=="S. Tacoma Way between 38th and 42th"|
                                        locs3$Location=="Park at warner"|
                                        locs3$Location=="S. 50th at Asia Pacific Center" |
                                        locs3$Location=="S. Tacoma Way at 38th"|
                                        locs3$Location=="Pine between 45th and 47th"|
                                        locs3$Location=="Pine between 45th and 43rd",]
#Correcting the logger sns by hand, as this file is not up to date
focallocs3$loggersn[focallocs3$Location== "S. Tacoma Way between 48th and 49th"]<-"21616945"
focallocs3$loggersn[focallocs3$Location== "Park at warner"]<-"21616938"
focallocs3$loggersn[focallocs3$Location== "S. 50th at Asia Pacific Center"]<-"21616959"
focallocs3$loggersn[focallocs3$Location== "S. Tacoma Way between 48th and 49th"]<-"21616945"
focallocs3$loggersn[focallocs3$Location== "S. Tacoma Way between 38th and 42th" & focallocs3$Pole_No=="TP13987/A1378019"]<-"21616931"
focallocs3$loggersn[focallocs3$Location== "S. Tacoma Way between 38th and 42th" & focallocs3$Pole_No=="TP13988/A1378018"]<-"21302981"
focallocs3$loggersn[focallocs3$Location== "S. Tacoma Way at 38th"]<-"21302946"

logdatlocs<-left_join(loggerdat,focallocs3)
#restrict to 
logdatlocs<-logdatlocs[logdat$date=="07/09/2024",]
logdatlocs2<-logdatlocs[-which(is.na(logdatlocs$Trees.)),]


dim(logdatlocs2)
#colnames(logdatlocs)[3]<-"tmax"

#restrict logger dat to  July 9:

