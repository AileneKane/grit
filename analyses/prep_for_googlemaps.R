#################################################
### Script to get pole location lat-longs in  ###
######### right format for google maps ##########
############### August 8, 2022 ##################
############ ailene.ettinger@tnc.org ############
#################################################


# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries

# set working directory
setwd("~/GitHub/grit/analyses")

#Read in data on hobo locatoins
locs<-read.csv("../data/HoboLocations_PoleLocations.csv", header=TRUE)
locs<-locs[,1:13]

locs$gmapslocs<-paste(locs$Latitude,locs$Longitude,sep=",")
gmaps<-subset(locs, select=c("WptNo", "Pole_No","gmapslocs"))
gmaps<-gmaps[-which(gmaps$gmapslocs=="NA,NA"),]      
gmaps<-gmaps[-which(gmaps$Pole_No=="WH1"),]      
gmaps<-gmaps[-which(gmaps$Pole_No=="WH2"),]      
gmaps<-gmaps[-which(gmaps$Pole_No=="STW1"),]      

gmaps<-gmaps[-which(gmaps$Pole_No=="STW2"),]      

write.csv(gmaps,"output/gmapslocs.csv", row.names=FALSE)
