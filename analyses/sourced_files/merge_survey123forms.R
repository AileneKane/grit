########################################################
### Script to put together GRIT Purple Air Locations ###
########## from 2024 and 2025 Survey 123 forms #########
########## Started by Ailene, August 27, 2025 ##########
########################################################

#set wd for ailene's computer:
setwd("~/GitHub/grit")
options("digits" = 15)

#load libraries
library(dplyr)

survey123_2024<-read.csv("data/PurpleAir/GRITPurpleAir_Survey123_2024_2025Nov11.csv", header=TRUE) 
survey123_2025<-read.csv("data/PurpleAir/GRITPurpleAir_Survey123_2025_2025Nov11.csv", header=TRUE) 
purpleairid<-read.csv("data/PurpleAir/PurpleAirAPIInfo.csv", header=TRUE)
#remove duplicate GRIT03 and clean before merging
survey123_2024<-survey123_2024[-which(survey123_2024$ObjectID=="5"),]
survey123_2024<-survey123_2024[-which(survey123_2024$Purple.Air.Name==""),]
survey123_2024$Address[survey123_2024$Purple.Air.Name=="GRIT03"]<-"9211 Fawcett Ave, Tacoma, WA, 98444"
survey123_2024$Purple.Air.Name[survey123_2024$Purple.Air.Name=="GRIt09"]<-"GRIT09"
survey123_2024$Purple.Air.Name[survey123_2024$Purple.Air.Name=="GRIt 31"]<-"GRIT31"
#GRIT07 has the wrong lat long. Should be 47.1980794,-122.4021333,452
survey123_2024$x[survey123_2024$Purple.Air.Name=="GRIT07"]<- -122.4021333
survey123_2024$y[survey123_2024$Purple.Air.Name=="GRIT07"]<- 47.1980794

#GRIT09 in the 2024 survey123 form is actually GRIT35 on the 2025 form so need to merge these two
#survey123_2025[survey123_2025$PurpleAir.Name=="GRIT35",]
#survey123_2024[survey123_2024$Purple.Air.Name=="GRIT09",]
survey123_2025$Notes[survey123_2025$PurpleAir.Name=="GRIT35"]<-survey123_2024$Notes[survey123_2024$Purple.Air.Name=="GRIT09"]
survey123_2025$PurpleAir.Steward.Phone.Number[survey123_2025$PurpleAir.Name=="GRIT35"]<-survey123_2024$Steward.Phone.Number[survey123_2024$Purple.Air.Name=="GRIT09"]
survey123_2025$x[survey123_2025$PurpleAir.Name=="GRIT35"]<-survey123_2024$x[survey123_2024$Purple.Air.Name=="GRIT09"]
survey123_2025$y[survey123_2025$PurpleAir.Name=="GRIT35"]<-survey123_2024$y[survey123_2024$Purple.Air.Name=="GRIT09"]
survey123_2025$Address.of.PurpleAir.Location[survey123_2025$PurpleAir.Name=="GRIT35"]<-survey123_2024$Address[survey123_2024$Purple.Air.Name=="GRIT09"]
survey123_2024<-survey123_2024[-which(survey123_2024$Purple.Air.Name=="GRIT09"),]

latlongs_2024 <- survey123_2024 %>% select(Purple.Air.Name,Address, x, y,Number.of.Trees)
latlongs_2025 <- survey123_2025 %>% select(PurpleAir.Name,Address.of.PurpleAir.Location, x, y,Number.of.Trees)



colnames(latlongs_2024)<-c("Purple.Air.Name", "Address","Long", "Lat","Number.of.Trees")
colnames(latlongs_2025)<-c("Purple.Air.Name", "Address","Long", "Lat","Number.of.Trees")


survey123_merged<-rbind(latlongs_2024,latlongs_2025)

#remove the test rows
testrows<-grep("est",survey123_merged$Purple.Air.Name)
survey123_merged<-survey123_merged[-testrows,]
survey123_merged<-survey123_merged[order(survey123_merged$Purple.Air.Name),]

#now merge in purpleair id info
colnames(purpleairid)[1]<-"Purple.Air.Name"
colnames(purpleairid)[5]<-"Notes"
purpleairid<-purpleairid[,-6]

survey123_merged2<-left_join(purpleairid,survey123_merged)
survey123_merged2<-survey123_merged2[!survey123_merged2$DeviceID=="",]
write.csv(survey123_merged2,"analyses/output/purpleair_locs_20242025.csv", row.names = FALSE)
