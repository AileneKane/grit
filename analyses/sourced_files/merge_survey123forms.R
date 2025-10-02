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

survey123_2024<-read.csv("data/PurpleAir/purpleair2024locs.csv", header=TRUE) 
survey123_2025<-read.csv("data/PurpleAir/purpleair2025locs.csv", header=TRUE) 

latlongs_2024 <- survey123_2024 %>% select(Purple.Air.Name, x, y,Number.of.Trees)
latlongs_2025 <- survey123_2025 %>% select(PurpleAir.Name, x, y,Number.of.Trees)

colnames(latlongs_2024)<-c("Purple.Air.Name", "Long", "Lat","Number.of.Trees")
colnames(latlongs_2025)<-c("Purple.Air.Name", "Long", "Lat","Number.of.Trees")

survey123_merged<-rbind(latlongs_2024,latlongs_2025)
testrows<-which(survey123_merged$Purple.Air.Name)
survey123_merged<- 