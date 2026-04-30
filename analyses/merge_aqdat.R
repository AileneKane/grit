#################################################
######## Script to merge purpleair data plot  PurpleAir data #########
######### and fit models to data in #############
############### purpleair_all.csv ###############
#################################################
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries
library(tidyverse)
library(dplyr)

setwd("~/Documents/GitHub/grit/analyses") 
# Setting working directory. Add in ailene's path in an if statement so that it works for her too
if(length(grep("ailene", getwd()))>0) {
  setwd("C:/Users/ailene.ettinger/Documents/GitHub/grit/analyses")
}

###read in air quality data###
pa_jul_oct_2025<- read.csv("output/purpleair_July2025_to_Oct2025.csv")
pa_nov2025_feb2026<- read.csv("output/purpleair_Nov2025_to_Feb2026.csv")
pa_tpch<- read.csv("output/purpleair_TPCH_Aug2025_to_Feb2026.csv")
pa_sd<- read.csv("output/purpleair_SDCARDS_Aug2025_to_Feb2026.csv")

### read in sensor data from GRIT and TPCH
gritsensors <- read.csv("../data/PurpleAir/PurpleAirAPIInfo.csv")
tpchsensors <- read.csv("../data/PurpleAir/TPCH_PurpleAirMonitors.csv")

gritsensors <- gritsensors %>% drop_na(SensorIndex) %>% rename(sensor_index = SensorIndex)

gritsensors_clean <- gritsensors %>%
  distinct(sensor_index, .keep_all = TRUE)
gritsensors_clean<-subset(gritsensors_clean, select=c(Name,sensor_index))

#merge tpchsensors with grit sensors
tpchsensors_to_use<-tpchsensors[tpchsensors$Data.downloaded.=="yes",]
tpchsensors_to_use<- tpchsensors_to_use %>% 
    rename(sensor_index = SensorIndex, Name = Place.Name)
tpchsensors_to_use<-subset(tpchsensors_to_use, select=c(Name,sensor_index))

sensornames_clean<-rbind(gritsensors_clean,tpchsensors_to_use)

pa<-rbind(pa_jul_oct_2025,pa_nov2025_feb2026,pa_tpch,pa_sd)

pa$datetime <- make_datetime(
  year = pa$year,
  month = pa$month,
  day   = pa$day,
  hour  = pa$hour,
  tz = "America/Los_Angeles")
pa$date <- as_date(pa$datetime)

#check din before joining
#dim(pa)#181690     11
#join purple air data with sensor data
pa <- pa %>%
  left_join(sensornames_clean, by = "sensor_index") 
#pa <- pa[, -c(12, 13)] ## included GRIT sensor names 
# dim(pa)#181690     12
#check that all names made it
#unique(pa$Name)
#head(pa[pa$Name=="N Tacoma N 9th and Stevens",])
#check for duplicates
#dim(pa[which(duplicated(pa[c("year", "month","day","hour","sensor_index","avg_pm")])),])#287
#remove these!
pa<- pa[-which(duplicated(pa[c("year", "month","day","hour","sensor_index","avg_pm")])),]

#for now, also remove duplicates in sensor index, year, month, day and hour- i should take care of this in a better way
pa<- pa[-which(duplicated(pa[c("year", "month","day","hour","sensor_index")])),]#16724

dim(pa)
write.csv(pa,"output/purpleair_all.csv")

#check for duplicates in sensor index, year, month, day and hour
dim(pa[which(duplicated(pa[c("year", "month","day","hour","sensor_index")])),])#16724

unique(pa$Name[which(duplicated(pa[c("year", "month","day","hour","sensor_index")]))])
table(pa$Name[which(duplicated(pa[c("year", "month","day","hour","sensor_index")]))])

#none!