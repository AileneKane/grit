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
pa_tpch_nov2025_feb2026<- read.csv("output/purpleair_Nov2025_to_Feb2026.csv")

### read in sensor data
sensornames <- read.csv("../data/PurpleAir/PurpleAirAPIInfo.csv")
sensornames <- sensornames %>% drop_na(SensorIndex) %>% rename(sensor_index = SensorIndex)
sensornames_clean <- sensornames %>%
  distinct(sensor_index, .keep_all = TRUE)

#add year to air quality data
pa_jul_oct_2025$year <- 2025 
pa_nov2025_feb2026$year<-2025
pa_nov2025_feb2026$year[pa_nov2025_feb2026$month==1]<-2026
pa_nov2025_feb2026$year[pa_nov2025_feb2026$month==2]<-2026

pa<-rbind(pa_jul_oct_2025,pa_nov2025_feb2026)

pa$datetime <- make_datetime(
  year = pa$year,
  month = pa$month,
  day   = pa$day,
  hour  = pa$hour)
pa$date <- as_date(pa$datetime)


#join purple air data with sensor data
pa <- pa %>%
  left_join(sensornames_clean, by = "sensor_index") 
pa <- pa[, -c(14, 15)] ## included GRIT sensor names 

head(pa)
write.csv(pa,"output/purpleair_all.csv")
