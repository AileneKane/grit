#######################################################
### Script to look at GRIT purpleair air quality data ###
################# Started June 20, 2024 #####################
################ ailene.ettinger@tnc.org ##############
#######################################################

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)

# set working directory
setwd("documents/GitHub/grit/analyses") #for apple computers add documents

#Read in data file with PurpleAir logger locations
#(notyet created)
#Read in air quality data from 2 sites
GRIT01<-read.csv("../data/PurpleAir/GRIT01-2024-07-03 2024-07-05.csv", header=TRUE) 
GRIT02<-read.csv("../data/PurpleAir/GRIT02 2024-07-03 2024-07-05.csv", header=TRUE) 
GRIT03<-read.csv("../data/PurpleAir/GRIT03 2024-07-03 2024-07-05.csv", header=TRUE) 
GRIT04<-read.csv("../data/PurpleAir/GRIT04 2024-07-03 2024-07-05.csv", header=TRUE) 

#look at the data
head(GRIT01)
summary(GRIT02)
dim(GRIT02)
#understand the data a bit more- what are these 2 columns- max and min?
range(GRIT01$pm2.5_atm_a, na.rm=TRUE) #_atm vs _cf_1, atm is for outdoor sensors, cf_1 is for indoor
range(GRIT01$pm2.5_atm_b, na.rm=TRUE)
mean(c(GRIT01$pm2.5_atm_b,GRIT01$pm2.5_atm_b, na.rm=TRUE),na.rm=TRUE)
mean(c(aqd2$X350Tacoma01.A,aqd2$X350Tacoma01.B,aqd2$Whitman.Elementary.School.A, aqd2$Whitman.Elementary.School.B),na.rm=TRUE)

#separating sensors by date

GRIT01_July03 <- GRIT01 %>% slice(1:41)
time <- c(
    "03:30", "04:00", "04:30", "05:00", "05:30", 
            "06:00", "06:30", "07:00", "07:30", "08:00", "08:30", 
            "09:00", "09:30", "10:00", "10:30", "11:00", "11:30", 
            "12:00", "12:30", "13:00", "13:30", "14:00", "14:30", 
            "15:00", "15:30", "16:00", "16:30", "17:00", "17:30", 
            "18:00", "18:30", "19:00", "19:30", "20:00", "20:30", 
            "21:00", "21:30", "22:00", "22:30", "23:00", "23:30") 
GRIT01_July03$time_stamp <-time
view(GRIT01_July03) # change time_stamp values so it's easier to read 

GRIT01_July04 <- GRIT01 %>% slice (42:89)
time2 <- c(
  "00:00", "00:30", "01:00", "01:30", "02:00", "02:30", 
  "03:00", "03:30", "04:00", "04:30", "05:00", "05:30", 
  "06:00", "06:30", "07:00", "07:30", "08:00", "08:30", 
  "09:00", "09:30", "10:00", "10:30", "11:00", "11:30", 
  "12:00", "12:30", "13:00", "13:30", "14:00", "14:30", 
  "15:00", "15:30", "16:00", "16:30", "17:00", "17:30", 
  "18:00", "18:30", "19:00", "19:30", "20:00", "20:30", 
  "21:00", "21:30", "22:00", "22:30", "23:00", "23:30"
)
GRIT01_July04$time_stamp <-time2
view(GRIT01_July04)
   
#plotting GRIT01 pm2.5_atm on July 3rd 
GRIT01_July03_Plot <- 
  ggplot( data = GRIT01_July03,
          mapping = aes(x = time_stamp, y = pm2.5_atm)) +
          geom_point() +
          labs(title = "GRIT01 Particulate Matter 2.5 Concentrations on July 3rd, 2024",
           x = "Time", 
           y = "PM 2.5 Concentrations (µg/m3)" ) + 
          theme(axis.text.x = element_text (angle = 90, vjust = -0.01))

#plotting GRIT01 pm2.5_atm on July 4th

GRIT01_July04_Plot <- 
  ggplot( data = GRIT01_July04,
          mapping = aes(x = time_stamp, y = pm2.5_atm)) +
  geom_point() +
  labs(title = "GRIT01 Particulate Matter 2.5 Concentrations on July 4th, 2024",
       x = "Time", 
       y = "PM 2.5 Concentrations (µg/m3)" ) +
      theme(axis.text.x = element_text (angle = 90, vjust = -0.01))


plot(GRIT01$pm2.5_atm_a, type="l",
     ylab="Air quality index",
     xlab="Collection #")
lines(aqd2$Whitman.Elementary.School.A, col="green")
