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

# set working directory
setwd("~/GitHub/grit/analyses")

#Read in data file with PurpleAir logger locations
#(notyet created)
#Read in air quality data from 2 sites
aqd2<-read.csv("../data/PurpleAir/us-epa-pm25-aqi-test2.csv", header=TRUE)

#look at the data
head(aqd2)
summary(aqd2)
dim(aqd2)
#understand the data a bit more- what are these 2 columns- max and min?
range(aqd2$X350Tacoma01.A, na.rm=TRUE)
range(aqd2$X350Tacoma01.B, na.rm=TRUE)
#the two channels at 350Tacoma have very different readings
range(aqd2$Whitman.Elementary.School.A, na.rm=TRUE)
range(aqd2$Whitman.Elementary.School.B, na.rm=TRUE)

mean(c(aqd2$X350Tacoma01.A,aqd2$X350Tacoma01.B),na.rm=TRUE)
mean(c(aqd2$X350Tacoma01.A,aqd2$X350Tacoma01.B,aqd2$Whitman.Elementary.School.A, aqd2$Whitman.Elementary.School.B),na.rm=TRUE)

#plot A and B
x()
boxplot(aqd2[,3:6])
range(aqd2$DateTime)

plot(aqd2$X350Tacoma01.A, type="l",
     ylab="Air quality index",
     xlab="Collection #")
lines(aqd2$Whitman.Elementary.School.A, col="green")
