###GRIT###
###Using remote-sensed data to quantify greenness/canopy cover around air quality monitors in Tacoma
###Started July 9, 2024 by Samie and Ailene###
###Goal to quantify greenness in different radii/buffers around points (i.e. purpleair loggers)
##############################

#clear workspace
rm(list=ls()) 

#set strings default and avoid scientific notation for 6 digits
options(stringsAsFactors = FALSE, "scipen"=1000, "digits"=6)
# load libraries

library(dplyr)
library(lme4)
library(ggplot2)
library(car)
require(gridExtra)
library(RColorBrewer)
library(sf)
library(sp)
library(tidyverse)
library(ggpubr)#used in Locke et al 2020
library(sjPlot)
library(sjstats)
library(knitr)
#library(rgdal)  # for vector work; sp package should always load with rgdal. 
library(raster)
library(sp)
library(maps)
library(mapdata)
library(tigris) 
options(tigris_use_cache = TRUE)

setwd("/Users/samiebaclig/Documents/GitHub/grit/analyses") 
options("digits" = 15)

#Read in lat/longs of air quality monitors
#(need to creat this file first based on the loggers chosen frmo PurpleAir Map)
locs<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/GRIT_AQ_Monitors_Survey.csv", header=TRUE) 
locs_raw <-rename(locs, Longitude = x, Latitude = y)

# locs_raw<-aq_locs
#colnames(locs_raw)<-c("Purple.Air.Name","Longitude","Latitude") 

#read in Puget Sound landcover layer from stormwater heat map (https://tnc.app.box.com/s/rephyio647qxpy44uuvaspkq8p8h4yfu)
lc <- 
  raster("~/Documents/Land Cover/psLandCover_mosaic.tif")

lcNOAA <-
  raster("~/Documents/Land Cover/wa_2021_ccap_v2_hires_canopy_20240402.tif")

#read in polygon for Tacoma

st <- states()
tacoma <- places("WA", cb = TRUE) %>%
  filter(NAME %in% c("Tacoma")) %>%
  st_transform(6580)

wa_outline <- states(cb = TRUE) %>%
  filter(NAME == "Washington") %>%
  st_transform(6580)

#plot
ggplot() + 
  geom_sf(data = wa_outline) + 
  geom_sf(data = tacoma, fill = "red", color = NA) + 
  theme_void()



#Useful websitehttps://gis.stackexchange.com/questions/292327/creating-buffers-around-points-and-merging-with-spatialpolygonsdataframe-to-crea

#Convert points data frame to sf using these as coordinates:
  
locs = st_as_sf(locs_raw,coords=c("Longitude","Latitude"), crs="+proj=longlat +datum=WGS84")
  
#take the coordinate system of the lc file booths:
    
locs <- st_transform(locs, crs(lc))
locs_NOAA <- st_transform(locs, crs(lcNOAA))

#tacoma2<-st_transform(tacoma, crs(lc))
tacoma2<-st_transform(tacoma, crs(lcNOAA))

#crop lc raster to just Tacoma:
# lc_tacoma<-crop(x = lc, y = tacoma2)

lc_tacoma_NOAA<-crop(x = lcNOAA, y = tacoma2)

#create a polygon bounded by min max of lc

#make a map of points with land cover
png("figs/tacomagcanopymap.png", width=12, height=12, units="in", res=220)

plot(lc_tacoma_NOAA, 
     main = "Tacoma landcover", col = c("gray", "darkgreen", "springgreen"))
plot(locs_NOAA, add=TRUE, col="black", pch=16, cex=2)
dev.off()


#i think both are now in metric system
#Create buffer zones of 10-800m around locs:
    
locs_buffer10m = st_buffer(locs, 10)
locs_buffer20m = st_buffer(locs, 20)
locs_buffer30m = st_buffer(locs, 30)
locs_buffer40m = st_buffer(locs, 40)
locs_buffer50m = st_buffer(locs, 50)
locs_buffer100m = st_buffer(locs, 100)
locs_buffer200m = st_buffer(locs, 200)
locs_buffer400m = st_buffer(locs, 400)
locs_buffer800m = st_buffer(locs, 800)

#locs_buffer100m = st_buffer(locs, 100)

locs_buffer10m$area_sqm <- st_area(locs_buffer10m)#
locs_buffer20m$area_sqm <- st_area(locs_buffer20m)#
locs_buffer30m$area_sqm <- st_area(locs_buffer30m)#
locs_buffer40m$area_sqm <- st_area(locs_buffer40m)#
locs_buffer50m$area_sqm <- st_area(locs_buffer50m)#
locs_buffer100m$area_sqm <- st_area(locs_buffer100m)#
locs_buffer200m$area_sqm <- st_area(locs_buffer200m)#
locs_buffer400m$area_sqm <- st_area(locs_buffer400m)#
locs_buffer800m$area_sqm <- st_area(locs_buffer800m)#

#locs_buffer100m$area_sqm <- st_area(locs_buffer100m)

e10 <- raster::extract(x = lc_tacoma_NOAA, 
                        y = locs_buffer10m, 
                        df = TRUE)
e20 <- raster::extract(x = lc_tacoma_NOAA, 
                        y = locs_buffer20m, 
                        df = TRUE)

e30 <- raster::extract(x = lc_tacoma_NOAA, 
                       y = locs_buffer30m, 
                       df = TRUE)

e40 <- raster::extract(x = lc_tacoma_NOAA, 
                       y = locs_buffer40m, 
                       df = TRUE)
e50 <- raster::extract(x = lc_tacoma_NOAA, 
                       y = locs_buffer50m, 
                       df = TRUE)
e100 <- raster::extract(x = lc_tacoma_NOAA, 
                       y = locs_buffer100m, 
                       df = TRUE)
e200 <- raster::extract(x = lc_tacoma_NOAA, 
                       y = locs_buffer200m, 
                       df = TRUE)
e400 <- raster::extract(x = lc_tacoma_NOAA, 
                       y = locs_buffer400m, 
                       df = TRUE)
e800 <- raster::extract(x = lc_tacoma_NOAA,  y = locs_buffer800m, df = TRUE)

#1 = upland tree forest
#2 = scrub shrub 
#0 = background

#summarize land cover classes for each buffer polygon
e10sums<-cbind(rownames(table(e10$ID,e10$Layer_1)),table(e10$ID,e10$Layer_1))
e20sums<-cbind(rownames(table(e20$ID,e20$Layer_1)),table(e20$ID,e20$Layer_1))
e30sums<-cbind(rownames(table(e30$ID,e30$Layer_1)),table(e30$ID,e30$Layer_1))
e40sums<-cbind(rownames(table(e40$ID,e40$Layer_1)),table(e40$ID,e40$Layer_1))
e50sums<-cbind(rownames(table(e50$ID,e50$Layer_1)),table(e50$ID,e50$Layer_1))
e100sums<-cbind(rownames(table(e100$ID,e100$Layer_1)),table(e100$ID,e100$Layer_1))
e200sums<-cbind(rownames(table(e200$ID,e200$Layer_1)),table(e200$ID,e200$Layer_1))
e400sums<-cbind(rownames(table(e400$ID,e400$Layer_1)),table(e400$ID,e400$Layer_1))
e800sums<-cbind(rownames(table(e800$ID,e800$Layer_1)),table(e800$ID,e800$Layer_1))

#colnames(e20sums)


#merge all data together
e10sums.df<-as.data.frame(e10sums)
e20sums.df<-as.data.frame(e20sums)
e30sums.df<-as.data.frame(e30sums)
e40sums.df<-as.data.frame(e40sums)
e50sums.df<-as.data.frame(e50sums)
e100sums.df<-as.data.frame(e100sums)
e200sums.df<-as.data.frame(e200sums)
e400sums.df<-as.data.frame(e400sums)
e800sums.df<-as.data.frame(e800sums)

head(e100sums.df)
colnames(e10sums.df)<-c("ID","0.10m","1.10m","2.10m")
colnames(e20sums.df)<-c("ID","0.20m","1.20m","2.20m")
colnames(e30sums.df)<-c("ID","0.30m","1.30m","2.30m")
colnames(e40sums.df)<-c("ID","0.40m","1.40m","2.40m")
colnames(e50sums.df)<-c("ID","0.50m","1.50m","2.50m")
colnames(e100sums.df)<-c("ID","0.100m","1.100m","2.100m")
colnames(e200sums.df)<-c("ID","0.200m","1.200m","2.200m")
colnames(e400sums.df)<-c("ID","0.400m","1.400m","2.400m")
colnames(e800sums.df)<-c("ID","0.800m","1.800m","2.800m")


locs_raw$ID<-substr(locs_raw$Purple.Air.Name,6,6)

locslc1<-left_join(locs_raw,e10sums.df, by="ID", copy=FALSE)
locslc2<-left_join(locslc1,e20sums.df, by="ID", copy=FALSE)
locslc3<-left_join(locslc2,e30sums.df, by="ID", copy=FALSE)
locslc4<-left_join(locslc3,e40sums.df, by="ID", copy=FALSE)
locslc5<-left_join(locslc4,e50sums.df, by="ID", copy=FALSE)
locslc6<-left_join(locslc5,e100sums.df, by="ID", copy=FALSE)
locslc7<-left_join(locslc6,e200sums.df, by="ID", copy=FALSE)
locslc8<-left_join(locslc7,e400sums.df, by="ID", copy=FALSE)
locslc<-left_join(locslc8,e800sums.df, by="ID", copy=FALSE)

#new col for percent canopy cover 
locslc$cancov.10m<-as.numeric(locslc$"1.10m")/ sum(as.numeric(locslc$"0.10m"),as.numeric(locslc$"1.10m"),as.numeric(locslc$"2.10m"), na.rm = TRUE)
locslc$cancov.20m<-as.numeric(locslc$"1.20m")/ sum(as.numeric(locslc$"0.20m"), as.numeric(locslc$"1.20m"),as.numeric(locslc$"2.20m"), na.rm = TRUE)
locslc$cancov.30m<-as.numeric(locslc$"1.30m")/ sum(as.numeric(locslc$"0.30m"), as.numeric(locslc$"1.30m"), as.numeric(locslc$"2.30m"), na.rm = TRUE)
locslc$cancov.40m<-as.numeric(locslc$"1.40m")/ sum(as.numeric(locslc$"0.40m"),as.numeric(locslc$"1.40m"), as.numeric(locslc$"2.40m"), na.rm = TRUE)
locslc$cancov.50m<-as.numeric(locslc$"1.50m")/ sum(as.numeric(locslc$"0.50m"),as.numeric(locslc$"1.50m"),as.numeric(locslc$"2.50m"), na.rm = TRUE)
locslc$cancov.100m<-as.numeric(locslc$"1.100m")/ sum(as.numeric(locslc$"0.100m"), as.numeric(locslc$"1.100m"),as.numeric(locslc$"2.100"), na.rm = TRUE)
locslc$cancov.200m<-as.numeric(locslc$"1.200m")/ sum(as.numeric(locslc$"0.200m"),as.numeric(locslc$"1.200m"),as.numeric(locslc$"2.200"), na.rm = TRUE)
locslc$cancov.400m<-as.numeric(locslc$"1.400m")/ sum(as.numeric(locslc$"0.400m"),as.numeric(locslc$"1.400m"), as.numeric(locslc$"2.400"), na.rm = TRUE)
locslc$cancov.800m<-as.numeric(locslc$"1.800m")/ sum(as.numeric(locslc$"0.800m"),as.numeric(locslc$"1.800m"), as.numeric(locslc$"2.800"), na.rm = TRUE)

write.csv(locslc,"output/grit_aq_lc.csv", row.names = FALSE)
