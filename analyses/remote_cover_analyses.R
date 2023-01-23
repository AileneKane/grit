###GRIT###
###Using remote-sensed data to quantify greenness/canopy cover around temp loggers
###Started Jan 2023###
###Goal to quantify greenness in different radii/buffers around points (e.g. temperature loggers, peoples homes)
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
library(rgdal)  # for vector work; sp package should always load with rgdal. 
library(raster)
library(sp)
library(maps)
library(mapdata)
library(tigris)

setwd("~/GitHub/grit/analyses")

#Read in lat/longs of temperature loggers
locs<-read.csv("../data/HoboLocations_PoleLocations.csv", header=TRUE)
source("sourced_files/clean_locs.R")
locs_raw<-locs
locs_raw <- as.data.frame(cbind(locs_raw$Pole_No, locs_raw$Longitude, locs_raw$Latitude))
locs_raw<-na.omit(locs_raw)
colnames(locs_raw)<-c("Pole_No","Longitude","Latitude")
#Read in lat/longs of residents (someday)

#read in Puget Sound landcover layer from stormwater heat map (https://tnc.app.box.com/s/rephyio647qxpy44uuvaspkq8p8h4yfu)
lc <- 
  raster("../data/psLandCover_mosaic.tif")

#read in polygon for TAcoma

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
tacoma2<-st_transform(tacoma, crs(lc))
#crop lc raster to just Tacoma:
lc_tacoma<-crop(x = lc, y = tacoma2)
lc_grit<-crop(x = lc, y = locs)#needto redo this

#lmake a map of points with land cover
png("figs/tacomagritlcmap.png", width=12, height=12, units="in", res=220)

plot(lc_tacoma, 
     main = "Tacoma landcover", col=c("springgreen","springgreen4","darkgreen","chocolate","skyblue","gray","darkgray"))
plot(locs, add=TRUE, col="black", pch=16)
dev.off()

#look at it just to check:
png("figs/gritlcmap.png", width=12, height=12, units="in", res=220)

plot(lc_grit, 
     main = "Tacoma landcover", col=c("springgreen","springgreen4","darkgreen","chocolate","skyblue","gray","darkgray"))
plot(locs, add=TRUE, col="black", pch=16)
dev.off()
#i think both are now in metric system
#Create buffer zones of 10-50 around locs:
    
locs_buffer10m = st_buffer(locs, 10)
locs_buffer20m = st_buffer(locs, 20)
locs_buffer30m = st_buffer(locs, 30)
locs_buffer40m = st_buffer(locs, 40)
locs_buffer50m = st_buffer(locs, 50)

#locs_buffer100m = st_buffer(locs, 100)

locs_buffer10m$area_sqm <- st_area(locs_buffer10m)#
locs_buffer20m$area_sqm <- st_area(locs_buffer20m)#
locs_buffer30m$area_sqm <- st_area(locs_buffer30m)#
locs_buffer40m$area_sqm <- st_area(locs_buffer40m)#
locs_buffer50m$area_sqm <- st_area(locs_buffer50m)#

#locs_buffer100m$area_sqm <- st_area(locs_buffer100m)

e10 <- raster::extract(x = lc_tacoma, 
                        y = locs_buffer10m, 
                        df = TRUE)
e20 <- raster::extract(x = lc_tacoma, 
                        y = locs_buffer20m, 
                        df = TRUE)

e30 <- raster::extract(x = lc_tacoma, 
                       y = locs_buffer30m, 
                       df = TRUE)

e40 <- raster::extract(x = lc_tacoma, 
                       y = locs_buffer40m, 
                       df = TRUE)
e50 <- raster::extract(x = lc_tacoma, 
                       y = locs_buffer50m, 
                       df = TRUE)
#6 and 7 = impervious
#3 = coarse vegetation
#summarize land cover classes for each buffer polygon
e10sums<-cbind(rownames(table(e10$ID,e10$psLandCover_mosaic)),table(e10$ID,e10$psLandCover_mosaic))
e20sums<-cbind(rownames(table(e20$ID,e20$psLandCover_mosaic)),table(e20$ID,e20$psLandCover_mosaic))
e30sums<-cbind(rownames(table(e30$ID,e30$psLandCover_mosaic)),table(e30$ID,e30$psLandCover_mosaic))
e40sums<-cbind(rownames(table(e40$ID,e40$psLandCover_mosaic)),table(e40$ID,e40$psLandCover_mosaic))
e50sums<-cbind(rownames(table(e50$ID,e50$psLandCover_mosaic)),table(e50$ID,e50$psLandCover_mosaic))
colnames(e10sums)<-c("ID","1FineVeg.10m","2MedVeg.10m","3CoarseVeg.10m","6ImpOther.10m","7ImpRoofs.10m")
colnames(e20sums)<-c("ID","1FineVeg.20m","2MedVeg.20m","3CoarseVeg.20m","6ImpOther.20m","7ImpRoofs.20m")
colnames(e30sums)<-c("ID","1FineVeg.30m","2MedVeg.30m","3CoarseVeg.30m","6ImpOther.30m","7ImpRoofs.30m")
colnames(e40sums)<-c("ID","1FineVeg.40m","2MedVeg.40m","3CoarseVeg.40m","6ImpOther.40m","7ImpRoofs.40m")
colnames(e50sums)<-c("ID","1FineVeg.50m","2MedVeg.50m","3CoarseVeg.50m","6ImpOther.50m","7ImpRoofs.50m")
  
#merge all data together
e10sums.df<-as.data.frame(e10sums)
e20sums.df<-as.data.frame(e20sums)
e30sums.df<-as.data.frame(e30sums)
e40sums.df<-as.data.frame(e40sums)
e50sums.df<-as.data.frame(e50sums)

locs_raw$ID<-as.character(seq(from=1, to=65, by=1))
locslc1<-left_join(locs_raw,e10sums.df, by="ID", copy=FALSE)
locslc2<-left_join(locslc1,e20sums.df, by="ID", copy=FALSE)
locslc3<-left_join(locslc2,e30sums.df, by="ID", copy=FALSE)
locslc4<-left_join(locslc3,e40sums.df, by="ID", copy=FALSE)
locslc<-left_join(locslc4,e50sums.df, by="ID", copy=FALSE)
dim(locslc)
head(locslc)
write.csv(locslc,"output/grit_logger_lc.csv", row.names = FALSE)
