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
library(rgdal)  # for vector work; sp package should always load with rgdal. 
library(raster)
library(sp)
library(maps)
library(mapdata)
library(tigris)

setwd("~/GitHub/grit/analyses")
options("digits" = 15)

#Read in lat/longs of air quality monitors
#(need to creat this file first based on the loggers chosen frmo PurpleAir Map)
locs<-read.csv("../data/HoboLocations_PoleLocations.csv", header=TRUE)

locs_raw<-locs
colnames(locs_raw)<-c("Name","Longitude","Latitude")

#read in Puget Sound landcover layer from stormwater heat map (https://tnc.app.box.com/s/rephyio647qxpy44uuvaspkq8p8h4yfu)
lc <- 
  raster("../data/psLandCover_mosaic.tif")

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

tacoma2<-st_transform(tacoma, crs(lc))
#crop lc raster to just Tacoma:
lc_tacoma<-crop(x = lc, y = tacoma2)


#create a polygon bounded by min max of lc


e <- as(raster::extent(538500, 540500, 5227000, 5231000), "SpatialPolygons")
proj4string(e) <- crs(lc)
plot(e)
lc_grit<-crop(x = lc, y =e )

#make a map of points with land cover
png("figs/tacomaglcmap.png", width=12, height=12, units="in", res=220)

plot(lc_tacoma, 
     main = "Tacoma landcover", col=c("springgreen","springgreen4","darkgreen","chocolate","skyblue","gray","darkgray"))
plot(locs, add=TRUE, col="black", pch=16)
dev.off()

#look at it just to check:
png("figs/gritlcmap.png", width=12, height=12, units="in", res=220)

plot(lc_grit, 
     main = "GRIT landcover", col=c("springgreen","springgreen4","darkgreen","chocolate","skyblue","gray","darkgray"))

plot(locs, add=TRUE, col="black", pch=16, cex=1.5)

dev.off()


###GRIT###
###Using remote-sensed data to quantify greenness/canopy cover around air quality loggers
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
options("digits" = 15)

#Read in lat/longs of temperature loggers
locs<-read.csv("../data/HoboLocations_PoleLocations.csv", header=TRUE)
source("sourced_files/clean_locs.R")
#remove locations where no temperature shield was present
locs$shield<-"YES"
locs$shield[locs$Location=="Wapato Hills 2"]<-"NO"
locs$shield[locs$Location=="Wapato Hills 1"]<-"NO"
locs$shield[locs$Location=="South Tacoma Wetland"]<-"NO"
locs$shield[locs$Location=="South Tacoma Wetland 1"]<-"NO"
locs<-locs[locs$shield=="YES",]

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


#create a polygon bounded by min max of lc


e <- as(raster::extent(538500, 540500, 5227000, 5231000), "SpatialPolygons")
proj4string(e) <- crs(lc)
plot(e)
lc_grit<-crop(x = lc, y =e )

#make a map of points with land cover
png("figs/tacomaglcmap.png", width=12, height=12, units="in", res=220)

plot(lc_tacoma, 
     main = "Tacoma landcover", col=c("springgreen","springgreen4","darkgreen","chocolate","skyblue","gray","darkgray"))
plot(locs, add=TRUE, col="black", pch=16)
dev.off()

#look at it just to check:
png("figs/gritlcmap.png", width=12, height=12, units="in", res=220)

plot(lc_grit, 
     main = "GRIT landcover", col=c("springgreen","springgreen4","darkgreen","chocolate","skyblue","gray","darkgray"))

plot(locs, add=TRUE, col="black", pch=16, cex=1.5)

dev.off()

#make a plot based on airtemp
tmin<-read.csv("output/meanTmin.csv", header=TRUE)
locstmin<-left_join(locs,tmin, by="Pole_No")
ryPal <- colorRampPalette(c('yellow','red'))

#This adds a column of color values
# based on the tmin values
locstmin$Col <- ryPal(20)[as.numeric(cut(locstmin$meanTmin,breaks = 20))]
colleg<-sort(unique(locstmin$Col), decreasing=TRUE)

pdf("figs/gritlcmap_mintemp.pdf", width=12, height=12)
par(mar=c(3,3,3,5)+0.5,xpd=TRUE)
plot(lc_grit, 
     col=c("springgreen","springgreen4","darkgreen","chocolate","skyblue","gray","darkgray"),
     xlim=c(538500,540500), ylim=c(522700,5231000), cex.lab=2, cex.axis=2)

plot(locs, add=TRUE, col=alpha(locstmin$Col, .8), pch=16, cex=2.5)

text(541000,5230050,"Landcover Class", cex=2)

text(540900,5230050,"Temperature", cex=2)
color.legend(540900,5228000,541000,5230000,legend=c(paste(round(min(locstmin$meanTmin, na.rm=TRUE), digits=0),"°C", sep=""),paste(round(max(locstmin$meanTmin, na.rm=TRUE), digits=0),"°C", sep="")),
                                                    rect.col=colleg,gradient="y", cex=2)
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
e100 <- raster::extract(x = lc_tacoma, 
                       y = locs_buffer100m, 
                       df = TRUE)
e200 <- raster::extract(x = lc_tacoma, 
                       y = locs_buffer200m, 
                       df = TRUE)
e400 <- raster::extract(x = lc_tacoma, 
                       y = locs_buffer400m, 
                       df = TRUE)
e800 <- raster::extract(x = lc_tacoma, 
                       y = locs_buffer800m, 
                       df = TRUE)
#6 and 7 = impervious
#1 = fine vegetation
#2 = med vegetation
#3 = coarse vegetation
#4 = dirt
#summarize land cover classes for each buffer polygon
e10sums<-cbind(rownames(table(e10$ID,e10$psLandCover_mosaic)),table(e10$ID,e10$psLandCover_mosaic))
e20sums<-cbind(rownames(table(e20$ID,e20$psLandCover_mosaic)),table(e20$ID,e20$psLandCover_mosaic))
e30sums<-cbind(rownames(table(e30$ID,e30$psLandCover_mosaic)),table(e30$ID,e30$psLandCover_mosaic))
e40sums<-cbind(rownames(table(e40$ID,e40$psLandCover_mosaic)),table(e40$ID,e40$psLandCover_mosaic))
e50sums<-cbind(rownames(table(e50$ID,e50$psLandCover_mosaic)),table(e50$ID,e50$psLandCover_mosaic))
e100sums<-cbind(rownames(table(e100$ID,e100$psLandCover_mosaic)),table(e100$ID,e100$psLandCover_mosaic))
e200sums<-cbind(rownames(table(e200$ID,e200$psLandCover_mosaic)),table(e200$ID,e200$psLandCover_mosaic))
e400sums<-cbind(rownames(table(e400$ID,e400$psLandCover_mosaic)),table(e400$ID,e400$psLandCover_mosaic))
e800sums<-cbind(rownames(table(e800$ID,e800$psLandCover_mosaic)),table(e800$ID,e800$psLandCover_mosaic))
#colnames(e20sums)
if(unique(colnames(e10sums)== c("","1","2","3","6","7"))==TRUE){colnames(e10sums)<-c("ID","1FineVeg.10m","2MedVeg.10m","3CoarseVeg.10m","6ImpOther.10m","7ImpRoofs.10m")}
if(unique(colnames(e20sums)== c("","1","2","3","6","7"))==TRUE){colnames(e20sums)<-c("ID","1FineVeg.20m","2MedVeg.20m","3CoarseVeg.20m","6ImpOther.20m","7ImpRoofs.20m")}
if(unique(colnames(e30sums)== c("","1","2","3","6","7"))==TRUE){colnames(e30sums)<-c("ID","1FineVeg.30m","2MedVeg.30m","3CoarseVeg.30m","6ImpOther.30m","7ImpRoofs.30m")}
if(unique(colnames(e40sums)== c("","1","2","3","6","7"))==TRUE){colnames(e40sums)<-c("ID","1FineVeg.40m","2MedVeg.40m","3CoarseVeg.40m","6ImpOther.40m","7ImpRoofs.40m")}
if(unique(colnames(e50sums)== c("","1","2","3","6","7"))==TRUE){colnames(e50sums)<-c("ID","1FineVeg.50m","2MedVeg.50m","3CoarseVeg.50m","6ImpOther.50m","7ImpRoofs.50m")}
if(unique(colnames(e100sums)== c("","1","2","3","6","7"))==TRUE){colnames(e100sums)<-c("ID","1FineVeg.100m","2MedVeg.100m","3CoarseVeg.100m","6ImpOther.100m","7ImpRoofs.100m")}
if(unique(colnames(e200sums)== c("","1","2","3","6","7"))==TRUE){colnames(e200sums)<-c("ID","1FineVeg.200m","2MedVeg.200m","3CoarseVeg.200m","6ImpOther.200m","7ImpRoofs.200m")}
if(unique(colnames(e400sums)== c("","1","2","3","4","6","7"))==TRUE){colnames(e400sums)<-c("ID","1FineVeg.400m","2MedVeg.400m","3CoarseVeg.400m","4Dirt.400m","6ImpOther.400m","7ImpRoofs.400m")}
if(unique(colnames(e800sums)== c("","1","2","3","4","5","6","7"))==TRUE){colnames(e800sums)<-c("ID","1FineVeg.800m","2MedVeg.800m","3CoarseVeg.800m","4Dirt.800m","5Water.800m","6ImpOther.800m","7ImpRoofs.800m")}

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

locs_raw$ID<-as.character(seq(from=1, to=dim(locs_raw)[1], by=1))

locslc1<-left_join(locs_raw,e10sums.df, by="ID", copy=FALSE)
locslc2<-left_join(locslc1,e20sums.df, by="ID", copy=FALSE)
locslc3<-left_join(locslc2,e30sums.df, by="ID", copy=FALSE)
locslc4<-left_join(locslc3,e40sums.df, by="ID", copy=FALSE)
locslc5<-left_join(locslc4,e50sums.df, by="ID", copy=FALSE)
locslc6<-left_join(locslc5,e100sums.df, by="ID", copy=FALSE)
locslc7<-left_join(locslc6,e200sums.df, by="ID", copy=FALSE)
locslc8<-left_join(locslc7,e400sums.df, by="ID", copy=FALSE)
locslc<-left_join(locslc8,e800sums.df, by="ID", copy=FALSE)

dim(locslc)
head(locslc)
write.csv(locslc,"output/grit_aq_lc.csv", row.names = FALSE)

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
e100 <- raster::extract(x = lc_tacoma, 
                       y = locs_buffer100m, 
                       df = TRUE)
e200 <- raster::extract(x = lc_tacoma, 
                       y = locs_buffer200m, 
                       df = TRUE)
e400 <- raster::extract(x = lc_tacoma, 
                       y = locs_buffer400m, 
                       df = TRUE)
e800 <- raster::extract(x = lc_tacoma, 
                       y = locs_buffer800m, 
                       df = TRUE)
#6 and 7 = impervious
#3 = coarse vegetation
#4 = dirt
#summarize land cover classes for each buffer polygon
e10sums<-cbind(rownames(table(e10$ID,e10$psLandCover_mosaic)),table(e10$ID,e10$psLandCover_mosaic))
e20sums<-cbind(rownames(table(e20$ID,e20$psLandCover_mosaic)),table(e20$ID,e20$psLandCover_mosaic))
e30sums<-cbind(rownames(table(e30$ID,e30$psLandCover_mosaic)),table(e30$ID,e30$psLandCover_mosaic))
e40sums<-cbind(rownames(table(e40$ID,e40$psLandCover_mosaic)),table(e40$ID,e40$psLandCover_mosaic))
e50sums<-cbind(rownames(table(e50$ID,e50$psLandCover_mosaic)),table(e50$ID,e50$psLandCover_mosaic))
e100sums<-cbind(rownames(table(e100$ID,e100$psLandCover_mosaic)),table(e100$ID,e100$psLandCover_mosaic))
e200sums<-cbind(rownames(table(e200$ID,e200$psLandCover_mosaic)),table(e200$ID,e200$psLandCover_mosaic))
e400sums<-cbind(rownames(table(e400$ID,e400$psLandCover_mosaic)),table(e400$ID,e400$psLandCover_mosaic))
e800sums<-cbind(rownames(table(e800$ID,e800$psLandCover_mosaic)),table(e800$ID,e800$psLandCover_mosaic))
#colnames(e20sums)
if(unique(colnames(e10sums)== c("","1","2","3","6","7"))==TRUE){colnames(e10sums)<-c("ID","1FineVeg.10m","2MedVeg.10m","3CoarseVeg.10m","6ImpOther.10m","7ImpRoofs.10m")}
if(unique(colnames(e20sums)== c("","1","2","3","6","7"))==TRUE){colnames(e20sums)<-c("ID","1FineVeg.20m","2MedVeg.20m","3CoarseVeg.20m","6ImpOther.20m","7ImpRoofs.20m")}
if(unique(colnames(e30sums)== c("","1","2","3","6","7"))==TRUE){colnames(e30sums)<-c("ID","1FineVeg.30m","2MedVeg.30m","3CoarseVeg.30m","6ImpOther.30m","7ImpRoofs.30m")}
if(unique(colnames(e40sums)== c("","1","2","3","6","7"))==TRUE){colnames(e40sums)<-c("ID","1FineVeg.40m","2MedVeg.40m","3CoarseVeg.40m","6ImpOther.40m","7ImpRoofs.40m")}
if(unique(colnames(e50sums)== c("","1","2","3","6","7"))==TRUE){colnames(e50sums)<-c("ID","1FineVeg.50m","2MedVeg.50m","3CoarseVeg.50m","6ImpOther.50m","7ImpRoofs.50m")}
if(unique(colnames(e100sums)== c("","1","2","3","6","7"))==TRUE){colnames(e100sums)<-c("ID","1FineVeg.100m","2MedVeg.100m","3CoarseVeg.100m","6ImpOther.100m","7ImpRoofs.100m")}
if(unique(colnames(e200sums)== c("","1","2","3","6","7"))==TRUE){colnames(e200sums)<-c("ID","1FineVeg.200m","2MedVeg.200m","3CoarseVeg.200m","6ImpOther.200m","7ImpRoofs.200m")}
if(unique(colnames(e400sums)== c("","1","2","3","4","6","7"))==TRUE){colnames(e400sums)<-c("ID","1FineVeg.400m","2MedVeg.400m","3CoarseVeg.400m","4Dirt.400m","6ImpOther.400m","7ImpRoofs.400m")}
if(unique(colnames(e800sums)== c("","1","2","3","4","5","6","7"))==TRUE){colnames(e800sums)<-c("ID","1FineVeg.800m","2MedVeg.800m","3CoarseVeg.800m","4Dirt.800m","5Water.800m","6ImpOther.800m","7ImpRoofs.800m")}

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

locs_raw$ID<-as.character(seq(from=1, to=dim(locs_raw)[1], by=1))

locslc1<-left_join(locs_raw,e10sums.df, by="ID", copy=FALSE)
locslc2<-left_join(locslc1,e20sums.df, by="ID", copy=FALSE)
locslc3<-left_join(locslc2,e30sums.df, by="ID", copy=FALSE)
locslc4<-left_join(locslc3,e40sums.df, by="ID", copy=FALSE)
locslc5<-left_join(locslc4,e50sums.df, by="ID", copy=FALSE)
locslc6<-left_join(locslc5,e100sums.df, by="ID", copy=FALSE)
locslc7<-left_join(locslc6,e200sums.df, by="ID", copy=FALSE)
locslc8<-left_join(locslc7,e400sums.df, by="ID", copy=FALSE)
locslc<-left_join(locslc8,e800sums.df, by="ID", copy=FALSE)

dim(locslc)
head(locslc)
write.csv(locslc,"output/grit_aq_lc.csv", row.names = FALSE)
