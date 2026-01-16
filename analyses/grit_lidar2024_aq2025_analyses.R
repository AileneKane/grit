###GRIT###
###Using remote-sensed data to quantify greenness/canopy cover around air quality monitors in Tacoma
###Started January 16, 2026 by Samie and Ailene###
###Goal to quantify canopy cover, height, and health from 2024 Tacoma lidar 
###in different radii/buffers around points (i.e. purpleair loggers)
####################################

#clear workspace
rm(list=ls())


#set strings default and avoid scientific notation for 6 digits
options(stringsAsFactors = FALSE, "scipen"=1000, "digits"=6)
# load libraries

library(sf)
library(dplyr)
library(purrr)

#-----------------------------
# 1. READ LAT/LONG POINT LOCATIONS OF AQ Monitors FROM CSV
#-----------------------------

#read in csv file with lat long and other info about locations of GRIT sensors
locpm2.5hrs<-read.csv("output/purpleairloc_wpmhrs.csv", header=TRUE)                                          
#remove rows with NAs
pm2.5<-locpm2.5hrs[!is.na(locpm2.5hrs$Long),]
pm2.5<-subset(locpm2.5hrs,select=c("Purple.Air.Name", "Long","Lat","pm2.5est_sept" ))
pts<-pm2.5[!is.na(pm2.5$pm2.5est_sept),]
colnames(pts)[2:3]<-c("longitude", "latitude")
pts<-pts[!is.na(pts$longitude),]

# Ensure column names match your file:
# e.g., "longitude", "latitude"

pts_sf <- st_as_sf(pts,
                   coords = c("longitude", "latitude"),
                   crs = 4326) |>     # start in WGS84
  st_transform(32610)                # project to UTM (meters) — use correct zone

#-----------------------------
# 2. CREATE BUFFERS 5–800 m
#-----------------------------
radii_m <- c(5, 10, 25, 50, 100, 200, 400, 800)  # change as needed

buffers <- map_df(radii_m, function(r) {
  st_buffer(pts_sf, dist = r) |>
    mutate(radius_m = r)
})



# Ensure pts_sf has an ID column
pts_sf$ID <- seq_len(nrow(pts_sf))   # 45 unique IDs

# radii
radii_m <- c(5, 10, 25, 50, 100, 200, 400, 800)

library(dplyr)
library(purrr)
library(sf)

# Create buffers *and keep the ID column replicated per radius*
buffers <- map_df(radii_m, function(r) {
  pts_sf |>
    mutate(radius_m = r) |>      # attach radius
    st_buffer(dist = r)          # create buffer
})



#-----------------------------
# 3. READ GDB FILE
#    (choose the layer you want)
#-----------------------------
gdb_path <- "../data/tacoma2024lidar/data/LandCover2024.gdb"

layers <- st_layers(gdb_path)$name
print(layers)    # see available layers
#create a datafile for tree canopy
lc_gdb <- st_read(gdb_path, layer = layers[1]) |> 
  st_transform(32610)   # match buffer CRS
head(lc_gdb)
unique(lc_gdb$Class);unique(canopy_gdb$Description)
#keep only tree canopy shapes in the canopy file
canopy_gdb <- lc_gdb[lc_gdb$Class==1,]

#create a noncanopy datafile
noncanopy_gdb <- lc_gdb[lc_gdb$Class!=1,]
head(noncanopy_gdb)

#create a vegetation datafile
veg_gdb<-lc_gdb[lc_gdb$Class==1|lc_gdb$Class==2,]
#create a nonvegetation datafile
nonveg_gdb<-lc_gdb[lc_gdb$Class==3|lc_gdb$Class==4|lc_gdb$Class==5|lc_gdb$Class==6|lc_gdb$Class==7,]

#create an impervious surface datafile
imp_gdb<-lc_gdb[lc_gdb$Class==4|lc_gdb$Class==7|lc_gdb$Class==3,]
nonimp_gdb<-lc_gdb[lc_gdb$Class==1|lc_gdb$Class==2|lc_gdb$Class==5|lc_gdb$Class==6,]

#pull out canopy height layer using tree points
pttreeht_ft_gdb <- st_read(gdb_path, layer = layers[3]) |> 
  st_transform(32610)   # match buffer CRS
head(pttreeht_ft_gdb)
unique(pttreeht_ft_gdb$Conifer)
#add a column for deciduous
pttreeht_ft_gdb$NonConifer<-1
pttreeht_ft_gdb$NonConifer[pttreeht_ft_gdb$Conifer==1]<-0
#-----------------------------
# 4. SUMMARIZE DATA WITHIN EACH BUFFER
#
# Get mean tree height and number of conifers
#-----------------------------
pttreeht.results <- st_intersection(buffers, pttreeht_ft_gdb) |>
  group_by(ID = Purple.Air.Name, radius_m) |>     # 
  summarize(mean_treeht_ft = mean(Height_ft, na.rm = TRUE),
            total_conifers  = sum(Conifer, na.rm = TRUE),
            total_nonconifer  = sum(NonConifer, na.rm = TRUE),
            
            n_features = n(),
            .groups = "drop")
# Get total canopy cover, noncanopy cover, impervious surface, vegetation cover
#-----------------------------
canopy.results <- st_intersection(buffers, canopy_gdb) |>
  group_by(ID = Purple.Air.Name, radius_m) |>     # 
  summarize(total_canopy  = sum(Shape_Area, na.rm = TRUE),
            
            n_features = n(),
            .groups = "drop")

veg.results <- st_intersection(buffers, veg_gdb) |>
  group_by(ID = Purple.Air.Name, radius_m) |>     # 
  summarize(total_canopy  = sum(Shape_Area, na.rm = TRUE),
            
            n_features = n(),
            .groups = "drop")
#-----------------------------
# 5. SAVE OUTPUT (optional)
#-----------------------------
write.csv(pttreeht.results, "sheight_within_buffers.csv", row.names = FALSE)





#for ailene's computer:
setwd("~/GitHub/grit")
options("digits" = 15)

#Read in lat/longs of potential locations for air quality monitors
#potlocs<-read.csv("data/PurpleAirPotentialLocations2025GRIT.csv", header=TRUE, skip=1) 
# used potlocs for locs_raw below to quantify tree cover
# now using code below to quantify tree cover around existing purple air locations from survey123
survey123_merged<-read.csv("data/PurpleAir/purpleair2024locs.csv", header=TRUE) 


locs_raw <- potlocs %>%
  subset (select=c(Address,Latitude_degN,Longitude_degW))
colnames(locs_raw)<-c("Address","Latitude","Longitude") 
locs_clean<- locs_raw %>% drop_na()
  
#read in CCAP landcover layer from NOAA#UPDATE THIS TO USE THE NEW LiDARdataset

lcNOAA <-raster("data/wa_2021_ccap_v2_hires_canopy_20240402.tif")
imperviousNOAA <- rast("data/C-CAP/wa_2021_ccap_v2_hires_impervious_20240402.tif")
imperviousNOAA <- raster(imperviousNOAA)


#read in polygon for Tacoma
st <- states()
tacoma <- places("WA", cb = TRUE) %>%
  filter(NAME %in% c("Tacoma","Lakewood","University Place",
                     "Midland","Federal Way","Clover Creek","Fife",
                     "Gig Harbor","Graham", "Puyallup", "Artondale","Summit")) %>%
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
  
locs = st_as_sf(locs_clean,coords=c("Longitude","Latitude"), crs="+proj=longlat +datum=WGS84")
  
#take the coordinate system of the lc file booths:
    
locs <- st_transform(locs, crs(lcNOAA))
#for impervious cover
#locs_impervious <- st_transform(locs,crs(imperviousNOAA))

tacoma2<-st_transform(tacoma, crs(lcNOAA))

#for impervious cover
#tacoma2<-st_transform(tacoma, crs(imperviousNOAA))
#crop lc raster to just Tacoma:
#lc_tacoma<-crop(x = lc, y = tacoma2)

#using the Tacoma polygon seems to cut out one of our AQ monitors- not sure why. To get around this, create a new, larger, rectangular polygon that is a bit bigger than Tacoma, and use this to crop the landcover

bbox <- st_bbox(locs)
#bbox <- st_bbox(imperviousNOAA)


etacrect <- as(raster::extent(bbox$xmin-2000, bbox$xmax+2000,bbox$ymin-1000, bbox$ymax+1000), "SpatialPolygons")
proj4string(etacrect) <- crs(lcNOAA) #change to impervious
plot(etacrect)

#instead of using tacoma2 shapefile, use tacrect_
lc_tacoma_NOAA<-crop(x = lcNOAA, y = etacrect)
lc_tacoma2_NOAA<-crop(x = lcNOAA, y = tacoma2)
#lc_impervious<- crop(x= imperviousNOAA, y=etacrect)


#create a polygon bounded by min max of lc

#make a map of points with land cover
png("analyses/figs/potentialgritaqsites2025.png", width=12, height=12, units="in", res=220)

plot(lc_tacoma_NOAA, 
     main = "Tacoma canopy cover", col = c("lightgray", "darkgreen", "springgreen"))
plot(locs_NOAA, add=TRUE, col="black", pch=16, cex= 2)
dev.off()

png("analyses/figs/tacomagcanopymap_bigger2.png", width=12, height=12, units="in", res=220)

plot(lc_tacoma2_NOAA, 
     main = "Tacoma canopy cover", col = c("lightgray", "darkgreen", "springgreen"))
plot(locs_NOAA, add=TRUE, col="black", pch=16, cex= 2)
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

#locs_buffer10m$area_sqm <- st_area(locs_buffer100m)
#change x to lc_impervious for impervious cover
e10 <- raster::extract(x = lc_tacoma_NOAA, 
                        y = locs_buffer10m, 
                        df = TRUE,method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                        na.rm=FALSE, factors=FALSE)
e20 <- raster::extract(x = lc_tacoma_NOAA, 
                        y = locs_buffer20m, 
                       df = TRUE,method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                       na.rm=FALSE, factors=FALSE)
e30 <- raster::extract(x =lc_tacoma_NOAA, 
                       y = locs_buffer30m, 
                       df = TRUE,method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                       na.rm=FALSE, factors=FALSE)
e40 <- raster::extract(x = lc_tacoma_NOAA, 
                       y = locs_buffer40m, 
                       df = TRUE,method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                       na.rm=FALSE, factors=FALSE)
e50 <- raster::extract(x = lc_tacoma_NOAA, 
                       y = locs_buffer50m, 
                       df = TRUE,method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                       na.rm=FALSE, factors=FALSE)
e100 <- raster::extract(x = lc_tacoma_NOAA, 
                       y = locs_buffer100m, 
                       df = TRUE,method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                       na.rm=FALSE, factors=FALSE)
e200 <- raster::extract(x = lc_tacoma_NOAA, 
                       y = locs_buffer200m, 
                       df = TRUE,method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                       na.rm=FALSE, factors=FALSE)
e400 <- raster::extract(x = lc_tacoma_NOAA, 
                       y = locs_buffer400m, 
                       df = TRUE,method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                       na.rm=FALSE, factors=FALSE)
e800 <- raster::extract(x = lc_tacoma_NOAA,  y = locs_buffer800m, df = TRUE,method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                        na.rm=FALSE, factors=FALSE)

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

colnames(e10sums.df)<-c("Num","0.10m", "1.10m", "2.10m")
colnames(e20sums.df)<-c("Num","0.20m", "1.20m", "2.20m")
colnames(e30sums.df)<-c("Num","0.30m", "1.30m", "2.30m")
colnames(e40sums.df)<-c("Num","0.40m", "1.40m", "2.40m")
colnames(e50sums.df)<-c("Num","0.50m", "1.50m", "2.50m")
colnames(e100sums.df)<-c("Num","0.100m", "1.100m", "2.100m")
colnames(e200sums.df)<-c("Num","0.200m", "1.200m", "2.200m")
colnames(e400sums.df)<-c("Num","0.400m", "1.400m", "2.400m")
colnames(e800sums.df)<-c("Num","0.800m", "1.800m", "2.800m")


locs_clean$Num<-as.character(seq(from=1, to=13, by=1))

locslc1<-left_join(locs_clean,e10sums.df, by="Num", copy=FALSE)
locslc2<-left_join(locslc1,e20sums.df, by="Num", copy=FALSE)
locslc3<-left_join(locslc2,e30sums.df, by="Num", copy=FALSE)
locslc4<-left_join(locslc3,e40sums.df, by="Num", copy=FALSE)
locslc5<-left_join(locslc4,e50sums.df, by="Num", copy=FALSE)
locslc6<-left_join(locslc5,e100sums.df, by="Num", copy=FALSE)
locslc7<-left_join(locslc6,e200sums.df, by="Num", copy=FALSE)
locslc8<-left_join(locslc7,e400sums.df, by="Num", copy=FALSE)
locslc<-left_join(locslc8,e800sums.df, by="Num", copy=FALSE)


locslc <- locslc %>%
  mutate(across(5:31, ~ as.numeric(.)))
#new col for percent canopy cover 
locslc$cancov.10m<-as.numeric(locslc$"1.10m")/ rowSums(locslc[,5:7])
locslc$cancov.20m<-as.numeric(locslc$"1.20m")/ rowSums(locslc[,8:10])
locslc$cancov.30m<-as.numeric(locslc$"1.30m")/ rowSums(locslc[,11:13])
locslc$cancov.40m<-as.numeric(locslc$"1.40m")/ rowSums(locslc[,14:16])
locslc$cancov.50m<-as.numeric(locslc$"1.50m")/ rowSums(locslc[,17:19])
locslc$cancov.100m<-as.numeric(locslc$"1.100m")/ rowSums(locslc[,20:22])
locslc$cancov.200m<-as.numeric(locslc$"1.200m")/ rowSums(locslc[,23:25])
locslc$cancov.400m<-as.numeric(locslc$"1.400m")/ rowSums(locslc[,26:28])
locslc$cancov.800m<-as.numeric(locslc$"1.800m")/ rowSums(locslc[,29:31])

write.csv(locslc,"analyses/output/grit_aq_lc_ccap2025.csv", row.names = FALSE)

##for shrub + upland tree forest
locslc$"1.10m" <- as.numeric(locslc$"1.10m")
locslc$"2.10m" <- as.numeric(locslc$"2.10m")
locslc$"0.10m" <- as.numeric(locslc$"0.10m")
locslc$"0.20m" <- as.numeric(locslc$"0.20m")
locslc$"1.20m" <- as.numeric(locslc$"1.20m")
locslc$"2.20m"<- as.numeric(locslc$"2.20m")
locslc$"0.30m"<- as.numeric(locslc$"0.30m")
locslc$"1.30m"<- as.numeric(locslc$"1.30m")
locslc$"2.30m"<- as.numeric(locslc$"2.30m")
locslc$"0.40m"<- as.numeric(locslc$"0.40m")
locslc$"1.40m"<- as.numeric(locslc$"1.40m")

locslc$cancov.10m <- (as.numeric(locslc$"1.10m") + as.numeric(locslc$"2.10m")) / 
  rowSums(locslc[, c("0.10m", "1.10m", "2.10m")], na.rm = TRUE)
locslc$cancov.20m<-(as.numeric(locslc$"1.20m") +as.numeric(locslc$"2.20m") / 
  rowSums(locslc[, c("0.20m", "1.20m","2.20m")], na.rm = TRUE)


#new col for percent impervious cover 
locslc$impcov.10m<-as.numeric(locslc$"1.10m")/ sum(as.numeric(locslc$"0.10m"),as.numeric(locslc$"1.10m"), na.rm = TRUE)
locslc$impcov.20m<-as.numeric(locslc$"1.20m")/ sum(as.numeric(locslc$"0.20m"), as.numeric(locslc$"1.20m"), na.rm = TRUE)
locslc$impcov.30m<-as.numeric(locslc$"1.30m")/ sum(as.numeric(locslc$"0.30m"), as.numeric(locslc$"1.30m"), na.rm = TRUE)
locslc$impcov.40m<-as.numeric(locslc$"1.40m")/ sum(as.numeric(locslc$"0.40m"),as.numeric(locslc$"1.40m"), na.rm = TRUE)
locslc$impcov.50m<-as.numeric(locslc$"1.50m")/ sum(as.numeric(locslc$"0.50m"),as.numeric(locslc$"1.50m"),na.rm = TRUE)
locslc$impcov.100m<-as.numeric(locslc$"1.100m")/ sum(as.numeric(locslc$"0.100m"), as.numeric(locslc$"1.100m"),na.rm = TRUE)
locslc$impcov.200m<-as.numeric(locslc$"1.200m")/ sum(as.numeric(locslc$"0.200m"),as.numeric(locslc$"1.200m"), na.rm = TRUE)
locslc$impcov.400m<-as.numeric(locslc$"1.400m")/ sum(as.numeric(locslc$"0.400m"),as.numeric(locslc$"1.400m"), na.rm = TRUE)
locslc$impcov.800m<-as.numeric(locslc$"1.800m")/ sum(as.numeric(locslc$"0.800m"),as.numeric(locslc$"1.800m"), na.rm = TRUE)


write.csv(locslc,"~/Documents/GitHub/grit/analyses/output/grit_aq_imp_jul_aug_updated.csv", row.names = FALSE)

#for ailene:
#write.csv(locslc,"analyses/output/grit_aq_lc_jul_aug_updated.csv", row.names = FALSE)
