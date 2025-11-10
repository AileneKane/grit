#Summarize NDVI in GRIT neighborhood pre- and post planting
#Goal is to quantify NDVI in GRIT neighborhood before and after madison district green infrastructure project
#load libraries
#library(sf)
library(terra)         # or raster
library(exactextractr) # for fast zonal stats

#load the polygon for the area of interest
polygon <- st_read("path_to_your_polygon.shp")

#load ndvi raster
ndvi <- rast("path_to_ndvi.tif")  # terra
# or
ndvi <- raster("path_to_ndvi.tif")  # raster

#make sure crs are the same for polygon and ndvi

polygon <- st_transform(polygon, crs(ndvi))

#extract ndvi for the polgyon using exactexractr

summary_stats <- exact_extract(ndvi, polygon, 'mean')


#Or use terra::extract:

values <- extract(ndvi, vect(polygon))
mean_ndvi <- mean(unlist(values), na.rm = TRUE)

#summarize ndvi
summary_stats <- exact_extract(ndvi, polygon, c('mean', 'min', 'max', 'median', 'sd'))

#visualize the ndvi
plot(ndvi)
plot(st_geometry(polygon), add = TRUE)



  