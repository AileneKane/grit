###GRIT###
###Using remote-sensed data to quantify greenness/canopy cover around air quality monitors in Tacoma
###Started January 16, 2026 by Samie and Ailene###
###Goal to quantify canopy cover, height, and health from 2024 Tacoma lidar 
###in different radii/buffers around points (i.e. purpleair loggers)
####################################

#clear workspace
rm(list=ls())


#set strings default and avoid scientific notation for 6 digits
options(stringsAsFactors = FALSE, "scipen"=1000, "digits"=8)
# load libraries

library(sf)
library(dplyr)
library(purrr)
library(tidyr)
library(lwgeom)

#set working directory
setwd("~/GitHub/grit/analyses")

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


#-----------------------------
# 3. READ GDB FILE
#    (and summarize tree height and conifer vs deciduous)
#-----------------------------
gdb_path <- "../data/tacoma2024lidar/data/LandCover2024.gdb"

layers <- st_layers(gdb_path)$name
print(layers)    # see available layers
#create a datafile for tree canopy
lc_gdb <- st_read(gdb_path, layer = layers[1]) |> 
  st_transform(32610)   # match buffer CRS
head(lc_gdb)
unique(lc_gdb$Class)
##below does not work
# #keep only tree canopy shapes in the canopy file
# canopy_gdb <- lc_gdb[lc_gdb$Class==1,]
# 
# #create a noncanopy datafile
# noncanopy_gdb <- lc_gdb[lc_gdb$Class!=1,]
# head(noncanopy_gdb)
# 
# #create a vegetation datafile
# veg_gdb<-lc_gdb[lc_gdb$Class==1|lc_gdb$Class==2,]
# #create a nonvegetation datafile
# nonveg_gdb<-lc_gdb[lc_gdb$Class==3|lc_gdb$Class==4|lc_gdb$Class==5|lc_gdb$Class==6|lc_gdb$Class==7,]
# 
# #create an impervious surface datafile
# imp_gdb<-lc_gdb[lc_gdb$Class==4|lc_gdb$Class==7|lc_gdb$Class==3,]
# nonimp_gdb<-lc_gdb[lc_gdb$Class==1|lc_gdb$Class==2|lc_gdb$Class==5|lc_gdb$Class==6,]

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
pttreeht.results.tosave<-as.data.frame(subset(pttreeht.results,select=c(ID,radius_m, mean_treeht_ft, total_conifers,total_nonconifer,n_features)))
pttreeht.results.tosave<-pttreeht.results.tosave[,1:6]
# # below does not work
# # Get total canopy cover, noncanopy cover, impervious surface, vegetation cover
# # below does not work! need to fix this
# #-----------------------------
# canopy.results <- st_intersection(buffers, canopy_gdb) |>
#   group_by(ID = Purple.Air.Name, radius_m) |>     # 
#   summarize(total_canopy  = sum(Shape_Area, na.rm = TRUE),
#             
#             n_features = n(),
#             .groups = "drop")
# 
# veg.results <- st_intersection(buffers, veg_gdb) |>
#   group_by(ID = Purple.Air.Name, radius_m) |>     # 
#   summarize(total_canopy  = sum(Shape_Area, na.rm = TRUE),
#             
#             n_features = n(),
#             .groups = "drop")
#-----------------------------
# 5. SAVE OUTPUT (optional)
#-----------------------------
write.csv(pttreeht.results.tosave, "output/canheight_within_buffers.csv", row.names = FALSE)


#now for canopy layer...the below is not ready and needs more work
#-----------------------------
# 0) USER INPUTS TO EDIT
#-----------------------------
lon_col    <- "longitude"              # <-- edit to match your CSV
lat_col    <- "latitude"               # <-- edit to match your CSV
id_col     <- "Purple.Air.Name"                     # <-- set to your point ID column (or will be created)

gdb_path  <- "../data/tacoma2024lidar/data/LandCover2024.gdb" # <-- path to your FileGDB
lc_layer   <- "LandCover2024"      # <-- layer name inside the GDB
class_field <- "Class"              # <-- column with classes 1..7

target_crs <- 32610                    # UTM Zone 10N (meters)


#-----------------------------
# 1) READ POINTS & MAKE BUFFERS
#-----------------------------

# Create sf points (WGS84) and project to meters
pts_sf <- st_as_sf(pts, coords = c(lon_col, lat_col), crs = 4326) |>
  st_transform(target_crs)

# Ensure a unique ID
if (!id_col %in% names(pts_sf)) {
  pts_sf[[id_col]] <- seq_len(nrow(pts_sf))
}

# Optional sanity check
message("Unique IDs: ", length(unique(pts_sf[[id_col]])))



#-----------------------------
# 2) READ LAND COVER POLYGONS
#-----------------------------
message("Listing GDB layers:")
print(st_layers(gdb_path)$name)

lc <- st_read(gdb_path, layer = lc_layer, quiet = TRUE) |>
  st_transform(target_crs)

# Keep only needed attributes + geometry
if (!class_field %in% names(lc)) {
  stop(sprintf("Field '%s' not found in layer '%s'. Available: %s",
               class_field, lc_layer, paste(names(lc), collapse = ", ")))
}
lc <- lc |>
  dplyr::select(!!class_field, Shape_Area)

# (Optional) Filter polygons to those touching any buffer to speed up
buffers_union <- st_union(st_geometry(buffers))
lc <- st_filter(lc, buffers_union, .predicate = st_intersects)

###trying to address scan error and confict error with invalid geometry

# Use planar GEOS ops (we're projected to meters already)
old_s2 <- sf_use_s2(FALSE)  # remember previous state

# 1) Basic cleaning: drop Z/M, empties; cast to polygonal types
buffers <- buffers |>
  st_zm(drop = TRUE, what = "ZM") |>
  filter(!st_is_empty(geometry))

lc <- lc |>
  st_zm(drop = TRUE, what = "ZM") |>
  filter(!st_is_empty(Shape))

# If your land cover is mixed or contains geometry collections, normalize it:
# (safe even if it's already MULTIPOLYGON)
lc <- suppressWarnings(st_cast(lc, "MULTIPOLYGON"))

# 2) Snap to a small grid to fix near-coincident edges (1 m here), then "0-buffer" to clean
prec <- 1  # meters
buffers <- buffers |>
  st_set_precision(prec) |>
  st_snap_to_grid(prec) |>
  st_buffer(0)

lc <- lc |>
  st_set_precision(prec) |>
  st_snap_to_grid(prec) |>
  st_buffer(0)

# 3) Validate only the bad ones with lwgeom::st_make_valid() if available
if (requireNamespace("lwgeom", quietly = TRUE)) {
  bad_b <- which(!st_is_valid(buffers))
  if (length(bad_b)) {
    message("Fixing ", length(bad_b), " invalid buffer(s) with st_make_valid()")
    buffers[bad_b, ] <- lwgeom::st_make_valid(buffers[bad_b, ])
  }
  bad_lc <- which(!st_is_valid(lc))
  if (length(bad_lc)) {
    message("Fixing ", length(bad_lc), " invalid land-cover polygon(s) with st_make_valid()")
    lc[bad_lc, ] <- lwgeom::st_make_valid(lc[bad_lc, ])
  }
}

# 4) Safe per-buffer intersection with retry
safe_intersection_one <- function(b) {
  # Preselect only LC polygons that intersect this buffer (speeds up & reduces failures)
  idx <- st_intersects(lc, b) |> lengths() > 0
  lc_sub <- lc[idx, , drop = FALSE]
  if (nrow(lc_sub) == 0) return(NULL)
  
  # First attempt
  res <- try(st_intersection(b, lc_sub), silent = TRUE)
  if (!inherits(res, "try-error")) return(res)
  
  # Retry with a fresh 0-buffer on both sides (more aggressive clean)
  message("Retrying ID=", b[[id_col]][1], " r=", b$radius_m[1],
          " with 0-buffer fallback due to: ", attr(res, "condition")$message)
  st_intersection(st_buffer(b, 0), st_buffer(lc_sub, 0))
}

# Run intersections buffer-by-buffer so one failure doesn't stop the run
int <- map_dfr(seq_len(nrow(buffers)), function(i) {
  b <- buffers[i, c(id_col, "radius_m")]
  safe_intersection_one(b)
})

# Restore s2 setting
sf_use_s2(old_s2)

#-----------------------------
# 3) CLIP & MEASURE AREAS
#-----------------------------
# Intersection: returns clipped pieces with both buffer attributes (ID, radius_m)
# and the land cover class.
int <- st_intersection(
  buffers |> dplyr::select(all_of(id_col), radius_m),
  lc      |> dplyr::select(all_of(class_field))
)

# Compute area of each clipped piece (square meters)
int <- int |>
  mutate(area_m2 = as.numeric(st_area(geometry)))  # units -> numeric m^2

#-----------------------------
# 4) SUM BY ID × RADIUS × CLASS
#-----------------------------
# Long format: one row per ID, radius, class
sum_long <- int |>
  st_drop_geometry() |>
  group_by(
    !!sym(id_col),
    radius_m,
    class = .data[[class_field]]
  ) |>
  summarize(area_m2 = sum(area_m2, na.rm = TRUE), .groups = "drop")

# Compute buffer area for proportions
buf_area <- buffers |>
  mutate(buffer_area_m2 = as.numeric(st_area(geometry))) |>
  st_drop_geometry() |>
  dplyr::select(all_of(id_col), radius_m, buffer_area_m2)

sum_long <- sum_long |>
  left_join(buf_area, by = c(id_col, "radius_m")) |>
  mutate(prop_of_buffer = ifelse(buffer_area_m2 > 0, area_m2 / buffer_area_m2, NA_real_))

# Wide format: columns class_1 ... class_7 (areas in m^2)
sum_wide_area <- sum_long |>
  mutate(class = paste0("class_", class)) |>
  select(all_of(id_col), radius_m, class, area_m2) |>
  tidyr::pivot_wider(names_from = class, values_from = area_m2, values_fill = 0)

# Wide format: proportions per buffer
sum_wide_prop <- sum_long |>
  mutate(class = paste0("class_", class)) |>
  select(all_of(id_col), radius_m, class, prop_of_buffer) |>
  tidyr::pivot_wider(names_from = class, values_from = prop_of_buffer, values_fill = 0)

#-----------------------------
# 5) SAVE RESULTS
#-----------------------------
write.csv(sum_long,      "lc_area_by_ID_radius_class_long.csv", row.names = FALSE)
write.csv(sum_wide_area, "lc_area_by_ID_radius_class_wide_area_m2.csv", row.names = FALSE)
write.csv(sum_wide_prop, "lc_prop_by_ID_radius_class_wide.csv", row.names = FALSE)

# Optional: save buffers and intersection for GIS QA/QC
st_write(buffers, "buffers.gpkg", delete_dsn = TRUE, quiet = TRUE)
st_write(int,     "lc_clipped_to_buffers.gpkg", delete_dsn = TRUE, quiet = TRUE)

message("Done. Files written:\n",
        " - lc_area_by_ID_radius_class_long.csv\n",
        " - lc_area_by_ID_radius_class_wide_area_m2.csv\n",
        " - lc_prop_by_ID_radius_class_wide.csv\n",
        " - buffers.gpkg\n",
        " - lc_clipped_to_buffers.gpkg")





#for ailene:
#write.csv(locslc,"analyses/output/grit_aq_lc_jul_aug_updated.csv", row.names = FALSE)
