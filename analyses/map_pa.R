#Code to map GRIT Purple Air sensors
#set working directory
setwd("~/GitHub/grit/analyses")

# install.packages(c("sf", "ggplot2", "ggspatial", "ggrepel"))
library(sf)
library(ggplot2)
library(ggspatial)
library(ggrepel)
library(dplyr)
library(maptiles)


#read in csv file with lat long and other info about locations of GRIT sensors
locpm2.5hrs<-read.csv("output/purpleairloc_wpmhrs.csv", header=TRUE)                                          
#remove rows with NAs
pm2.5<-locpm2.5hrs[!is.na(locpm2.5hrs$Long),]
pm2.5<- pm2.5[!is.na(pm2.5$pm2.5est_sept),]
pm2.5<-subset(locpm2.5hrs,select=c("Purple.Air.Name", "Long","Lat","pm2.5est_sept","pm2.5est_jan","tempest_sept","tempest_jan" ))
tacoma_df<-pm2.5[!is.na(pm2.5$pm2.5est_sept),]

# Convert to sf (WGS84)
pts <- st_as_sf(tacoma_df, coords = c("Long", "Lat"), crs = 4326)

# Optionally: focus the map to the points' bounding box
bb <- st_bbox(pts)  # xmin, ymin, xmax, ymax in lon/lat


# Add padding (in degrees). Try 0.02–0.1; adjust as needed.
pad_x <- 0.1  # longitude padding
pad_y <- 0.1  # latitude padding

xlim <- c(bb$xmin - pad_x, bb$xmax + pad_x)
ylim <- c(bb$ymin - pad_y, bb$ymax + pad_y)


# Build static map with OSM tiles
p <- ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 12) +#previously type used was "osm"
  geom_sf(data = pts, aes(color = pm2.5est_sept), size = 4) +  # circles via point geometry
  # Labels (optional): repel to avoid overlaps
  # geom_text_repel(
  #   data = pts |> cbind(st_coordinates(pts)),
  #   aes(X, Y, label = Purple.Air.Name),
  #   size = 4, min.segment.length = 0, segment.color = "grey50",
  #   box.padding = 0.3, max.overlaps = 1000
  # ) +
  # Continuous gradient: green → yellow → orange → red
  scale_color_gradientn(
    colors = c("#2ECC71", "#F1C40F", "#E67E22", "#E74C3C"),
    name = "PM2.5 (Sept)"
  ) +
  
  coord_sf(
    crs = 4326,
    xlim = xlim,
    ylim = ylim,
    expand = FALSE
  ) +
  # --- Add scale bar and compass (north arrow) ---
  annotation_scale(
    location = "bl",               # "bl", "tl", "tr", "br"
    width_hint = 0.25,             # relative width of the scale bar
    unit_category = "metric"       # "metric" or "imperial"
  ) +
  annotation_north_arrow(
    location = "br",               # choose a corner
    which_north = "true",          # true north
    pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering  # or north_arrow_minimal
  ) +
  labs(
    title = "GRIT AQ Monitors, colored by mean PM2.5 in September 2025",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

p

# Save high-res
ggsave("gritpa_pm25_sept_simpler_nolabels.png", p, width = 12, height = 8, dpi = 300)
ggsave("gritpa_pm25_sept_simpler_nolabels.pdf", p, width = 12, height = 8)

#Using Maptile package

# Convert to Web Mercator (required!)
bb_3857 <- st_transform(st_as_sfc(bb), 3857)

# Get tiles
tile <- get_tiles(bb_3857, provider = "CartoDB.Positron", zoom = 12)

# Plot
p <- ggplot() +
  layer_spatial(tile) +
  geom_sf(data = pts, aes(color = pm2.5est_sept), size = 4) +
  scale_color_gradientn(
    colors = c("#2ECC71", "#F1C40F", "#E67E22", "#E74C3C"),
    name = "PM2.5 (Sept)"
  ) +
  
  coord_sf(
    crs = 4326,
    xlim = xlim,
    ylim = ylim,
    expand = FALSE
  ) +
  # --- Add scale bar and compass (north arrow) ---
  annotation_scale(
    location = "bl",               # "bl", "tl", "tr", "br"
    width_hint = 0.25,             # relative width of the scale bar
    unit_category = "metric"       # "metric" or "imperial"
  ) +
  annotation_north_arrow(
    location = "br",               # choose a corner
    which_north = "true",          # true north
    pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering  # or north_arrow_minimal
  ) +
  labs(
    title = "GRIT AQ Monitors, colored by mean PM2.5 in September 2025",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# Save high-res
ggsave("gritpa_pm25_sept_simpler_nolabels.png", p, width = 12, height = 8, dpi = 300)
ggsave("gritpa_pm25_sept_simpler_nolabels.pdf", p, width = 12, height = 8)


# Same map but with January data
tacomajan_df<-pm2.5[!is.na(pm2.5$pm2.5est_jan),]
# Convert to sf (WGS84)
pts <- st_as_sf(tacomajan_df, coords = c("Long", "Lat"), crs = 4326)

# Optionally: focus the map to the points' bounding box
bb <- st_bbox(pts)  # xmin, ymin, xmax, ymax in lon/lat


# Add padding (in degrees). Try 0.02–0.1; adjust as needed.
pad_x <- 0.1  # longitude padding
pad_y <- 0.1  # latitude padding

xlim <- c(bb$xmin - pad_x, bb$xmax + pad_x)
ylim <- c(bb$ymin - pad_y, bb$ymax + pad_y)


# Convert to Web Mercator (required!)
bb_3857 <- st_transform(st_as_sfc(bb), 3857)

# Get tiles
tile <- get_tiles(bb_3857, provider = "CartoDB.Positron", zoom = 12)

# Plot
p <- ggplot() +
  layer_spatial(tile) +
  geom_sf(data = pts, aes(color = pm2.5est_jan), size = 4) +
  scale_color_gradientn(
    colors = c("#2ECC71", "#F1C40F", "#E67E22", "#E74C3C"),
    name = "PM2.5 (Jan)"
  ) +
  
  coord_sf(
    crs = 4326,
    xlim = xlim,
    ylim = ylim,
    expand = FALSE
  ) +
  # --- Add scale bar and compass (north arrow) ---
  annotation_scale(
    location = "bl",               # "bl", "tl", "tr", "br"
    width_hint = 0.25,             # relative width of the scale bar
    unit_category = "metric"       # "metric" or "imperial"
  ) +
  annotation_north_arrow(
    location = "br",               # choose a corner
    which_north = "true",          # true north
    pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering  # or north_arrow_minimal
  ) +
  labs(
    title = "GRIT AQ Monitors, colored by mean PM2.5 in January 2026",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# Save high-res
ggsave("gritpa_pm25_jan.png", p, width = 12, height = 8, dpi = 300)
ggsave("gritpa_pm25_jan.pdf", p, width = 12, height = 8)



# make a map with points all one color for simple map
p2 <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 12) +
  geom_sf(data = pts, aes(color = "darkgreen"), size = 4) +  # circles via point geometry
  # Labels (optional): repel to avoid overlaps
  geom_text_repel(
    data = pts |> cbind(st_coordinates(pts)),
    aes(X, Y, label = Purple.Air.Name),
    size = 4, min.segment.length = 0, segment.color = "grey50",
    box.padding = 0.3, max.overlaps = 1000
  ) +
  
  coord_sf(
    crs = 4326,
    xlim = xlim,
    ylim = ylim,
    expand = FALSE
  ) +
  # --- Add scale bar and compass (north arrow) ---
  annotation_scale(
    location = "bl",               # "bl", "tl", "tr", "br"
    width_hint = 0.25,             # relative width of the scale bar
    unit_category = "metric"       # "metric" or "imperial"
  ) +
  annotation_north_arrow(
    location = "br",               # choose a corner
    which_north = "true",          # true north
    pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering  # or north_arrow_minimal
  ) +
  labs(
    title = "GRIT AQ Monitors, installed 2024-2025",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

p2

# Save high-res
ggsave("gritpas.png", p2, width = 12, height = 8, dpi = 300)
ggsave("gritpas.pdf", p2, width = 12, height = 8)

#Make a map on top of the equity index or livability index

# Load Equity Index shape file
equity.shp <- st_read("../data/tacoma_equity_index/Equity_Index_2024_(Tacoma)_-_Data.shp")

pts_equity_sf <- st_transform(pts, st_crs(equity.shp))

#Plot Livability
p3<-ggplot() +
  geom_sf(
    data = equity.shp, 
    aes(fill =livabili_1),   #
    color = "grey50",
    size = 0.2
  ) +
  geom_sf(data = pts_equity_sf, aes(color = pm2.5est_sept), size = 4) +
  scale_color_gradientn(
    colors = c("#2ECC71", "#F1C40F"),
    name = "PM2.5 (Sept)",
  ) +
  
  scale_fill_gradient(
    low = "white",
    high = "darkblue",
    name = "Livability Index"
  ) +
  theme_minimal() +
  labs(
    title = "PM2.5 with Livability Index Map",
    subtitle = "GRIT PurpleAir sensors with Tacoma Livability Index",
    x = "Longitude",
    y = "Latitude"
  )
# Save high-res
ggsave("gritpas_livability_sept.png", p3, width = 12, height = 8, dpi = 300)
ggsave("gritpas_livability_sept.pdf", p3, width = 12, height = 8)

#Plot Equity
p4<-ggplot() +
  geom_sf(
    data = equity.shp, 
    aes(fill =equityin_1),   #
    color = "grey50",
    size = 0.2
  ) +
  geom_sf(data = pts_equity_sf, aes(color = pm2.5est_sept), size = 4) +
  scale_color_gradientn(
    colors = c("#2ECC71", "#F1C40F"),
    name = "PM2.5 (Sept)",
  ) +
  
  scale_fill_gradient(
    low = "white",
    high = "purple3",
    name = "Equity Index"
  ) +
  theme_minimal() +
  labs(
    title = "PM2.5 with Equity Index Map",
    subtitle = "GRIT PurpleAir sensors with Tacoma Equity Index",
    x = "Longitude",
    y = "Latitude"
  )
# Save high-res
ggsave("gritpas_equity_sept.png", p4, width = 12, height = 8, dpi = 300)
ggsave("gritpas_equity_sept.pdf", p4, width = 12, height = 8)

#Plot Equity with no AQ
p4np<-ggplot() +
  geom_sf(
    data = equity.shp, 
    aes(fill =equityin_1),   #
    color = "grey50",
    size = 0.2
  ) +
  
  scale_fill_gradient(
    low = "white",
    high = "purple3",
    name = "Equity Index"
  ) +
  theme_minimal() +
  labs(
    title = "Tacoma Equity Index Map",
    x = "Longitude",
    y = "Latitude"
  )
# Save high-res
ggsave("tacomaequity.png", p4np, width = 12, height = 8, dpi = 300)
ggsave("tacomaequity.pdf", p4np, width = 12, height = 8)

#Plot urban tree cover
p5<-ggplot() +
  geom_sf(
    data = equity.shp, 
    aes(fill =urbantreec),   #
    color = "grey50",
    size = 0.2
  ) +
  geom_sf(data = pts_equity_sf, aes(color = pm2.5est_jan), size = 4) +
  scale_color_gradientn(
    colors = c("#2ECC71", "#F1C40F"),
    name = "PM2.5 (Jan)",
  ) +
  
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    name = "Tree Cover"
  ) +
  theme_minimal() +
  labs(
    title = "PM2.5 with Tree Cover",
    subtitle = "GRIT PurpleAir sensors with Tree Cover",
    x = "Longitude",
    y = "Latitude"
  )
# Save high-res
ggsave("gritpas_treec_jan.png", p5, width = 12, height = 8, dpi = 300)
ggsave("gritpas_treec_jan.pdf", p5, width = 12, height = 8)



#now join points with polygons to make a plot
# Join points to polygons
pts_with_equity <- st_join(
  pts_equity_sf,
  equity.shp,
  join = st_within   # ensures point falls inside polygon
)
#inspect reults
#names(pts_with_equity)
#any points without an equity index?
pts_with_equity |> filter(is.na(equityinde))# yes 4: GRIT04, GRIT19, GRIT20, GRIT24

# plot
p6<-ggplot(pts_with_equity, aes(x = equityin_1, y = pm2.5est_jan)) +
  geom_point(
    color = "black",
    alpha = 0.6,
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    color = "blue",
    se = TRUE
  ) +
  theme_minimal() +
  labs(
    title = "January PM2.5 vs Equity Index",
    x = "Equity Index",
    y = "PM2.5 (Jan)"
  )

# Save high-res
ggsave("janpm2.5vsequity.png", p6, width = 12, height = 8, dpi = 300)
ggsave("janpm2.5vsequity.pdf", p6, width = 12, height = 8)

# plot
p6<-ggplot(pts_with_equity, aes(x = livabili_1, y = pm2.5est_jan)) +
  geom_point(
    color = "black",
    alpha = 0.6,
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    color = "blue",
    se = TRUE
  ) +
  theme_minimal() +
  labs(
    title = "January PM2.5 vs Livability Index",
    x = "Livability Index",
    y = "PM2.5 (Jan)"
  )
summary(lm(pm2.5est_jan~livabili_1, data=pts_with_equity))

# Save high-res
ggsave("janpm2.5vslivibility.png", p6, width = 12, height = 8, dpi = 300)
ggsave("janpm2.5vslivibility.pdf", p6, width = 12, height = 8)
summary(lm(pm2.5est_jan~equityin_1, data=pts_with_equity))

p6<-ggplot(pts_with_equity, aes(x = equityin_1, y = pm2.5est_jan)) +
  geom_point(
    color = "black",
    alpha = 0.6,
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    color = "blue",
    se = TRUE
  ) +
  theme_minimal() +
  labs(
    title = "January PM2.5 vs Equity Index",
    x = "Equity Index",
    y = "PM2.5 (Jan)"
  )

# Save high-res
ggsave("janpm2.5vsequity.png", p6, width = 12, height = 8, dpi = 300)

#Tree cover
p7<-ggplot(pts_with_equity, aes(x = urbantreec, y = pm2.5est_jan)) +
  geom_point(
    color = "black",
    alpha = 0.6,
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    color = "blue",
    se = TRUE
  ) +
  theme_minimal() +
  labs(
    title = "January PM2.5 vs Tree Cover",
    x = "Tree Cover",
    y = "PM2.5 (Jan)"
  )

# Save high-res
ggsave("janpm2.5vstreecov.png", p7, width = 12, height = 8, dpi = 300)
summary(lm(pm2.5est_jan~urbantreec, data=pts_with_equity))
