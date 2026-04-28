##################################################
######### Script to save PurpleAir data ########## 
####### from SD card synthesized data file, ######
#### format like other data files, & save as ##### 
####### purpleair_TPCH_Aug2025_to_Feb2026.csv ####
##################################################

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(magrittr)


setwd("~/Documents/GitHub/grit/analyses") 
# Setting working directory. Add in ailene's path in an if statement so that it works for her too
if(length(grep("ailene", getwd()))>0) {
  setwd("C:/Users/ailene.ettinger/Documents/GitHub/grit/analyses")
  folder <- "C:/Users/ailene.ettinger/Documents/GitHub/grit/data/PurpleAir/AQ_API_Download_TPCHTacomaPAs_02272026/"
}
## 720 possible points per day  Barkjohn paper keep 24 hr avg if at leat 90% possible points 
needed_measurements_120s = 0.9 * 720
                                   
## function for converting time:

convert_time <- function(x, tz = "America/Los_Angeles") {
  
  # Ensure character
  x <- as.character(x)
  
  # Normalize and CLEAN encoding (this fixes your error)
  x <- stringi::stri_enc_toutf8(x, is_unknown_8bit = TRUE)
  x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
  
  # Normalize Z/z
  x <- sub("Z$", "z", x)
  
  # Prepare output
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x) & nzchar(x)
  
  out[ok] <-
    ymd_hms(x[ok], tz = "UTC", quiet = TRUE) |>
    with_tz(tz) |>
    format("%Y-%m-%dT%H:%M:%S%z") |>
    (\(s) sub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", s))()
  
  out
}



dat <- read.csv("output/ALLPASDCARD_combined_2026March.csv", header = TRUE)
pa_info <- read.csv("../data/PurpleAir/PurpleAirAPIInfo.csv", header = TRUE)

#remove rows when id is missing- these are mostly (all?) frmo GRIT 23 and might be recoverable witht the PurpleAir data download tool
dat<-dat[!dat$id=="20251211.csv",]
dat<-dat[!dat$id=="",]
#add a column for sensor index
sensor_lookup<-subset(pa_info, select=c(Name, SensorIndex))
colnames(sensor_lookup)<-c("id","sensor_index")
df <- dat |>
  left_join(sensor_lookup, by = "id")
            
df$time_stamp <- convert_time(df$UTCDateTime)
df$pm2_5_atm_a <-as.numeric (df$pm2_5_atm_a)
df$pm2_5_atm_b <-as.numeric (df$pm2_5_atm_b)
df$current_humidity <-as.numeric (df$current_humidity)
df$current_temp_f <-as.numeric (df$current_temp_f)

df <- df %>% 
    mutate(
      time_stamp = as.POSIXct(time_stamp, format="%Y-%m-%dT%H:%M", tz="PST8PDT"),
      day = day(time_stamp),
      month = month(time_stamp),
      hour = hour(time_stamp),
      avg_pm = (pm2_5_atm_a + pm2_5_atm_b) / 2,
      avg_rh = current_humidity,
      avg_temp = current_temp_f
      
    )
  
  valid_dates <- df %>% 
    group_by(month, day) %>% 
    summarize(n_rows = n()) %>% 
    filter(n_rows >= needed_measurements_120s)
  
  df_filtered <- df %>% 
    inner_join(valid_dates, by=c("month", "day")) %>% 
    mutate(channel_diff = abs(pm2_5_atm_a - pm2_5_atm_b)) %>% 
    filter(channel_diff <= 5) %>% 
    group_by(month, day, hour) %>% 
    summarize(
      avg_pm = mean(avg_pm),
      avg_rh = mean(avg_rh),
      avg_temp = mean(avg_temp),
      .groups = "keep"
    ) %>%
    mutate(
      pm2.5_corrected = ( avg_pm * 0.524) - (0.0862 * avg_rh) + 5.75,
      sensor_index = sensor_index
    )
  



all_sensors <- lapply(files, function(f) {
  process_sensor(f, needed_measurements_120s)
})

purpleair_all <- bind_rows(all_sensors)
write.csv(purpleair_all, "output/purpleair_TPCH_Aug2025_to_Feb2026.csv", row.names = FALSE)                                          

purpleair_missing <- purpleair_all %>%
  group_by(month, day, sensor_index) %>%
  summarize(hours_present = n()) %>%
  mutate(missing_hours = 24 - hours_present) %>% filter(missing_hours != 0)
write.csv(purpleair_missing, "output/purpleair_missing_TPCH_Aug2025_to_Feb2026.csv", row.names = FALSE)  

head(purpleair_all)
