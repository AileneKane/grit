# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
install.packages("patchwork")
install.packages("gridExtra")
install.packages("lubridate")

# load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)

setwd("~/Documents/GitHub/grit/analyses") 
folder <- "~/Documents/GitHub/grit/data/PurpleAir/PurpleAir_Download_2025Aug_to_Oct/PurpleAir Download 11-10-2025/"

## 720 possible points per day  Barkjohn paper keep 24 hr avg if at leat 90% possible points 
needed_measurements_120s = 0.9 * 720
                                   

process_sensor <- function(file_path, needed_measurements_120s) {
  
  sensor_index <- stringr::str_extract(basename(file_path), "^[0-9]+")
  
  df <- read.csv(file_path, header = TRUE)
  
  if (nrow(df) == 0 || !"time_stamp" %in% names(df)) {
    message("Skipping empty or invalid file: ", file_path)
    return(NULL)
  }
  df$sensor_index <- sensor_index
  
  df <- df %>% 
    mutate(
      time_stamp = as.POSIXct(time_stamp, format="%Y-%m-%dT%H:%M", tz="PST8PDT"),
      day = day(time_stamp),
      month = month(time_stamp),
      hour = hour(time_stamp),
      avg_pm = (pm2.5_atm_a + pm2.5_atm_b) / 2,
      avg_rh = (humidity_a + humidity_b) / 2
    )
  
  valid_dates <- df %>% 
    group_by(month, day) %>% 
    summarize(n_rows = n()) %>% 
    filter(n_rows >= needed_measurements_120s)
  
  df_filtered <- df %>% 
    inner_join(valid_dates, by=c("month", "day")) %>% 
    mutate(channel_diff = abs(pm2.5_atm_a - pm2.5_atm_b)) %>% 
    filter(channel_diff <= 5) %>% 
    group_by(month, day, hour) %>% 
    summarize(
      avg_pm = mean(avg_pm),
      avg_rh = mean(avg_rh),
      .groups = "drop"
    ) %>%
    mutate(
      pm2.5_corrected = ( avg_pm * 0.524) - (0.0862 * avg_rh) + 5.75,
      sensor_index = sensor_index
    )
  
  return(df_filtered)
}


files <- list.files(folder, pattern="\\.csv$", full.names = TRUE)

all_sensors <- lapply(files, function(f) {
  process_sensor(f, needed_measurements_120s)
})

purpleair_all <- bind_rows(all_sensors)
write.csv(purpleair_all, "~/Documents/GitHub/grit/analyses/output/purpleair_all.csv", row.names = FALSE)                                          

purpleair_missing <- purpleair_all %>%
  group_by(month, day, sensor_index) %>%
  summarize(hours_present = n()) %>%
  mutate(missing_hours = 24 - hours_present) %>% filter(missing_hours != 0)
write.csv(purpleair_missing, "~/Documents/GitHub/grit/analyses/output/purpleair_missing.csv", row.names = FALSE)        
