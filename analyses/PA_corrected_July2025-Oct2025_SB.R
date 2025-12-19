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
####script for filtering raw sensor data####
test["sensor_index"] <- c(183885) #sensor index from pa_locs
test <- test %>%
  mutate(
    time_stamp =  as.POSIXct(test$time_stamp, format= "%Y-%m-%dT%H:%M", tz="PST8PDT"),
    day = day(time_stamp),
    month = month(time_stamp)
  )
per_day <- test %>% group_by(day) %>% ##grouped by days, count number of recording per days, tally is sum of all recordings, 720 recordings / day 
  tally()

#valid dates
valid_dates <- test %>% group_by(month,day) %>% summarize(n_rows=n()) %>% filter(n_rows>=720)
test<- test %>% inner_join(valid_dates, by = c("month","day"))
#valid readings if channel a and b were within  5 µg/m3,
test$channel_diff <- abs(test$pm2.5_atm_a - test$pm2.5_atm_b)
test_filter <- test[test$channel_diff<=5, ]



df <- data.frame()


grit03 <- read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir_Download_2025Aug_to_Oct/PurpleAir Download 11-10-2025/183883 2025-08-01 2025-11-01 0-Minute Average.csv",
                   header=TRUE)
grit03["sensor_index"] <- c(183883)
grit03<- grit03 %>% mutate(
  time_stamp =  as.POSIXct(grit03$time_stamp, format= "%Y-%m-%dT%H:%M", tz="PST8PDT"), #time zone 'PST8PDT' is PST 
  day = day(time_stamp),
  month = month(time_stamp),
  hour = hour(time_stamp),
  avg_pm = (pm2.5_atm_a + pm2.5_atm_b) / 2,
  avg_rh = (humidity_a + humidity_b) / 2,
  ) 
valid_dates <- grit03 %>% group_by(month, day) %>% summarize(n_rows=n()) %>% filter(n_rows>= needed_measurements_120s)
grit03_filter <- grit03 %>% 
  inner_join(valid_dates, by = c("month","day")) %>% 
  mutate(channel_diff = abs(pm2.5_atm_a - pm2.5_atm_b)) %>% 
  filter(channel_diff<=5) %>% 
  group_by(month, day, hour) %>% 
  summarize(hr_pm = mean(avg_pm), 
            hr_rh = mean(avg_rh)) %>%
  mutate(pm2.5_corrected = (hr_pm * 0.524)-(0.0862 * hr_rh) + 5.75,sensor_index = c("183883"))


df<-rbind(df, data_converted)





grit03_24hr_avg <- grit03_filter %>% 
  mutate(pm2.5_corrected = (hr_pm * 0.524)-(0.0862 * hr_rh) + 5.75,sensor_index = c("183883")) %>% 
  group_by(month,day) %>% 
  summarize(daily_avg = mean(pm2.5_corrected)) %>%
  mutate(sensor_index = c("183883"))
write.csv(grit03_24hr_avg, "~/Documents/GitHub/grit/analyses/output/grit03_24hr_avg.csv", row.names = FALSE)                                          



grit04 <- read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir_Download_2025Aug_to_Oct/PurpleAir Download 11-10-2025/183851 2025-08-01 2025-11-01 0-Minute Average.csv",
                   header=TRUE)
grit04["sensor_index"] <- c(183851)
grit04<- grit04 %>% mutate(
  time_stamp =  as.POSIXct(grit04$time_stamp, format= "%Y-%m-%dT%H:%M", tz="PST8PDT"), #time zone 'PST8PDT' is PST 
  day = day(time_stamp),
  month = month(time_stamp),
  hour = hour(time_stamp),
  avg_pm = (pm2.5_atm_a + pm2.5_atm_b) / 2,
  avg_rh = (humidity_a + humidity_b) / 2,)
valid_dates <- grit04 %>% group_by(month, day) %>% summarize(n_rows=n()) %>% filter(n_rows>= needed_measurements_120s)
grit04_filter <- grit04 %>% 
  inner_join(valid_dates, by = c("month","day")) %>% 
  mutate(channel_diff = abs(pm2.5_atm_a - pm2.5_atm_b)) %>% 
  filter(channel_diff<=5) %>% 
  group_by(month, day, hour) %>% 
  summarize(hr_pm = mean(avg_pm), 
            hr_rh = mean(avg_rh)) %>%
  mutate(pm2.5_corrected = (hr_pm * 0.524)-(0.0862 * hr_rh) + 5.75,sensor_index = c("183851"))
grit04_24hr_avg <- grit04_filter %>% 
  mutate(pm2.5_corrected = (hr_pm * 0.524)-(0.0862 * hr_rh) + 5.75,sensor_index = c("183851")) %>% 
  group_by(month,day) %>% 
  summarize(daily_avg = mean(pm2.5_corrected)) %>%
  mutate(sensor_index = c("183851"))
write.csv(grit04_24hr_avg, "~/Documents/GitHub/grit/analyses/output/grit04_24hr_avg.csv", row.names = FALSE)                                          



grit06 <- read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir_Download_2025Aug_to_Oct/PurpleAir Download 11-10-2025/183855 2025-08-01 2025-11-01 0-Minute Average.csv",header=TRUE)
grit06["sensor_index"] <- c(183855)
grit06<- grit06 %>% mutate(
  time_stamp =  as.POSIXct(grit06$time_stamp, format= "%Y-%m-%dT%H:%M", tz="PST8PDT"), #time zone 'PST8PDT' is PST 
  day = day(time_stamp),
  month = month(time_stamp),
  hour = hour(time_stamp),
  avg_pm = (pm2.5_atm_a + pm2.5_atm_b) / 2,
  avg_rh = (humidity_a + humidity_b) / 2,)
valid_dates <- grit06%>% group_by(month, day) %>% summarize(n_rows=n()) %>% filter(n_rows>= needed_measurements_120s)
grit06_filter <- grit06 %>% 
  inner_join(valid_dates, by = c("month","day")) %>% 
  mutate(channel_diff = abs(pm2.5_atm_a - pm2.5_atm_b)) %>% 
  filter(channel_diff<=5) %>% 
  group_by(month, day, hour) %>% 
  summarize(hr_pm = mean(avg_pm), 
            hr_rh = mean(avg_rh)) %>%
  mutate(pm2.5_corrected = (hr_pm * 0.524)-(0.0862 * hr_rh) + 5.75,sensor_index = c("183855"))
grit06_24hr_avg <- grit06_filter %>% 
  mutate(pm2.5_corrected = (hr_pm * 0.524)-(0.0862 * hr_rh) + 5.75,sensor_index = c("183855")) %>% 
  group_by(month,day) %>% 
  summarize(daily_avg = mean(pm2.5_corrected)) %>%
  mutate(sensor_index = c("183855"))
write.csv(grit06_24hr_avg, "~/Documents/GitHub/grit/analyses/output/grit06_24hr_avg.csv", row.names = FALSE)                                          



grit07 <- read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir_Download_2025Aug_to_Oct/PurpleAir Download 11-10-2025/183843 2025-08-01 2025-11-01 0-Minute Average.csv",header=TRUE)
grit07["sensor_index"] <- c(183843)
grit07<- grit07 %>% mutate(
  time_stamp =  as.POSIXct(grit07$time_stamp, format= "%Y-%m-%dT%H:%M", tz="PST8PDT"), #time zone 'PST8PDT' is PST 
  day = day(time_stamp),
  month = month(time_stamp),
  hour = hour(time_stamp),
  avg_pm = (pm2.5_atm_a + pm2.5_atm_b) / 2,
  avg_rh = (humidity_a + humidity_b) / 2,)
valid_dates <- grit07%>% group_by(month, day) %>% summarize(n_rows=n()) %>% filter(n_rows>= needed_measurements_120s)
grit07_filter <- grit07 %>% 
  inner_join(valid_dates, by = c("month","day")) %>% 
  mutate(channel_diff = abs(pm2.5_atm_a - pm2.5_atm_b)) %>% 
  filter(channel_diff<=5) %>% 
  group_by(month, day, hour) %>% 
  summarize(hr_pm = mean(avg_pm), 
            hr_rh = mean(avg_rh)) %>%
  mutate(pm2.5_corrected = (hr_pm * 0.524)-(0.0862 * hr_rh) + 5.75,sensor_index = c("183843"))
grit07_24hr_avg <- grit07_filter %>% 
  mutate(pm2.5_corrected = (hr_pm * 0.524)-(0.0862 * hr_rh) + 5.75,sensor_index = c("183843")) %>% 
  group_by(month,day) %>% 
  summarize(daily_avg = mean(pm2.5_corrected)) %>%
  mutate(sensor_index = c("183843"))
write.csv(grit07_24hr_avg, "~/Documents/GitHub/grit/analyses/output/sensor 24hr averages aug-oct 2025/grit07_24hr_avg.csv", row.names = FALSE)                                          



grit10 <- read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir_Download_2025Aug_to_Oct/PurpleAir Download 11-10-2025/183639 2025-08-01 2025-11-01 0-Minute Average.csv",header=TRUE)
grit10["sensor_index"] <- c(183639)
grit10<- grit10 %>% mutate(
  time_stamp =  as.POSIXct(grit10$time_stamp, format= "%Y-%m-%dT%H:%M", tz="PST8PDT"), #time zone 'PST8PDT' is PST 
  day = day(time_stamp),
  month = month(time_stamp),
  hour = hour(time_stamp),
  avg_pm = (pm2.5_atm_a + pm2.5_atm_b) / 2,
  avg_rh = (humidity_a + humidity_b) / 2,)
valid_dates <- grit10%>% group_by(month, day) %>% summarize(n_rows=n()) %>% filter(n_rows>= needed_measurements_120s)
grit10_filter <- grit10 %>% 
  inner_join(valid_dates, by = c("month","day")) %>% 
  mutate(channel_diff = abs(pm2.5_atm_a - pm2.5_atm_b)) %>% 
  filter(channel_diff<=5) %>% 
  group_by(month, day, hour) %>% 
  summarize(hr_pm = mean(avg_pm), 
            hr_rh = mean(avg_rh)) %>%
  mutate(pm2.5_corrected = (hr_pm * 0.524)-(0.0862 * hr_rh) + 5.75,sensor_index = c("183639"))
grit10_24hr_avg <- grit10_filter %>% 
  mutate(pm2.5_corrected = (hr_pm * 0.524)-(0.0862 * hr_rh) + 5.75,sensor_index = c("183639")) %>% 
  group_by(month,day) %>% 
  summarize(daily_avg = mean(pm2.5_corrected)) %>%
  mutate(sensor_index = c("183639"))
write.csv(grit10_24hr_avg, "~/Documents/GitHub/grit/analyses/output/sensor 24hr averages aug-oct 2025/grit10_24hr_avg.csv", row.names = FALSE)                                          



grit11<- read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir_Download_2025Aug_to_Oct/PurpleAir Download 11-10-2025/183869 2025-08-01 2025-11-01 0-Minute Average.csv",header=TRUE)
grit11["sensor_index"] <- c(183869)
grit11<- grit11%>% mutate(
  time_stamp =  as.POSIXct(grit11$time_stamp, format= "%Y-%m-%dT%H:%M", tz="PST8PDT"), #time zone 'PST8PDT' is PST 
  day = day(time_stamp),
  month = month(time_stamp),
  hour = hour(time_stamp),
  avg_pm = (pm2.5_atm_a + pm2.5_atm_b) / 2,
  avg_rh = (humidity_a + humidity_b) / 2,)
valid_dates <- grit11%>% group_by(month, day) %>% summarize(n_rows=n()) %>% filter(n_rows>= needed_measurements_120s)
grit11_filter <- grit11%>% 
  inner_join(valid_dates, by = c("month","day")) %>% 
  mutate(channel_diff = abs(pm2.5_atm_a - pm2.5_atm_b)) %>% 
  filter(channel_diff<=5) %>% 
  group_by(month, day, hour) %>% 
  summarize(hr_pm = mean(avg_pm), 
            hr_rh = mean(avg_rh)) %>%
  mutate(pm2.5_corrected = (hr_pm * 0.524)-(0.0862 * hr_rh) + 5.75,sensor_index = c("183869"))
grit11_24hr_avg <- grit11_filter %>% 
  mutate(pm2.5_corrected = (hr_pm * 0.524)-(0.0862 * hr_rh) + 5.75,sensor_index = c("183869")) %>% 
  group_by(month,day) %>% 
  summarize(daily_avg = mean(pm2.5_corrected)) %>%
  mutate(sensor_index = c("183869"))
write.csv(grit11_24hr_avg, "~/Documents/GitHub/grit/analyses/output/sensor 24hr averages aug-oct 2025/grit12_24hr_avg.csv", row.names = FALSE)                                          



grit12 <- read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir_Download_2025Aug_to_Oct/PurpleAir Download 11-10-2025/183877 2025-08-01 2025-11-01 0-Minute Average.csv",header=TRUE)
grit12["sensor_index"] <- c(183877)
grit12<- grit12 %>% mutate(
  time_stamp =  as.POSIXct(grit12$time_stamp, format= "%Y-%m-%dT%H:%M", tz="PST8PDT"), #time zone 'PST8PDT' is PST 
  day = day(time_stamp),
  month = month(time_stamp),
  hour = hour(time_stamp),
  avg_pm = (pm2.5_atm_a + pm2.5_atm_b) / 2,
  avg_rh = (humidity_a + humidity_b) / 2,)
valid_dates <- grit12%>% group_by(month, day) %>% summarize(n_rows=n()) %>% filter(n_rows>= needed_measurements_120s)
grit12_filter <- grit12 %>% 
  inner_join(valid_dates, by = c("month","day")) %>% 
  mutate(channel_diff = abs(pm2.5_atm_a - pm2.5_atm_b)) %>% 
  filter(channel_diff<=5) %>% 
  group_by(month, day, hour) %>% 
  summarize(hr_pm = mean(avg_pm), 
            hr_rh = mean(avg_rh)) %>%
  mutate(pm2.5_corrected = (hr_pm * 0.524)-(0.0862 * hr_rh) + 5.75,sensor_index = c("183877"))
grit12_24hr_avg <- grit12_filter %>% 
  mutate(pm2.5_corrected = (hr_pm * 0.524)-(0.0862 * hr_rh) + 5.75,sensor_index = c("183877")) %>% 
  group_by(month,day) %>% 
  summarize(daily_avg = mean(pm2.5_corrected)) %>%
  mutate(sensor_index = c("183877"))



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
