# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
install.packages("patchwork")
install.packages("gridExtra")
install.packages("lubridate")
install.packages("scales")
install.packages("zoo")
# load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(zoo)
setwd("~/Documents/GitHub/grit/analyses") 
folder <- "~/Documents/GitHub/grit/data/PurpleAir/PurpleAir_Download_2025Aug_to_Oct/PurpleAir Download 11-10-2025/"
# Setting working directory. Add in ailene's path in an if statement so that it works for her too
if(length(grep("ailene", getwd()))>0) {
  setwd("C:/Users/ailene.ettinger/Documents/GitHub/grit/analyses")
  folder <- "C:/Users/ailene.ettinger/Documents/GitHub/grit/data/PurpleAir/PurpleAir_Download_2025Aug_to_Oct/PurpleAir Download 11-10-2025/"
}
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
write.csv(purpleair_all, "output/purpleair_all.csv", row.names = FALSE)                                          

purpleair_missing <- purpleair_all %>%
  group_by(month, day, sensor_index) %>%
  summarize(hours_present = n()) %>%
  mutate(missing_hours = 24 - hours_present) %>% filter(missing_hours != 0)
write.csv(purpleair_missing, "output/purpleair_missing.csv", row.names = FALSE)  

###Time Series Graphs###
pa<- read.csv("output/purpleair_all.csv")
sensornames <- read.csv("../data/PurpleAir/PurpleAirAPIInfo.csv")
sensornames <- sensornames %>% drop_na(SensorIndex) %>% rename(sensor_index = SensorIndex)
pa$year <- 2025
pa$datetime <- make_datetime(
  year = pa$year,
  month = pa$month,
  day   = pa$day,
  hour  = pa$hour)
pa$date <- as_date(pa$datetime)
sensornames_clean <- sensornames %>%
  distinct(sensor_index, .keep_all = TRUE)
pa <- pa %>%
  left_join(sensornames_clean, by = "sensor_index") 
pa <- pa[, -c(14, 15)] ## included GRIT sensor names 


p <- ggplot(pa, aes(datetime, pm2.5_corrected))+
  geom_line(alpha = 0.5, linewidth = 0.3)+
  geom_hline(yintercept = 25, linetype = 2)+
  scale_x_datetime(date_breaks = "1 month", 
                   date_minor_breaks = "1 week",
                   date_labels = "%B",
                   limits =  as.POSIXct(c("2025-08-01", "2025-10-31")))+
  theme_classic()+
  labs(y ="PM2.5 (µg/m³)", 
       x = "Date", 
       title = "Average PM2.5 Concentration August - October 2025")

ggsave("PurpleAir figs/avg_pm2.5_allsensors.png", width = 6, height = 4, units = "in")

x <- ggplot (pa, aes(datetime,pm2.5_corrected))+
  geom_line()+
  facet_wrap(~Name)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(y ="PM2.5 (µg/m³)", x = "Date", title = "Average PM 2.5 Concentration by Sensor")
ggsave("PurpleAir figs/avg_pm2.5_bysensor.png", width = 20, height = 9, dpi = 300)

#what do average daily pm2.5 values suggest?
dailyavg<-aggregate(palocs$avg_pm, by=list(palocs$date,palocs$Purple.Air.Name), sum, na.rm=TRUE)


#Number of hours above 35.5 mg/m3 across the time series (by sensor and across all sensors)
locs<-read.csv("output/purpleair_locs_20242025.csv")
colnames(locs)[3]<-"sensor_index"
palocs<-left_join(pa,locs, copy=TRUE)
palocs$hrsabove35.5<-0#EPA standard = 35.5
palocs$hrsabove35.5[palocs$pm2.5_corrected >35.5]<-1
palocs$hrsabove25<-0#Puget Sound Clean Air health goal=25
palocs$hrsabove25[palocs$pm2.5_corrected>25]<-1
date.hrsabove25<-aggregate(palocs$hrsabove25, by=list(palocs$date,palocs$Purple.Air.Name), sum, na.rm=TRUE)
date.hrsabove35.5<-aggregate(palocs$hrsabove35.5, by=list(palocs$date,palocs$Purple.Air.Name), sum, na.rm=TRUE)
colnames(date.hrsabove25)<-c("datetime","sensor","hrsabove25")

date.hrsabove25$date <- as.POSIXct(date.hrsabove25$datetime, format = "%Y-%m-%d", tz = "UTC")
date.hrsabove25<-date.hrsabove25[order(date.hrsabove25$date),]

h25 <- ggplot(date.hrsabove25, aes(date, hrsabove25))+
  geom_line(alpha = 0.5, linewidth = 0.3)+
  scale_x_datetime(date_breaks = "1 month", 
                   date_minor_breaks = "1 week",
                   date_labels = "%B",
                   limits =  as.POSIXct(c("2025-08-01", "2025-10-31")))+
  theme_classic()+
  labs(y ="Hours with PM2.5 > 25 µg/m³", 
       x = "Date", 
       title = "Hours with PM2.5 > 25 µg/m³  August - October 2025")

ggsave("PurpleAir figs/hoursabove25_allsensors.png", width = 6, height = 4, units = "in")

#Which sensors had the worst  and best air quality? (highest average and greatest # of hours above threshold)?
worst10<-head(palocs[order(palocs$pm2.5_corrected, decreasing=TRUE),], n=10)#highest/worst values= 61.71485 58.41759 57.16004 39.46606 38.01056 36.50640
unique(worst10$Purple.Air.Name)
best10<-tail(palocs[order(palocs$pm2.5_corrected, decreasing=TRUE),], n=10)#highest/worst values= 61.71485 58.41759 57.16004 39.46606 38.01056 36.50640

#What days had the worst and best air quality?
unique(worst10$date)
unique(best10$date)
#Did air quality differ significantly across sites?
#select out just september
palocs_sept<-palocs[palocs$month==9,]
palocs_oct<-palocs[palocs$month==10,]

palocs_sept$Sensor<-as.factor(substr(palocs_sept$Purple.Air.Name,5,6))
palocs_oct$Sensor<-as.factor(substr(palocs_oct$Purple.Air.Name,5,6))

bp_sept<- ggplot(palocs_sept, aes(Sensor, pm2.5_corrected)) +
  geom_boxplot()

bp_oct<- ggplot(palocs_oct, aes(Sensor, pm2.5_corrected)) +
  geom_boxplot()
palocs_sept$Lat<-as.numeric(palocs_sept$Lat)
palocs_sept$Long<-as.numeric(palocs_sept$Long)
palocs$date<-as.factor(palocs$date)
library(lme4)
library(car)
septmmod<-lmer(pm2.5_corrected~-1+Purple.Air.Name + (1|date),data=palocs_sept)
Anova(septmmod)
summary(septmmod)
fixef(septmmod)

octmmod<-lmer(pm2.5_corrected~-1+Purple.Air.Name + (1|date),data=palocs_oct)
Anova(octmmod)
summary(octmmod)
fixef(octmmod)
#significant differenecs in air quality acorss purpleair sensors
septmmod<-lmer(pm2.5_corrected~-1+Purple.Air.Name + (1|date),data=palocs_sept)

# Save with custom dimensions
ggsave("PurpleAir figs/pm2.5boxplot_bysensor_sept.png", plot = bp_sept, width = 8, height = 6, units = "in")
ggsave("PurpleAir figs/pm2.5boxplot_bysensor_oct.png", plot = bp_oct, width = 8, height = 6, units = "in")
``

#On bad air quality days, where was the the worst  and best air quality? (highest average and greatest # of hours above threshold)?

#average daily air quality across all sensors
pm2.5_dailyavg_bysens<-aggregate(palocs$pm2.5_corrected, by=list(palocs$date,palocs$Purple.Air.Name), mean, na.rm=TRUE)
colnames(pm2.5_dailyavg_bysens)<-c("date","sensor","avg_pm2.5_corrected")
pm2.5_dailyavg_bysens$good<-0
pm2.5_dailyavg_bysens$good[pm2.5_dailyavg_bysens$avg_pm2.5_corrected<=9.0]<-1
pm2.5_dailyavg_bysens$mod<-0
pm2.5_dailyavg_bysens$mod[pm2.5_dailyavg_bysens$avg_pm2.5_corrected>9.0 & pm2.5_dailyavg_bysens$avg_pm2.5_corrected <=35.5]<-1
pm2.5_dailyavg_bysens$unhealthysens<-0
pm2.5_dailyavg_bysens$unhealthysens[pm2.5_dailyavg_bysens$avg_pm2.5_corrected>35.5 & pm2.5_dailyavg_bysens$avg_pm2.5_corrected <=55.5]<-1
pm2.5_dailyavg_bysens$unhealthy<-0
pm2.5_dailyavg_bysens$unhealthy[pm2.5_dailyavg_bysens$avg_pm2.5_corrected>55.5]<-1
sum(pm2.5_dailyavg_bysens$good)
sum(pm2.5_dailyavg_bysens$mod)
sum(pm2.5_dailyavg_bysens$unhealthysens)
sum(pm2.5_dailyavg_bysens$unhealthy)

length(unique(pm2.5_dailyavg_bysens$date[pm2.5_dailyavg_bysens$mod==1]))#32 days with moderate air quality average over 24 hours
length(unique(pm2.5_dailyavg_bysens$sensor[pm2.5_dailyavg_bysens$mod==1]))#32 days with moderate air quality average over 24 hours

length(unique(pm2.5_dailyavg_bysens$sensor[pm2.5_dailyavg_bysens$good==1]))# 84 days with moderate air quality average over 24 hours

unique(pm2.5_dailyavg_bysens$sensor)





  