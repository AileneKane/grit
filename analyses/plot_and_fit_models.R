#################################################
######## Script to plot  PurpleAir data #########
######### and fit models to data in #############
############### purpleair_all.csv ###############
#################################################
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(zoo)
library(lme4)
library(car)

setwd("~/Documents/GitHub/grit/analyses") 
folder <- "~/Documents/GitHub/grit/data/PurpleAir/PurpleAir_Download_2025Aug_to_Oct/PurpleAir Download 11-10-2025/"
# Setting working directory. Add in ailene's path in an if statement so that it works for her too
if(length(grep("ailene", getwd()))>0) {
  setwd("C:/Users/ailene.ettinger/Documents/GitHub/grit/analyses")
  folder <- "C:/Users/ailene.ettinger/Documents/GitHub/grit/data/PurpleAir/PurpleAir_Download_2025Aug_to_Oct/PurpleAir Download 11-10-2025/"
}

###pa data###
pa<- read.csv("output/purpleair_all.csv")

#location data
gritlocs<-read.csv("output/purpleair_locs_20242025.csv")
colnames(gritlocs)[3]<-"sensor_index"
gritlocs_subs<-subset(gritlocs, select=c(Purple.Air.Name,sensor_index, Long, Lat))

tpchsensors <- read.csv("../data/PurpleAir/TPCH_PurpleAirMonitors.csv")
tpchsensors_to_use<-tpchsensors[tpchsensors$Data.downloaded.=="yes",]
tpchsensors_to_use<- tpchsensors_to_use %>% 
  rename(sensor_index = SensorIndex, Purple.Air.Name = Place.Name, Long = Longitude, Lat = Latitude)
tpchsensors_to_use<-subset(tpchsensors_to_use, select=c(Purple.Air.Name,sensor_index, Long, Lat))
locs<-rbind(gritlocs_subs,tpchsensors_to_use)
#remove GRIT 41, since this one is located outside tacoma
#save comnbined grit/tpch location purple air location file
locs<-locs[!locs$Purple.Air.Name =="GRIT41",]
#for some reason, sensor index 173501 ("Baker Middle School") has avg_pm but does not have avg_rh, avg_temp, or pm2.5_corrected
#temporaily remove
locs<-locs[!locs$Purple.Air.Name =="Baker Middle School",]

write.csv(locs,"output/grit_tpch_purpleair_locs_20242025.csv")
##time series graphs
pa$datetime <- make_datetime(
  year = pa$year,
  month = pa$month,
  day   = pa$day,
  hour  = pa$hour)
pa$date <- as_date(pa$datetime)
pa<-pa[!pa$Name=="GRIT41",]

#plot to check

p <- ggplot(pa, aes(datetime, pm2.5_corrected))+
  geom_line(alpha = 0.5, linewidth = 0.3)+
  geom_hline(yintercept = 25, linetype = 2, col="gold", linewidth = 1)+
  geom_hline(yintercept = 9, linetype = 2, col="darkgreen", linewidth = 1)+
  scale_x_datetime(date_breaks = "1 month", 
                   date_minor_breaks = "1 week",
                   date_labels = "%B",
                   limits =  as.POSIXct(c("2025-09-01", "2026-03-01")))+
  theme_classic()+
  
  #theme(
  #  axis.text = element_text(size = 18),      # tick labels (x & y)
  #  axis.title = element_text(size = 20),      # axis titles
  #  plot.title = element_text(size = 24)  # adjust size as needed
  #  
  #)+
  labs(y ="PM2.5 (µg/m³)", 
       x = "Date", 
       title = "Average PM2.5 Concentration, 1 Sept 2025 - 1 Mar 2026")


ggsave("PurpleAir figs/avg_pm2.5_allsensors.png", width = 6, height = 4, units = "in")


x <- ggplot (pa, aes(datetime,pm2.5_corrected))+
  geom_line()+
  facet_wrap(~Name)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(y ="PM2.5 (µg/m³)", x = "Date", title = "Average PM 2.5 Concentration by Sensor")
ggsave("PurpleAir figs/avg_pm2.5_bysensor.png", width = 20, height = 9, dpi = 300)


#Number of hours above 35.5 mg/m3 across the time series (by sensor and across all sensors)

pa<-subset(pa,
           select=c(sensor_index,year,month,day,hour,
                    avg_pm,avg_rh,avg_temp,pm2.5_corrected,
                    datetime,date))

palocs<-left_join(pa,locs, copy=TRUE)
palocs$hrsabove35.5<-0#EPA standard = 35.5
palocs$hrsabove35.5[palocs$pm2.5_corrected >35.5]<-1
palocs$hrsabove25<-0#Puget Sound Clean Air health goal=25
palocs$hrsabove25[palocs$pm2.5_corrected>25]<-1
palocs<-palocs[-which(is.na(palocs$Purple.Air.Name)),]
date.hrsabove25<-aggregate(palocs$hrsabove25, by=list(palocs$date,palocs$Purple.Air.Name), sum, na.rm=TRUE)
date.hrsabove35.5<-aggregate(palocs$hrsabove35.5, by=list(palocs$date,palocs$Purple.Air.Name), sum, na.rm=TRUE)
colnames(date.hrsabove25)<-c("datetime","sensor","hrsabove25")

date.hrsabove25$date <- as.POSIXct(date.hrsabove25$datetime, format = "%Y-%m-%d")
date.hrsabove25<-date.hrsabove25[order(date.hrsabove25$date),]
#remove dates before Sept 1,2025 and after MArch 1, 2026
date.hrsabove25<-date.hrsabove25[which(date.hrsabove25$date>"2025-08-31"),]
date.hrsabove25<-date.hrsabove25[which(date.hrsabove25$date<"2026-03-01"),]

date.hrsabove25$month<-as.factor(substr(date.hrsabove25$date,6,7))

monthsumtab<-aggregate(date.hrsabove25$hrsabove25, by=list(date.hrsabove25$month), sum, na.rm=TRUE)
colnames(monthsumtab)<-c("month","hrsabove24")

h25 <- ggplot(date.hrsabove25, aes(date, hrsabove25))+
  geom_line(alpha = 0.5, linewidth = 0.3)+
  scale_x_datetime(date_breaks = "1 month", 
                   date_minor_breaks = "1 week",
                   date_labels = "%B",
                   limits =  as.POSIXct(c("2025-09-01", "2026-03-01")))+
  theme_classic()+
  
  labs(y ="Hours with PM2.5 > 25 µg/m³", 
       x = "Date", 
       title = "Hours with PM2.5 > 25 µg/m³, 1 Sept 2025 - 1 Mar 2026")

ggsave("PurpleAir figs/hoursabove25_allsensors.png", width = 6, height = 4, units = "in")

#what do average daily pm2.5 values suggest?

dailyavg_allsens<-aggregate(palocs$pm2.5_corrected, by=list(palocs$date), mean, na.rm=TRUE)
colnames(dailyavg_allsens)<-c("date","pm2.5_avg_day")
dailyavg_allsens<-dailyavg_allsens[which(dailyavg_allsens$date>"2025-08-31"),]
dailyavg_allsens<-date.hrsabove25[which(dailyavg_allsens$date<"2026-03-01"),]

dailyavg_allsens$date<-as.POSIXct(dailyavg_allsens$date, format = "%Y-%m-%d")

# Example: df has columns datetime and pm25
# Convert datetime to Date and calculate daily mean
daily_avg <- palocs %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(avg_pm25 = mean(pm2.5_corrected, na.rm = TRUE)) %>%
  ungroup()
daily_avg<-daily_avg[which(daily_avg$date>"2025-08-31"),]
daily_avg<-daily_avg[which(daily_avg$date<"2026-03-01"),]

# Plot
ggplot(daily_avg, aes(x = date, y = avg_pm25)) +
  geom_line(color = "black", lwd=.8) +
  geom_hline(yintercept = 25, linetype = 2, col="gold", linewidth = 1)+
  geom_hline(yintercept = 9, linetype = 2, col="darkgreen", linewidth = 1)+
  scale_x_datetime(date_breaks = "1 month", 
                   date_minor_breaks = "1 week",
                   date_labels = "%B",
                   limits =  as.POSIXct(c("2025-09-01", "2026-03-01")))+
  
  labs(
    title = "Daily Average PM2.5",
    x = "Date",
    y = "PM2.5 (µg/m³)"
  ) +
  theme_minimal()
#+ geom_smooth(se = FALSE, color = "red")
ggsave("PurpleAir figs/dailyavg_allsensors.png", width = 6, height = 4, units = "in")


#make a plot with the sensors with worst data and no trees within 25m
# then make a plot with the sensor with lots of trees
#what do average daily pm2.5 values suggest?
dailyavg<-aggregate(palocs$pm2.5_corrected, by=list(palocs$date,palocs$Purple.Air.Name), mean, na.rm=TRUE)
colnames(dailyavg)<-c("date","sensor","pm2.5_avg_day")
dailyavg$date<-as.POSIXct(dailyavg$date, format = "%Y-%m-%d")
date.hrsabove25$sensor<-as.character(date.hrsabove25$sensor)
date.hrsabove25<-date.hrsabove25[,-(which(colnames(date.hrsabove25)=="datetime"))]
#merge dailyavg with daily hours above 25
dailyavg_hrsabove25<-left_join(date.hrsabove25,dailyavg)
#Which sensors had the worst  and best air quality? (highest average and greatest # of hours above threshold)?
worst10<-head(dailyavg_hrsabove25[order(dailyavg_hrsabove25$pm2.5_avg_day, decreasing=TRUE),], n=10)#highest/worst values= 61.71485 58.41759 57.16004 39.46606 38.01056 36.50640
unique(worst10$sensor)
best10<-tail(dailyavg_hrsabove25[order(dailyavg_hrsabove25$pm2.5_avg_day, decreasing=TRUE),], n=10)#highest/worst values= 61.71485 58.41759 57.16004 39.46606 38.01056 36.50640
unique(best10$sensor)
#What days had the worst and best air quality?
unique(worst10$date)
unique(best10$date)
#Did air quality differ significantly across sites?
#select out distinct months
palocs_sept<-palocs[palocs$month==9 & palocs$year==2025,]
palocs_oct<-palocs[palocs$month==10 & palocs$year==2025,]
palocs_nov<-palocs[palocs$month==11 & palocs$year==2025,]
palocs_dec<-palocs[palocs$month==12 & palocs$year==2025,]
palocs_jan<-palocs[palocs$month==1 & palocs$year==2026,]
palocs_feb<-palocs[palocs$month==2 & palocs$year==2026,]

palocs_sept$Sensor<-as.factor(substr(palocs_sept$Purple.Air.Name,5,6))
palocs_oct$Sensor<-as.factor(substr(palocs_oct$Purple.Air.Name,5,6))
palocs_nov$Sensor<-as.factor(substr(palocs_nov$Purple.Air.Name,5,6))
palocs_dec$Sensor<-as.factor(substr(palocs_dec$Purple.Air.Name,5,6))
palocs_jan$Sensor<-as.factor(substr(palocs_jan$Purple.Air.Name,5,6))
palocs_feb$Sensor<-as.factor(substr(palocs_feb$Purple.Air.Name,5,6))

dailyavg_sept<-dailyavg_hrsabove25[substr(dailyavg_hrsabove25$date,1,7)=="2025-09",]
dailyavg_oct<-dailyavg_hrsabove25[substr(dailyavg_hrsabove25$date,1,7)=="2025-10",]
dailyavg_nov<-dailyavg_hrsabove25[substr(dailyavg_hrsabove25$date,1,7)=="2025-11",]
dailyavg_dec<-dailyavg_hrsabove25[substr(dailyavg_hrsabove25$date,1,7)=="2025-12",]
dailyavg_jan<-dailyavg_hrsabove25[substr(dailyavg_hrsabove25$date,1,7)=="2026-01",]
dailyavg_feb<-dailyavg_hrsabove25[substr(dailyavg_hrsabove25$date,1,7)=="2026-02",]

bp_sept<- ggplot(palocs_sept, aes(Sensor, pm2.5_corrected)) +
  geom_boxplot()

bp_oct<- ggplot(palocs_oct, aes(Sensor, pm2.5_corrected)) +
  geom_boxplot()
palocs_sept$Lat<-as.numeric(palocs_sept$Lat)
palocs_sept$Long<-as.numeric(palocs_sept$Long)

palocs_jan$Lat<-as.numeric(palocs_jan$Lat)
palocs_jan$Long<-as.numeric(palocs_jan$Long)


palocs_sept$Purple.Air.Name<-as.factor(palocs_sept$Purple.Air.Name)
septmmod<-lmer(pm2.5_corrected~-1+Purple.Air.Name + (1|date),data=palocs_sept)
sept_temp_mmod<-lmer(avg_temp~-1+Purple.Air.Name + (1|date),data=palocs_sept)

palocs_oct$Purple.Air.Name<-as.factor(palocs_oct$Purple.Air.Name)
octmmod<-lmer(pm2.5_corrected~-1+Purple.Air.Name + (1|date),data=palocs_oct)
oct_temp_mmod<-lmer(avg_temp~-1+Purple.Air.Name + (1|date),data=palocs_oct)

palocs_nov$Purple.Air.Name<-as.factor(palocs_nov$Purple.Air.Name)
novmmod<-lmer(pm2.5_corrected~-1+Purple.Air.Name + (1|date),data=palocs_nov)
nov_temp_mmod<-lmer(avg_temp~-1+Purple.Air.Name + (1|date),data=palocs_nov)

palocs_dec$Purple.Air.Name<-as.factor(palocs_dec$Purple.Air.Name)
decmmod<-lmer(pm2.5_corrected~-1+Purple.Air.Name + (1|date),data=palocs_dec)
dec_temp_mmod<-lmer(avg_temp~-1+Purple.Air.Name + (1|date),data=palocs_dec)

palocs_jan$Purple.Air.Name<-as.factor(palocs_jan$Purple.Air.Name)
janmmod<-lmer(pm2.5_corrected~-1+Purple.Air.Name + (1|date),data=palocs_jan)
jan_temp_mmod<-lmer(avg_temp~-1+Purple.Air.Name + (1|date),data=palocs_jan)

palocs_feb$Purple.Air.Name<-as.factor(palocs_feb$Purple.Air.Name)
febmmod<-lmer(pm2.5_corrected~-1+Purple.Air.Name + (1|date),data=palocs_feb)
feb_temp_mmod<-lmer(avg_temp~-1+Purple.Air.Name + (1|date),data=palocs_feb)

#there are significant differences in air quality acorss purpleair sensors

#now a model for sept hours with pm2.5 greater than 25

date.hrsabove25$hrsabove25<-as.integer(date.hrsabove25$hrsabove25)
date.hrsabove25$sensor<-as.factor(date.hrsabove25$sensor)
date.hrsabove25$date<-as.factor(date.hrsabove25$date)

#hrsmmod<-glmer(hrsabove25~sensor + (1|date),family="poisson",data=date.hrsabove25)
#model did not converge
dailyavg_sept$hrsabove25<-as.integer(dailyavg_sept$hrsabove25)
dailyavg_oct$hrsabove25<-as.integer(dailyavg_oct$hrsabove25)
dailyavg_nov$hrsabove25<-as.integer(dailyavg_nov$hrsabove25)
dailyavg_dec$hrsabove25<-as.integer(dailyavg_dec$hrsabove25)
dailyavg_jan$hrsabove25<-as.integer(dailyavg_jan$hrsabove25)
dailyavg_feb$hrsabove25<-as.integer(dailyavg_feb$hrsabove25)

sept_hrsmmod<-glm(hrsabove25~-1+sensor,family="poisson",data=dailyavg_sept)
(sept_hrsmmod)
hrsmod<-glm(hrsabove25~sensor,family="poisson",data=date.hrsabove25)
#test for overdispersion
#hrsmod$deviance / hrsmod$df.residual#overdispersed(>1,so try negative binomial

Anova(hrsmod)
summary(hrsmod)

#get predicted hrs above the threshold for each sensor to plot on a map

# Ensure factor levels are consistent with the model
nd <- data.frame(sensor = stats::na.omit(unique(date.hrsabove25$sensor)))
nd$sensor <- factor(nd$sensor, levels = levels(date.hrsabove25$sensor))

# Predict on the link scale to get SEs, then transform back
p <- predict(hrsmod, newdata = nd, type = "link", se.fit = TRUE)

z <- qnorm(0.975)
eta <- p$fit
se  <- p$se.fit

# inverse-link for Poisson (exp)
fit      <- exp(eta)
fit_lwr  <- exp(eta - z * se)
fit_upr  <- exp(eta + z * se)

est_by_sensor <- cbind(nd, 
                       data.frame(
                         est = fit,
                         lwr = fit_lwr,
                         upr = fit_upr
                       )
)

colnames(est_by_sensor)<-c("Purple.Air.Name", "hrsabove25est","hrsabove25.lwr","hrsabove25_upr")

#let's save these in a format that we can map them...
sept_pm2.5ci<-confint(septmmod)
sept_pm2.5est<-fixef(septmmod)
septpm2.5<-as.data.frame(cbind(names(sept_pm2.5est),sept_pm2.5est,sept_pm2.5ci[3:35,]))
colnames(septpm2.5)<-c("Purple.Air.Name", "pm2.5est_sept","pm2.5.lwr_sept","pm2.5.upr_sept")
septpm2.5$Purple.Air.Name[septpm2.5$Purple.Air.Name=="Manito"]<-"Manitou Park Elementary School"
septpm2.5$Purple.Air.Name[septpm2.5$Purple.Air.Name=="N Taco"]<-"N Tacoma N 9th and Stevens"
septpm2.5$Purple.Air.Name[septpm2.5$Purple.Air.Name=="Seabur"]<-"Seabury School"
septpm2.5$Purple.Air.Name[septpm2.5$Purple.Air.Name=="Tacoma"]<-"Tacoma Center YMCA"

jan_pm2.5ci<-confint(janmmod)
jan_pm2.5est<-fixef(janmmod)
janpm2.5<-as.data.frame(cbind(names(jan_pm2.5est),jan_pm2.5est,jan_pm2.5ci[3:35,]))
colnames(janpm2.5)<-c("Purple.Air.Name", "pm2.5est_jan","pm2.5.lwr_jan","pm2.5.upr_jan")
janpm2.5$Purple.Air.Name[janpm2.5$Purple.Air.Name=="Manito"]<-"Manitou Park Elementary School"
janpm2.5$Purple.Air.Name[janpm2.5$Purple.Air.Name=="N Taco"]<-"N Tacoma N 9th and Stevens"
janpm2.5$Purple.Air.Name[janpm2.5$Purple.Air.Name=="Seabur"]<-"Seabury School"
janpm2.5$Purple.Air.Name[janpm2.5$Purple.Air.Name=="Tacoma"]<-"Tacoma Center YMCA"

#temperature mods
sept_tempci<-confint(sept_temp_mmod)
sept_tempest<-fixef(sept_temp_mmod)
sept_temp<-as.data.frame(cbind(names(sept_tempest),sept_tempest,sept_tempci[3:35,]))
colnames(sept_temp)<-c("Purple.Air.Name", "tempest_sept","temp.lwr_sept","temp.upr_sept")
sept_temp$Purple.Air.Name[sept_temp$Purple.Air.Name=="Manito"]<-"Manitou Park Elementary School"
sept_temp$Purple.Air.Name[sept_temp$Purple.Air.Name=="N Taco"]<-"N Tacoma N 9th and Stevens"
sept_temp$Purple.Air.Name[sept_temp$Purple.Air.Name=="Seabur"]<-"Seabury School"
sept_temp$Purple.Air.Name[sept_temp$Purple.Air.Name=="Tacoma"]<-"Tacoma Center YMCA"

jan_tempci<-confint(jan_temp_mmod).

jan_tempest<-fixef(jan_temp_mmod)
jan_temp<-as.data.frame(cbind(names(jan_tempest),jan_tempest,jan_tempci[3:35,]))
colnames(jan_temp)<-c("Purple.Air.Name", "tempest_jan","temp.lwr_jan","temp.upr_jan")
jan_temp$Purple.Air.Name[jan_temp$Purple.Air.Name=="Manito"]<-"Manitou Park Elementary School"
jan_temp$Purple.Air.Name[jan_temp$Purple.Air.Name=="N Taco"]<-"N Tacoma N 9th and Stevens"
jan_temp$Purple.Air.Name[jan_temp$Purple.Air.Name=="Seabur"]<-"Seabury School"
jan_temp$Purple.Air.Name[jan_temp$Purple.Air.Name=="Tacoma"]<-"Tacoma Center YMCA"
septjan_temp<-left_join(sept_temp,jan_temp)
septjan_temp$tempest_sept<-round(as.numeric(septjan_temp$tempest_sept), digits=3)
septjan_temp$tempest_jan<-round(as.numeric(septjan_temp$tempest_jan), digits=3)
septjan_temp$temp.lwr_sept<-round(as.numeric(septjan_temp$temp.lwr_sept), digits=3)
septjan_temp$temp.upr_sept<-round(as.numeric(septjan_temp$temp.upr_sept), digits=3)
septjan_temp$temp.lwr_jan<-round(as.numeric(septjan_temp$temp.lwr_jan), digits=3)
septjan_temp$temp.upr_jan<-round(as.numeric(septjan_temp$temp.upr_jan), digits=3)

#merge with location df
rownames(septpm2.5)<-NULL
septpm2.5$Purple.Air.Name<- sub("^Purple\\.Air\\.Name", "", septpm2.5$Purple.Air.Name)
rownames(janpm2.5)<-NULL
janpm2.5$Purple.Air.Name<- sub("^Purple\\.Air\\.Name", "", janpm2.5$Purple.Air.Name)

locpm2.5_sept<-left_jNULLlocpm2.5_sept<-left_join(locs,septpm2.5)
locpm2.5hrs_sept<-left_join(locpm2.5_sept,est_by_sensor)
locpm2.5hrs_septjan<-left_join(locpm2.5_sept,janpm2.5)

#add temperature to this file
septjan_temp$Purple.Air.Name<- sub("^Purple\\.Air\\.Name", "", septjan_temp$Purple.Air.Name)

locpm2.5hrs_septjan_temp<-left_join(locpm2.5hrs_septjan,septjan_temp)


write.csv(locpm2.5hrs_septjan_temp,"output/purpleairloc_wpmhrs.csv", row.names = FALSE)                                          

plot(locpm2.5hrs_septjan$pm2.5est_sept,locpm2.5hrs_septjan$pm2.5est_jan)
cor(as.numeric(locpm2.5hrs_septjan$pm2.5est_sept),as.numeric(locpm2.5hrs_septjan$pm2.5est_jan), use="pairwise.complete.obs")
#add other months, too

#0.84 highly correlated

#Do lat/long predict pm2.5?
septlatlongmmod<-lmer(pm2.5_corrected~1+Lat + Long + (1|date),data=palocs_sept)
summary(septlatlongmmod)
Anova(septlatlongmmod)
sort(fixef(septlatlongmmod), decreasing=TRUE)
#both lat and long were significant predictors.... (pm2.5 decreases further north, and increases further west)
janlatlongmmod<-lmer(pm2.5_corrected~1+Lat + Long + (1|date),data=palocs_jan)
summary(janlatlongmmod)
Anova(janlatlongmmod)
sort(fixef(janlatlongmmod), decreasing=TRUE)

# Save with custom dimensions
ggsave("PurpleAir figs/pm2.5boxplot_bysensor_sept.png", plot = bp_sept, width = 8, height = 6, units = "in")
ggsave("PurpleAir figs/pm2.5boxplot_bysensor_oct.png", plot = bp_oct, width = 8, height = 6, units = "in")
``

#On bad air quality days, where was the the worst  and best air quality? (highest average and greatest # of hours above threshold)?

#average daily air quality categories across all sensors
#remove dates before Sept 1,2025 and after MArch 1, 2026
palocs<-palocs[which(palocs$date>"2025-08-31"),]
palocs<-palocs[which(palocs$date<"2026-03-01"),]

pm2.5_dailyavg_bysens<-aggregate(palocs$pm2.5_corrected, by=list(palocs$date,palocs$Purple.Air.Name), mean, na.rm=TRUE)
colnames(pm2.5_dailyavg_bysens)<-c("date","sensor","avg_pm2.5_corrected")
pm2.5_dailyavg_bysens$good<-0
pm2.5_dailyavg_bysens$good[pm2.5_dailyavg_bysens$avg_pm2.5_corrected<=9.0]<-1
pm2.5_dailyavg_bysens$mod<-0
pm2.5_dailyavg_bysens$mod[pm2.5_dailyavg_bysens$avg_pm2.5_corrected>9.0 & pm2.5_dailyavg_bysens$avg_pm2.5_corrected <=35.5]<-1
pm2.5_dailyavg_bysens$unhealthysens<-0
pm2.5_dailyavg_bysens$unhealthysens[pm2.5_dailyavg_bysens$avg_pm2.5_corrected>35.5 & pm2.5_dailyavg_bysens$avg_pm2.5_corrected <=55.5]<-1
pm2.5_dailyavg_bysens$unhealthy<-0
pm2.5_dailyavg_bysens$unhealthy[pm2.5_dailyavg_bysens$avg_pm2.5_corrected>55.5 & pm2.5_dailyavg_bysens$avg_pm2.5_corrected <=125.5]<-1
pm2.5_dailyavg_bysens$veryunhealthy<-0
pm2.5_dailyavg_bysens$veryunhealthy[pm2.5_dailyavg_bysens$avg_pm2.5_corrected>125.5]<-1

dim(pm2.5_dailyavg_bysens)

sum(pm2.5_dailyavg_bysens$good)
sum(pm2.5_dailyavg_bysens$mod)
sum(pm2.5_dailyavg_bysens$unhealthysens)
sum(pm2.5_dailyavg_bysens$unhealthy)
sum(pm2.5_dailyavg_bysens$veryunhealthy)


length(unique(pm2.5_dailyavg_bysens$date[pm2.5_dailyavg_bysens$unhealthysens==1]))#1 day with unhealthysens air quality average over 24 hours during sept-feb
unhsensdates<-unique(unique(pm2.5_dailyavg_bysens$date[pm2.5_dailyavg_bysens$unhealthysens==1]))
unique(pm2.5_dailyavg_bysens$sensor[pm2.5_dailyavg_bysens$unhealthysens==1])#1 sensor with unhealthysens "GRIT25"

length(unique(pm2.5_dailyavg_bysens$date[pm2.5_dailyavg_bysens$unhealthy==1]))#1 day with unhealthy air quality average over 24 hours during sept-feb
unhdates<-unique(pm2.5_dailyavg_bysens$date[pm2.5_dailyavg_bysens$unhealthy==1])
unique(pm2.5_dailyavg_bysens$sensor[pm2.5_dailyavg_bysens$unhealthy==1])#1 sensor with unhealthysens "GRIT03"

length(unique(pm2.5_dailyavg_bysens$date[pm2.5_dailyavg_bysens$veryunhealthy==1]))#0 day with very unhealthy air quality average over 24 hours during sept-feb

length(unique(pm2.5_dailyavg_bysens$sensor[pm2.5_dailyavg_bysens$mod==1]))#35 sensors with moderate air quality average over 24 hours

length(unique(pm2.5_dailyavg_bysens$date[pm2.5_dailyavg_bysens$mod==1]))#78 days with moderate air quality average over 24 hours during sept-feb
moddates<-unique(pm2.5_dailyavg_bysens$date[pm2.5_dailyavg_bysens$mod==1])
mod_dates <- setdiff(moddates,unhdates)
mod_dates <- setdiff(mod_dates,unhsensdates)
length(mod_dates)
unique(pm2.5_dailyavg_bysens$sensor[pm2.5_dailyavg_bysens$mod==1])#34 sensors

length(unique(pm2.5_dailyavg_bysens$date[pm2.5_dailyavg_bysens$good==1]))#167 days with good air quality average over 24 hours during sept-feb
gooddates<-unique(pm2.5_dailyavg_bysens$date[pm2.5_dailyavg_bysens$good==1])
good_dates <- setdiff(gooddates, moddates)
length(good_dates)#103 days with good air quality at ALL sensorsaverage over 24 hours during sept-feb
unique(pm2.5_dailyavg_bysens$sensor[pm2.5_dailyavg_bysens$good==1])#34 sensors

unique(pm2.5_dailyavg_bysens$sensor)

#Make pie chart of days
# Sample data
values <- c(103,76,1,1,0)
labels <- c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy/Hazardous")
labels_with_counts <- paste(labels, "(", values, ") days", sep = "")
# Create pie chart
pdf("AQdays_pie_chart.pdf", width = 8, height = 6)

pie(values, labels = labels_with_counts, main = "Air quality, 1 Sept 2025 - 1 Mar 2026", col = c("springgreen4","gold","darkorange","darkred","thistle3"))

legend("topright",
       legend = labels,
       fill = c("springgreen4","gold","darkorange","darkred","thistle3"))

dev.off()
#calculate the average across the whole 6 month time period for each sensor- are any above the epa stnadard of 9
pm2.5_sensavg<-aggregate(palocs$pm2.5_corrected, by=list(palocs$Purple.Air.Name), mean, na.rm=TRUE)
pm2.5_sensavg[which(pm2.5_sensavg$x>9),]#GRIT 32 exceeded 
length(which(is.na(palocs$pm2.5_corrected[palocs$Purple.Air.Name=="GRIT32"])))
             
#what time of day was worst?
septhrmmod<-lmer(pm2.5_corrected~hour + (1|Purple.Air.Name),data=palocs_sept)
Anova(septhrmmod)
summary(septhrmmod)


ggplot(palocs_sept, aes(factor(hour), pm2.5_corrected)) +
  geom_violin(fill = "#f1c40f", color = NA, alpha = 0.5) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.3) +
  labs(
    title = "PM2.5 distributions by hour of day (September, all sensors)",
    x = "Hour of day", y = "PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 12)


plot<-ggplot(palocs_sept %>% group_by(Purple.Air.Name, hour) %>% summarise(pm25_mean = mean(pm2.5_corrected), .groups = "drop"),
             aes(hour, pm25_mean, color = Purple.Air.Name, group = Purple.Air.Name)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 0:23) +
  guides(color = guide_legend(ncol = 2)) +
  labs(
    title = "Diurnal PM2.5 by Sensor (September)",
    x = "Hour of day", y = "Mean PM2.5 (µg/m³)", color = "Sensor"
  ) +
  theme_minimal(base_size = 12)


ggplot(palocs_sept, aes(hour + minute(datetime)/60, pm2.5_corrected, color = Purple.Air.Name)) + geom_smooth(se = FALSE, method = "loess", span = 0.3, size = 0.8) +
  scale_x_continuous(breaks = 0:23, limits = c(0, 24)) +
  labs(
    title = "Smoothed PM2.5 by time of day (September, per sensor)",
    x = "Hour of day", y = "PM2.5 (µg/m³)", color = "Sensor"
  ) +
  theme_minimal(base_size = 12)
ggsave("PurpleAir figs/pm2.5_byhour_sept.png", plot = plot, width = 8, height = 6, units = "in")

plotjan<-ggplot(palocs_jan, aes(hour + minute(datetime)/60, pm2.5_corrected, color = Purple.Air.Name)) + geom_smooth(se = FALSE, method = "loess", span = 0.3, size = 0.8) +
  scale_x_continuous(breaks = 0:23, limits = c(0, 24)) +
  labs(
    title = "Smoothed PM2.5 by time of day (January, per sensor)",
    x = "Hour of day", y = "PM2.5 (µg/m³)", color = "Sensor"
  ) +
  theme_minimal(base_size = 12)
ggsave("PurpleAir figs/pm2.5_byhour_jan.png", plot = plotjan, width = 8, height = 6, units = "in")


#averages (rather than model estimates)

pmavg<-aggregate(pa$pm2.5_corrected, by=list(pa$date,pa$sensor_index), mean, na.rm=TRUE)
colnames(pmavg)<-c("date","sensor_index","pm2.5_avg")
pmavg$month<-substr(pmavg$date,1,7)
pmavg_sept<-pmavg[pmavg$month=="2025-09",]
pmavg_septbysensor<-aggregate(pmavg_sept$pm2.5_avg, by=list(pmavg_sept$sensor_index), mean, na.rm=TRUE)
colnames(pmavg_septbysensor)<-c("sensor_index","septpm2.5_avg")

pmavg_jan<-pmavg[pmavg$month=="2026-01",]
pmavg_janbysensor<-aggregate(pmavg_jan$pm2.5_avg, by=list(pmavg_jan$sensor_index), mean, na.rm=TRUE)
colnames(pmavg_janbysensor)<-c("sensor_index","janpm2.5_avg")

#month model
pa$month<-as.factor(pa$month)
monmmod<-lmer(pm2.5_corrected~-1+month + (1|sensor_index),data=pa)
Anova(monmmod)
fixef(monmmod)
summary(monmmod)


monplot<-ggplot(pa, aes(factor(month), pm2.5_corrected)) +
#  geom_violin(fill = "#f1c40f", color = NA, alpha = 0.5) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.3) +
  labs(
    title = "PM2.5 distributions month (across all sensors)",
    x = "Month", y = "PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 12)


nique(worst10$sensor)
[1] "GRIT03"                         "GRIT25"                        
[3] "GRIT12"                         "GRIT19"                        
[5] "GRIT22"                         "Manitou Park Elementary School"
[7] "GRIT15"                         "GRIT10"                        
[9] "GRIT13"                        
> best10<-tail(dailyavg_hrsabove25[order(dailyavg_hrsabove25$pm2.5_avg_day, decreasing=TRUE),], n=10)#highest/worst values= 61.71485 58.41759 57.16004 39.46606 38.01056 36.50640
> unique(best10$sensor)
[1] "GRIT20" "GRIT13" "GRIT15" "GRIT24" "GRIT19" "GRIT17" "GRIT34" "GRIT35" "GRIT07"

