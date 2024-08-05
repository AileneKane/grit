#######################################################
### Script to look at GRIT purpleair air quality data ###
################# Started June 20, 2024 #####################
################ ailene.ettinger@tnc.org ##############
#######################################################

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
install.packages("patchwork")
install.packages("gridExtra")

# load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(plotly)
library(gridExtra)
library(scales)


# set working directory
setwd("documents/GitHub/grit/analyses") 

#Read in data file with PurpleAir logger locations
#(notyet created)
#Read in air quality data from 2 sites
GRIT01<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/GRIT01 2024-07-03 2024-07-05.csv", header=TRUE) 
GRIT02<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/GRIT02 2024-07-03 2024-07-05.csv", header=TRUE) 
GRIT03<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/GRIT03 2024-07-03 2024-07-05.csv", header=TRUE) 
GRIT04<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/GRIT04 2024-07-03 2024-07-05.csv", header=TRUE) 

GRIT01$Purple.Air.Name<- "GRIT01"
GRIT02$Purple.Air.Name<-"GRIT02"
GRIT03$Purple.Air.Name<-"GRIT03"
GRIT04$Purple.Air.Name<-"GRIT04"


#look at the data
head(GRIT01)
summary(GRIT02)
dim(GRIT02)
#understand the data a bit more- what are these 2 columns- max and min?
range(GRIT01$pm2.5_atm_a, na.rm=TRUE) #_atm vs _cf_1, atm is for outdoor sensors, cf_1 is for indoor
range(GRIT01$pm2.5_atm_b, na.rm=TRUE)
mean(c(GRIT01$pm2.5_atm_b,GRIT01$pm2.5_atm_b, na.rm=TRUE),na.rm=TRUE)

#Separating sensors by date, cleaning data

GRIT01_July03 <- GRIT01 %>% slice(1:41) #selecting July 3rd only
time <- c(
    "03:30", "04:00", "04:30", "05:00", "05:30", 
            "06:00", "06:30", "07:00", "07:30", "08:00", "08:30", 
            "09:00", "09:30", "10:00", "10:30", "11:00", "11:30", 
            "12:00", "12:30", "13:00", "13:30", "14:00", "14:30", 
            "15:00", "15:30", "16:00", "16:30", "17:00", "17:30", 
            "18:00", "18:30", "19:00", "19:30", "20:00", "20:30", 
            "21:00", "21:30", "22:00", "22:30", "23:00", "23:30")
GRIT01_July03$time_stamp <-time # change time_stamp values so it's easier to read
GRIT01_July03 <- GRIT01_July03 %>%
  rename(date_time = time_stamp) #renamed date_time because R has built in argument with time_stamp and I was getting errors
view(GRIT01_July03)
GRIT01_July03$date_time <- factor(GRIT01_July03$date_time, levels = GRIT01_July03$date_time)
# day <- rep(c("July 3rd"), each=41)
GRIT01_July03 <- GRIT01_July03 %>% mutate (day = "July 3rd")
GRIT01_July04 <- GRIT01_July04 %>% mutate (day = "July 4th")


GRIT02_July03 <- GRIT02 %>% slice(1:48)
additional_times <- c("00:00", "00:30", "01:00", "01:30", "02:00", "02:30", "03:00")
all_times <- c(additional_times, time)
GRIT02_July03$time_stamp <-all_times
GRIT02_July03 <- GRIT02_July03 %>%
  rename(date_time = time_stamp) 
GRIT02_July03$date_time <- factor(GRIT02_July03$date_time, levels = GRIT02_July03$date_time)

GRIT03_July03 <- GRIT03 %>% slice(1:48)
GRIT03_July03$time_stamp <-all_times
GRIT03_July03 <- GRIT03_July03 %>%
  rename(date_time = time_stamp) 
GRIT03_July03$date_time <- factor(GRIT03_July03$date_time, levels = GRIT03_July03$date_time)

GRIT04_July03 <- GRIT04 %>% slice(1:48)
GRIT04_July03$time_stamp <-all_times
GRIT04_July03 <- GRIT04_July03 %>%
  rename(date_time = time_stamp) 
GRIT04_July03$date_time <- factor(GRIT04_July03$date_time, levels = GRIT04_July03$date_time)

#July 4th

GRIT01_July04 <- GRIT01 %>% slice (42:89)
time2 <- c(
  "00:00", "00:30", "01:00", "01:30", "02:00", "02:30", 
  "03:00", "03:30", "04:00", "04:30", "05:00", "05:30", 
  "06:00", "06:30", "07:00", "07:30", "08:00", "08:30", 
  "09:00", "09:30", "10:00", "10:30", "11:00", "11:30", 
  "12:00", "12:30", "13:00", "13:30", "14:00", "14:30", 
  "15:00", "15:30", "16:00", "16:30", "17:00", "17:30", 
  "18:00", "18:30", "19:00", "19:30", "20:00", "20:30", 
  "21:00", "21:30", "22:00", "22:30", "23:00", "23:30")
GRIT01_July04$time_stamp <-time2
GRIT01_July04 <- GRIT01_July04 %>%
  rename(date_time = time_stamp)
view(GRIT01_July04)
GRIT01_July04$date_time <- factor(GRIT01_July04$date_time, levels = GRIT01_July04$date_time)

GRIT02_July04 <- GRIT02 %>% slice(49:96)
GRIT02_July04$time_stamp <-time2
GRIT02_July04 <- GRIT02_July04 %>%
  rename(date_time = time_stamp)
GRIT02_July04$date_time <- factor(GRIT02_July04$date_time, levels = GRIT02_July04$date_time)

GRIT03_July04 <- GRIT03 %>% slice(49:96)
GRIT03_July04$time_stamp <-time2
GRIT03_July04 <- GRIT03_July04 %>%
  rename(date_time = time_stamp)
GRIT03_July04$date_time <- factor(GRIT03_July04$date_time, levels = GRIT03_July04$date_time)

GRIT04_July04 <- GRIT04 %>% slice(49:96)
GRIT04_July04$time_stamp <-time2
GRIT04_July04 <- GRIT04_July04 %>%
  rename(date_time = time_stamp)
GRIT04_July04$date_time <- factor(GRIT04_July04$date_time, levels = GRIT04_July04$date_time)

####Graphs####

# GRIT01 pm2.5_atm on July 3rd 
p1 <- 
  ggplot(data = GRIT01_July03,
          aes(date_time, pm2.5_atm, group = 1)) +
          geom_line(color = "darkgreen") +
          geom_point(color = "darkgreen")+
          ggtitle("GRIT01 Particulate Matter 2.5 Concentrations on July 3rd, 2024")+
          labs( x = "Time",  y = "PM 2.5 Concentrations (µg/m3)" ) + 
          theme(axis.text.x = element_text (angle = 90, vjust = -0.01),
                plot.title = element_text(face='bold'))
print(p1)
#GRIT01 Temp and PM 2.5 on July 3rd
p2 <-ggplot(data = GRIT01_July03,
          aes(date_time, temperature, group =1)) +
          geom_line()+
          ggtitle("GRIT01 Temperature Inside of Sensor July 3rd 2024")+
          labs(x = "Time", 
            y = "Temperature Inside of Sensor (F)") + #Temp readings on .csv reflect average temp of two channels within sensor housing, 8F higher than ambient temp
          theme(axis.text.x = element_text (angle = 90, vjust = -0.01),
                plot.title = element_text(face='bold'))

p3 <- ggplot(data = GRIT01_July03,
             aes(date_time, pm2.5_atm, group = 1)) +
  geom_point(color = "darkgreen")+
  geom_line(color = "darkgreen")+
  labs(x = "Time", 
       y = "PM 2.5 Concentrations (µg/m3)" ) + 
  theme(axis.text.x = element_text (angle = 90, vjust = -0.01))
print(p2 + p3)

#GRIT01 July3rd Temp & PM2.5 Readings graphed together
coeff <- 0.1
p4 <- 
ggplot (GRIT01_July03, aes (x=date_time))+
  geom_line ( aes(y = temperature, group = 1, color= "Temperature")) +
  geom_line ( aes(y = pm2.5_atm / coeff, group = 1, color = "PM 2.5 Concentration")) +
  scale_y_continuous(name = "Temperature Inside of Sensor (F)",
                     sec.axis = sec_axis(~.*coeff, name =  "PM 2.5 Concentrations (µg/m3)" ))+
  ggtitle("GRIT01 Sensor Temperature (F) and PM 2.5 (µg/m3) Concentrations on July 3rd")+
  labs( x = "Time", color = "Indicator")+
  theme(axis.text.x = element_text (angle = 90, vjust = -0.01),
        plot.title = element_text(face='bold'))+
  scale_color_manual(values=c('darkgreen', 'blue'))
print(p4)
print(p4 + p6)
  
#GRIT01 channel a vs. channel b pm2.5_atm conc. on July 3rd
p5 <-   ggplot() + 
  geom_point(data = GRIT01_July03,
             mapping = aes(date_time, pm2.5_atm_a, group = 1, color= "A")) +
  geom_line(data = GRIT01_July03, 
            mapping = aes(date_time, pm2.5_atm_a, group = 1, color = "A"))+
  geom_point(data = GRIT01_July03, 
             mapping =  aes(date_time, pm2.5_atm_b, group = 1, color = "B"))+
  geom_line(data = GRIT01_July03, 
            mapping =  aes(date_time, pm2.5_atm_b, group = 1,color = "B"))+
  ggtitle("GRIT01 Channel A vs. Channel B PM 2.5 Concentration Readings on July 3rd")+
  labs(x = "Time",
       y = "PM 2.5 Concentrations (µg/m3)",
       color = "Channel")+
  theme(
    axis.text.x = element_text (angle = 90, vjust = -0.01),
    plot.title = element_text(face='bold'))+
  scale_color_manual(values=c('darkgreen', 'blue'))
  
print(p5)
  

#GRIT01 pm2.5_atm on July 4th
GRIT_July04_p <- GRIT01_July04 %>% slice(8:48)

GRIT01_July04_Plot <- 
  ggplot( data = GRIT_July04_p,
          mapping = aes(x = date_time, y = pm2.5_atm, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue")+
  ggtitle("GRIT01 Particulate Matter 2.5 Concentrations on July 4th, 2024")+
  labs(x = "Time", y = "PM 2.5 Concentrations (µg/m3)" ) +
      theme(axis.text.x = element_text (angle = 90, vjust = -0.01),
            plot.title = element_text(face = 'bold'))


#July 4th vs July 3rd pm2.5_atm 

x1 = ggplot() + 
    geom_line(data = GRIT01_July04,  aes(x = date_time, y = pm2.5_atm, group = 1, color = "July 4th")) +
    geom_line (data = GRIT01_July03, aes(date_time, pm2.5_atm, group = 1, color = "July 3rd")) +
    theme(axis.text.x = element_text (angle = 90, vjust = -0.01))+
    facet_wrap(~)
    ggtitle("GRIT01 PM 2.5 July 3rd vs. July 4th")+
    labs(x = "Time", y = "PM 2.5 Concentrations (µg/m3)",
        color = "Day") +
  scale_color_manual(values=c('darkgreen', 'blue'))+
  theme(plot.title = element_text(face ='bold'))
print(x1)

x2 = ggplot() + 
  geom_line(data = GRIT02_July04,  aes(x = date_time, y = pm2.5_atm, group = 1, color = "July 4th")) +
  geom_line (data = GRIT02_July03, aes(date_time, pm2.5_atm, group = 1, color = "July 3rd")) +
  theme(axis.text.x = element_text (angle = 90, vjust = -0.01))+
  ggtitle("GRIT02 PM 2.5 July 3rd vs. July 4th")+
  labs(x = "Time", y = "PM 2.5 Concentrations (µg/m3)",
       color = "Day") +
  scale_color_manual(values=c('darkgreen', 'blue'))+
  theme(plot.title = element_text(face ='bold'))
print(x2)

x3 = ggplot() + 
  geom_line(data = GRIT03_July04,  aes(x = date_time, y = pm2.5_atm, group = 1, color = "July 4th")) +
  geom_line (data = GRIT03_July03, aes(date_time, pm2.5_atm, group = 1, color = "July 3rd")) +
  theme(axis.text.x = element_text (angle = 90, vjust = -0.01))+
  ggtitle("GRIT03 PM 2.5 July 3rd vs. July 4th")+
  labs(x = "Time", y = "PM 2.5 Concentrations (µg/m3)",
       color = "Day") +
  scale_color_manual(values=c('darkgreen', 'blue'))+
  theme(plot.title = element_text(face ='bold'))
print(x3)

x4 = ggplot() + 
  geom_line(data = GRIT04_July04,  aes(x = date_time, y = pm2.5_atm, group = 1, color = "July 4th")) +
  geom_line (data = GRIT04_July03, aes(date_time, pm2.5_atm, group = 1, color = "July 3rd")) +
  theme(axis.text.x = element_text (angle = 90, vjust = -0.01))+
  ggtitle("GRIT04 PM 2.5 July 3rd vs. July 4th")+
  labs(x = "Time", y = "PM 2.5 Concentrations (µg/m3)",
       color = "Day") +
  scale_color_manual(values=c('darkgreen', 'blue'))+
  theme(plot.title = element_text(face ='bold'))
print(x4)

x1 + x2 + x3 + x4

#GRIT01 July4th Temp & PM2.5 Readings graphed together
coeff <- 0.1
p6 <-  ggplot (GRIT01_July04, aes (x=date_time))+
  geom_line ( aes(y = temperature, group = 1, color= "Temperature")) +
  geom_line ( aes(y = pm2.5_atm / coeff, group = 1, color = "PM 2.5 Concentration")) +
  scale_y_continuous(name = "Temperature Inside of Sensor (F)",
                     sec.axis = sec_axis(~.*coeff, name =  "PM 2.5 Concentrations (µg/m3)" ))+
  ggtitle("GRIT01 Sensor Temperature (F) and PM 2.5 (µg/m3) Concentrations on July 4th")+
  labs( x = "Time", color = "Indicator")+
  theme(axis.text.x = element_text (angle = 90, vjust = -0.01),
        plot.title = element_text(face='bold'))+
  scale_color_manual(values=c('darkgreen', 'blue'))
print(p6)

#GRIT01 channel a vs. channel b pm2.5_atm conc. on July 4th
p7 <-   
  ggplot() + 
  geom_point(data = GRIT01_July04,
             mapping = aes(date_time, pm2.5_atm_a, group = 1, color= "A")) +
  geom_line(data = GRIT01_July04, 
            mapping = aes(date_time, pm2.5_atm_a, group = 1, color = "A"))+
  geom_point(data = GRIT01_July04, 
             mapping =  aes(date_time, pm2.5_atm_b, group = 1, color = "B"))+
  geom_line(data = GRIT01_July04, 
            mapping =  aes(date_time, pm2.5_atm_b, group = 1,color = "B"))+
  ggtitle("GRIT01 Channel A vs. Channel B PM 2.5 Concentration Readings on July 4th")+
  labs(x = "Time",
       y = "PM 2.5 Concentrations (µg/m3)",
       color = "Channel")+
  theme(
    axis.text.x = element_text (angle = 90, vjust = -0.01),
    plot.title = element_text(face='bold'))+
  scale_color_manual(values=c('darkgreen', 'blue'))

print(p7)

#viewing July 3rd and July 4th graphs side by side
grid.arrange(p4, p6, nrow = 2)

#Merging all GRIT
GRIT01_July03 <- GRIT01_July03[,-which(colnames(GRIT01_July03)=="day")]
all_GRITaq <- rbind(GRIT01_July03, GRIT01_July04,GRIT02_July03, GRIT02_July04, GRIT03_July03, GRIT03_July04, GRIT04_July03, GRIT04_July04)
all_GRITaq_July03 <- rbind(GRIT01_July03,GRIT02_July03, GRIT03_July03,GRIT04_July03)
all_GRITq_July04<- rbind(GRIT01_July04, GRIT02_July04, GRIT03_July04, GRIT04_July04)

matched <- intersect(colnames(GRIT01_July03),colnames(GRIT02_July03))
all <- union(colnames(GRIT01_July03), colnames(GRIT02_July03))
non.matched <- all[!all %in% matched]
canopy_cover <- read.csv("output/grit_aq_lc.csv", header=TRUE)

canopy_cover$cancov.10m <- round(canopy_cover$cancov.10m, digits = 4)
canopy_cover$cancov.20m <- round(canopy_cover$cancov.20m, digits = 4)
canopy_cover$cancov.30m <- round(canopy_cover$cancov.30m, digits = 4)
canopy_cover$cancov.40m <- round(canopy_cover$cancov.40m, digits = 4)
canopy_cover$cancov.50m <- round(canopy_cover$cancov.50m, digits = 4)
canopy_cover$cancov.100m <- round(canopy_cover$cancov.100m, digits = 4)
canopy_cover$cancov.200m <- round(canopy_cover$cancov.200m, digits = 4)
canopy_cover$cancov.400m <- round(canopy_cover$cancov.400m, digits = 4)
canopy_cover$cancov.800m <- round(canopy_cover$cancov.800m, digits = 4)


all_cc<-left_join(all_GRITaq, canopy_cover, by= "Purple.Air.Name", copy = TRUE)

all_cc_July03<- left_join(all_GRITaq_July03, canopy_cover, by = "Purple.Air.Name", copy=TRUE)
all_cc_July04 <- left_join(all_GRITq_July04, canopy_cover, by = "Purple.Air.Name", copy=TRUE)

all_cc_combined <- bind_rows(
  all_cc_July03 %>% mutate(date = "July 3rd"),
  all_cc_July04 %>% mutate(date = "July 4th"))
all_cc_combined_2 <- bind_rows(all_cc_July03 %>% mutate (Pu))

#All GRIT Graphs 
#Box plots comparing July 3rd vs July 4th PM 2.5 Concentrations
label_10 <- all_cc_combined %>% group_by(Purple.Air.Name,date,cancov.10m) %>% summarise(pm2.5_atm = max(pm2.5_atm) + 1) %>% ungroup()
label_20<- all_cc_combined %>% group_by(Purple.Air.Name,date,cancov.20m) %>% summarise(pm2.5_atm = max(pm2.5_atm) + 1) %>% ungroup()
label_30<- all_cc_combined %>% group_by(Purple.Air.Name,date,cancov.30m) %>% summarise(pm2.5_atm = max(pm2.5_atm) + 1) %>% ungroup()
label_40<- all_cc_combined %>% group_by(Purple.Air.Name,date,cancov.40m) %>% summarise(pm2.5_atm = max(pm2.5_atm) + 1) %>% ungroup()
label_50<- all_cc_combined %>%group_by(Purple.Air.Name,date,cancov.50m) %>%  summarise(pm2.5_atm = max(pm2.5_atm) + 1) %>% ungroup()
label_100<- all_cc_combined %>% group_by(Purple.Air.Name,date,cancov.100m) %>% summarise(pm2.5_atm = max(pm2.5_atm) + 1) %>% ungroup()
label_200<- all_cc_combined %>% group_by(Purple.Air.Name,date,cancov.200m) %>% summarise(pm2.5_atm = max(pm2.5_atm) + 1) %>% ungroup()
label_400<- all_cc_combined %>% group_by(Purple.Air.Name,date,cancov.400m) %>% summarise(pm2.5_atm = max(pm2.5_atm) + 1) %>% ungroup()
label_800<- all_cc_combined %>% group_by(Purple.Air.Name,date,cancov.800m) %>% summarise(pm2.5_atm = max(pm2.5_atm) + 1) %>% ungroup()

box_cancov10 <- 
  ggplot(all_cc_combined, aes(x = factor(cancov.10m), y = pm2.5_atm, fill = date)) + 
  geom_boxplot()+
  geom_text(data = label_10, aes(label = Purple.Air.Name, y = pm2.5_atm), position = position_dodge(width = 0.75), vjust = -0.1, size = 3) +
  scale_y_continuous(limits = c(0, 20))+
  scale_fill_manual(values = c("July 3rd" = "lightgoldenrod", "July 4th" = "darkolivegreen3"))+
  ggtitle("All GRIT Sensors: Canopy Cover within 10m and PM 2.5 Concentration", 
          subtitle = "July 3rd vs July 4th")+
  labs(x= "Canopy Cover 10 (m)", y  = "PM 2.5 Concentration (µg/m3)", fill = "Date")+
  theme(plot.title = element_text(face ="bold"))
box_cancov10 + facet_wrap(vars(Purple.Air.Name))

box_cancov20<-
  ggplot(all_cc_combined, aes(x = factor(cancov.20m), y = pm2.5_atm, fill = date)) + 
  geom_boxplot()+
  geom_text(data = label_20, aes(label = Purple.Air.Name, y = pm2.5_atm), position = position_dodge(width = 0.75), vjust = -0.1, size = 3) +
  scale_y_continuous(limits = c(0, 20))+
  scale_fill_manual(values = c("July 3rd" = "lightgoldenrod", "July 4th" = "darkolivegreen3"))+
  ggtitle("All GRIT Sensors: Canopy Cover within 20m and PM 2.5 Concentration", 
          subtitle = "July 3rd vs July 4th")+
  labs(x= "Canopy Cover 20 (m)", y  = "PM 2.5 Concentration (µg/m3)", fill = "Date")+
  theme(plot.title = element_text(face ="bold"))
box_cancov20 + facet_wrap(vars(Purple.Air.Name)) #facet wrap option remove labels from graph, delete geom_text()

box_cancov30<-
  ggplot(all_cc_combined, aes(x = factor(cancov.30m), y = pm2.5_atm, fill = date)) + 
  geom_boxplot()+
  geom_text(data = label_30, aes(label = Purple.Air.Name, y = pm2.5_atm), position = position_dodge(width = 0.75), vjust = -0.1, size = 3) +
  scale_y_continuous(limits = c(0, 20))+
  scale_fill_manual(values = c("July 3rd" = "lightgoldenrod", "July 4th" = "darkolivegreen3"))+
  ggtitle("All GRIT Sensors: Canopy Cover within 30m and PM 2.5 Concentration", 
          subtitle = "July 3rd vs July 4th")+
  labs(x= "Canopy Cover 20 (m)", y  = "PM 2.5 Concentration (µg/m3)", fill = "Date")+
  theme(plot.title = element_text(face ="bold"))
#repeat for rest of canopy cover buffers

#Line graph representing each sensors x = time, y = AQ 

#######################################################
### Script to look at Non-GRIT purpleair air quality data  ###
#######################################################

#read CSV Low Tree Canopy###
LOWTC01<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/Low Tree Canopy/Baker Middle School  2024-07-03 2024-07-05.csv", header=TRUE)
LOWTC02<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/Low Tree Canopy/PSU Star Lab South Tacoma 2024-07-03 2024-07-05 .csv", header=TRUE) 
LOWTC03<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/Low Tree Canopy/Morgan Family YMCA 2024-07-03 2024-07-05 .csv", header=TRUE) 
LOWTC04<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/Low Tree Canopy/Boze Elementary School 2024-07-03 2024-07-05 .csv", header=TRUE) 
LOWTC05<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/Low Tree Canopy/6th Ave and Sprague-ish 2024-07-03 2024-07-05 .csv", header=TRUE) 
LOWTC06<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/Low Tree Canopy/Rustin 2024-07-03 2024-07-05 .csv", header=TRUE) 
LOWTC07<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/Low Tree Canopy/Whitman Elementary School 2024-07-03 2024-07-05.csv", header=TRUE) 
LOWTC08<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/Low Tree Canopy/N Tacoma N 9th and Stevens 2024-07-03 2024-07-05 .csv", header=TRUE) 
LOWTC09<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/Low Tree Canopy/Tacoma Alexander 2024-07-03 2024-07-05 .csv", header=TRUE) 

#read CSV High Tree Canopy#
HIGHTC01 <-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/High Tree Canopy/PT Woodworth 2024-07-03 2024-07-05.csv", header=TRUE)
HIGHTC02<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/High Tree Canopy/Manitou Park Elementary 2024-07-03 2024-07-05.csv", header=TRUE) 
HIGHTC03<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/High Tree Canopy/Owl's Roost 2024-07-03 2024-07-05.csv", header=TRUE)
HIGHTC04<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/High Tree Canopy/PTOI 2024-07-03 2024-07-05.csv", header=TRUE)
HIGHTC05<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/High Tree Canopy/Central Tacoma 19th and Mullen 2024-07-03 2024-07-05.csv",header=TRUE)
HIGHTC06<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/High Tree Canopy/1 Broadway 2024-07-03 2024-07-05.csv",header=TRUE)
HIGHTC07<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/High Tree Canopy/Pointe Woodworth 2024-07-03 2024-07-05.csv", header=TRUE)
HIGHTC08<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/High Tree Canopy/Norpoint 2024-07-03 2024-07-05.csv",header=TRUE)
HIGHTC09<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/High Tree Canopy/Millers Skyline Terrace 2024-07-03 2024-07-05.csv",header=TRUE)
HIGHTC010<-read.csv("~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Map Data (Non GRIT)/High Tree Canopy/PSU Star Labs Titlow 2024-07-03 2024-07-05.csv",header=TRUE)


#changing column name#
time <- c(
  "00:00", "00:30", "01:00", "01:30", "02:00", "02:30", 
  "03:00", "03:30", "04:00", "04:30", "05:00", "05:30", 
  "06:00", "06:30", "07:00", "07:30", "08:00", "08:30", 
  "09:00", "09:30", "10:00", "10:30", "11:00", "11:30", 
  "12:00", "12:30", "13:00", "13:30", "14:00", "14:30", 
  "15:00", "15:30", "16:00", "16:30", "17:00", "17:30", 
  "18:00", "18:30", "19:00", "19:30", "20:00", "20:30", 
  "21:00", "21:30", "22:00", "22:30", "23:00", "23:30")


LOWTC01$time_stamp <-time 
LOWTC01 <- LOWTC01 %>%
  rename(date_time = time_stamp)
LOWTC02$time_stamp <-time 
LOWTC02 <- LOWTC02 %>%
  rename(date_time = time_stamp)
LOWTC03$time_stamp <-time 
LOWTC03 <- LOWTC03 %>%
  rename(date_time = time_stamp)
LOWTC04$time_stamp <-time 
LOWTC04 <- LOWTC04 %>%
  rename(date_time = time_stamp)
LOWTC05$time_stamp <-time 
LOWTC05 <- LOWTC05 %>%
  rename(date_time = time_stamp)
LOWTC06$time_stamp <-time 
LOWTC06 <- LOWTC06 %>%
  rename(date_time = time_stamp)
LOWTC07$time_stamp <-time 
LOWTC07 <- LOWTC07 %>%
  rename(date_time = time_stamp)
LOWTC08$time_stamp <-time 
LOWTC08 <- LOWTC08 %>%
  rename(date_time = time_stamp)
LOWTC09$time_stamp <-time 
LOWTC09 <- LOWTC09 %>%
  rename(date_time = time_stamp)

HIGHTC01$time_stamp <-time 
HIGHTC01 <- HIGHTC01 %>%
  rename(date_time = time_stamp)
HIGHTC02$time_stamp <-time 
HIGHTC02 <- HIGHTC02 %>%
  rename(date_time = time_stamp)
HIGHTC03$time_stamp <-time 
HIGHTC03 <- HIGHTC03 %>%
  rename(date_time = time_stamp)
HIGHTC04$time_stamp <-time 
HIGHTC04 <- HIGHTC04 %>%
  rename(date_time = time_stamp)
HIGHTC05$time_stamp <-time 
HIGHTC05 <- HIGHTC05 %>%
  rename(date_time = time_stamp)
HIGHTC06$time_stamp <-time 
HIGHTC06 <- HIGHTC06 %>%
  rename(date_time = time_stamp)
HIGHTC07$time_stamp <-time 
HIGHTC07 <- HIGHTC07 %>%
  rename(date_time = time_stamp)
HIGHTC08$time_stamp <-time 
HIGHTC08 <- HIGHTC08 %>%
  rename(date_time = time_stamp)
HIGHTC09$time_stamp <-time 
HIGHTC09 <- HIGHTC09 %>%
  rename(date_time = time_stamp)

#Low TC PM2.5 Concentrations on July 4th
y1 <- ggplot() + 
  geom_line(data = LOWTC01, mapping = aes(date_time, pm2.5_atm, group = 1, color ="Baker"))+
  geom_line (data = LOWTC02, mapping = aes(date_time, pm2.5_atm, group = 1, color = "PSU Star"))+
  geom_line (data = LOWTC03, mapping = aes(date_time, pm2.5_atm, group = 1, color = "Morgan"))+
  geom_line (data = LOWTC04, mapping = aes(date_time, pm2.5_atm, group = 1, color = "Boze"))+
  geom_line (data = LOWTC05, mapping = aes(date_time, pm2.5_atm, group = 1, color = "6th"))+
  geom_line (data = LOWTC06, mapping = aes(date_time, pm2.5_atm, group = 1, color = "Rustin"))+
  geom_line (data = LOWTC07, mapping = aes(date_time, pm2.5_atm, group = 1, color = "Whitman"))+
  geom_line (data = LOWTC08, mapping = aes(date_time, pm2.5_atm, group = 1, color = "NTacoma"))+
  geom_line (data = LOWTC09, mapping = aes(date_time, pm2.5_atm, group = 1, color = "TacomaAlexander"))+
  ggtitle("Low Tree Canopy PurpleAir PM 2.5 Concentrations on July 4th, 2024")+ 
  labs(x = "Time",
       y = "PM 2.5 Concentrations (µg/m3)", color = "PurpleAir Name")+
  theme(
    axis.text.x = element_text (angle = 90, vjust = -0.01),
    plot.title = element_text(face='bold'))
  
#High TC PM2.5 Concentrations on July 4th
y2 <- ggplot() +
  geom_line(data = HIGHTC01, mapping = aes(date_time, pm2.5_atm, group = 1, color ="PT Woodworth"))+
  geom_line (data = HIGHTC02, mapping = aes(date_time, pm2.5_atm, group = 1, color = "Manitou"))+
  geom_line (data = HIGHTC03, mapping = aes(date_time, pm2.5_atm, group = 1, color = "Owl"))+
  geom_line (data = HIGHTC04, mapping = aes(date_time, pm2.5_atm, group = 1, color = "PTOI"))+
  geom_line (data = HIGHTC05, mapping = aes(date_time, pm2.5_atm, group = 1, color = "Central Tacoma"))+
  geom_line (data = HIGHTC06, mapping = aes(date_time, pm2.5_atm, group = 1, color = "1 Broadway"))+
  geom_line (data = HIGHTC07, mapping = aes(date_time, pm2.5_atm, group = 1, color = "Pointe Woodworth"))+
  geom_line (data = HIGHTC08, mapping = aes(date_time, pm2.5_atm, group = 1, color = "Norpoint"))+
  geom_line (data =HIGHTC09, mapping = aes(date_time, pm2.5_atm, group = 1, color = "Miller"))+
  ggtitle("High Tree Canopy PurpleAir PM 2.5 Concentrations on July 4th, 2024")+ 
  labs(x = "Time",
       y = "PM 2.5 Concentrations (µg/m3)", color = "PurpleAir Name")+
  theme(
    axis.text.x = element_text (angle = 90, vjust = -0.01),
    plot.title = element_text(face='bold'))
  

y3 <- ggplot() +
  geom_line(data = GRIT01_July04, mapping = aes(date_time, pm2.5_atm, group = 1, color ="GRIT 01"))+
  geom_line (data = GRIT02_July04, mapping = aes(date_time, pm2.5_atm, group = 1, color = "GRIT 02"))+
  geom_line (data = GRIT03_July04, mapping = aes(date_time, pm2.5_atm, group = 1, color = "GRIT 03"))+
  geom_line (data = GRIT04_July04, mapping = aes(date_time, pm2.5_atm, group = 1, color = "GRIT04"))+
  ggtitle("GRIT PM 2.5 Concentrations on July 4th, 2024")+ 
  labs(x = "Time",
       y = "PM 2.5 Concentrations (µg/m3)", color = "PurpleAir Name")+
  theme(
    axis.text.x = element_text (angle = 90, vjust = -0.01),
    plot.title = element_text(face='bold'))
y1 + y2
  
  
  
  
 