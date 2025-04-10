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
setwd(setwd("documents/GitHub/grit/") 

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

#combined all_cc with separate date column to indicate July 3rd or July4th
all_cc_combined <- bind_rows(
  all_cc_July03 %>% mutate(date = "July 3rd"),
  all_cc_July04 %>% mutate(date = "July 4th")) 

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

#EPA PM2.5 Standards 
PM2.5_EPA_short <- data.frame(yintercept=35, Lines='24 hour') #primary 24-hour PM2.5 standard
PM2.5_EPA_annual <- data.frame(yintercept = 9, Lines = 'Annual') # long-term standard (annual average)

box_cancov10 <- 
  ggplot(all_cc_combined, aes(x = factor(cancov.10m), y = pm2.5_atm, fill = date)) + 
  geom_boxplot()+
  #geom_text(data = label_10, aes(label = Purple.Air.Name, y = pm2.5_atm), position = position_dodge(width = 0.75), vjust = -0.1, size = 3) +
  scale_y_continuous(limits = c(0, 40))+
  ggtitle("All GRIT Sensors: Tree Canopy Cover (10 Meters) and PM 2.5 Concentration", 
          subtitle = "July 3rd vs July 4th")+
  labs(x= "Proportion of Tree Canopy Cover within 10 Meters", y  = "PM 2.5 Concentration (µg/m3)", fill = "Date",
      color = "EPA PM 2.5 standard", linetype = "EPA PM 2.5 Standards")+
  theme_bw()+
  geom_hline(data = PM2.5_EPA_short, aes(yintercept = yintercept, color = "Short-term Standard", linetype = "Short-term Standard"), linetype = "dashed") +
 # geom_hline(data = PM2.5_EPA_annual, aes(yintercept = yintercept, color = "Annual Standard", linetype = "Annual Standard"), linetype = "dashed") +
  scale_color_manual(values = c("Short-term Standard" = "red"),
                     labels = c("Short-term Standard" = "Short-Term")) + # EPA primary 24-hour PM2.5 standard at the level of 35 µg/m3. 
  scale_fill_manual(values = c("July 3rd" = "lightgoldenrod", "July 4th" = "deepskyblue"))+
  theme(plot.title = element_text(face ="bold"))

 box_cancov10 + labs(
   colour = "name1",
   shape = "name2"
 )

box_cancov10 + facet_wrap(vars(Purple.Air.Name))

box_cancov20<-
  ggplot(all_cc_combined, aes(x = factor(cancov.20m), y = pm2.5_atm, fill = date)) + 
  geom_boxplot()+
  #geom_text(data = label_10, aes(label = Purple.Air.Name, y = pm2.5_atm), position = position_dodge(width = 0.75), vjust = -0.1, size = 3) +
  scale_y_continuous(limits = c(0, 40))+
  ggtitle("All GRIT Sensors: Tree Canopy Cover (20 Meters) and PM 2.5 Concentration ", 
          subtitle = "July 3rd vs July 4th")+
  labs(x= "Proportion of Tree Canopy Cover within 20 Meters", y  = "PM 2.5 Concentration (µg/m3)", fill = "Date",
       color = "EPA PM 2.5 standard", linetype = "EPA PM 2.5 Standards")+
  theme_bw()+
  geom_hline(data = PM2.5_EPA_short, aes(yintercept = yintercept, color = "Short-term Standard", linetype = "Short-term Standard"), linetype = "dashed") +
  # geom_hline(data = PM2.5_EPA_annual, aes(yintercept = yintercept, color = "Annual Standard", linetype = "Annual Standard"), linetype = "dashed") +
  scale_color_manual(values = c("Short-term Standard" = "red"),
                     labels = c("Short-term Standard" = "Short-Term")) + # EPA primary 24-hour PM2.5 standard at the level of 35 µg/m3. 
  scale_fill_manual(values = c("July 3rd" = "lightgoldenrod", "July 4th" = "deepskyblue"))+
  theme(plot.title = element_text(face ="bold"))


box_cancov50<-
  ggplot(all_cc_combined, aes(x = factor(cancov.50m), y = pm2.5_atm, fill = date)) + 
  geom_boxplot()+
  #geom_text(data = label_10, aes(label = Purple.Air.Name, y = pm2.5_atm), position = position_dodge(width = 0.75), vjust = -0.1, size = 3) +
  scale_y_continuous(limits = c(0, 40))+
  ggtitle("All GRIT Sensors: Tree Canopy Cover (50 Meters) and PM 2.5 Concentration ", 
          subtitle = "July 3rd vs July 4th")+
  labs(x= "Proportion of Tree Canopy Cover within 50 Meters", y  = "PM 2.5 Concentration (µg/m3)", fill = "Date",
       color = "EPA PM 2.5 standard", linetype = "EPA PM 2.5 Standards")+
  theme_bw()+
  geom_hline(data = PM2.5_EPA_short, aes(yintercept = yintercept, color = "Short-term Standard", linetype = "Short-term Standard"), linetype = "dashed") +
  # geom_hline(data = PM2.5_EPA_annual, aes(yintercept = yintercept, color = "Annual Standard", linetype = "Annual Standard"), linetype = "dashed") +
  scale_color_manual(values = c("Short-term Standard" = "red"),
                     labels = c("Short-term Standard" = "Primary")) + # EPA primary 24-hour PM2.5 standard at the level of 35 µg/m3. 
  scale_fill_manual(values = c("July 3rd" = "lightgoldenrod", "July 4th" = "deepskyblue"))+
  theme(plot.title = element_text(face ="bold"))

box_cancov100<-
  ggplot(all_cc_combined, aes(x = factor(cancov.100m), y = pm2.5_atm, fill = date)) + 
  geom_boxplot()+
  #geom_text(data = label_10, aes(label = Purple.Air.Name, y = pm2.5_atm), position = position_dodge(width = 0.75), vjust = -0.1, size = 3) +
  scale_y_continuous(limits = c(0, 40))+
  ggtitle("All GRIT Sensors: Tree Canopy Cover (100 Meters) and PM 2.5 Concentration ", 
          subtitle = "July 3rd vs July 4th")+
  labs(x= "Proportion of Tree Canopy Cover within 100 Meters", y  = "PM 2.5 Concentration (µg/m3)", fill = "Date",
       color = "EPA PM 2.5 standard", linetype = "EPA PM 2.5 Standards")+
  theme_bw()+
  geom_hline(data = PM2.5_EPA_short, aes(yintercept = yintercept, color = "Short-term Standard", linetype = "Short-term Standard"), linetype = "dashed") +
  # geom_hline(data = PM2.5_EPA_annual, aes(yintercept = yintercept, color = "Annual Standard", linetype = "Annual Standard"), linetype = "dashed") +
  scale_color_manual(values = c("Short-term Standard" = "red"),
                     labels = c("Short-term Standard" = "Short-Term")) + # EPA primary 24-hour PM2.5 standard at the level of 35 µg/m3. 
  scale_fill_manual(values = c("July 3rd" = "lightgoldenrod", "July 4th" = "deepskyblue"))+
  theme(plot.title = element_text(face ="bold"))

box_canco800<-
  ggplot(all_cc_combined, aes(x = factor(cancov.800m), y = pm2.5_atm, fill = date)) + 
  geom_boxplot()+
  #geom_text(data = label_10, aes(label = Purple.Air.Name, y = pm2.5_atm), position = position_dodge(width = 0.75), vjust = -0.1, size = 3) +
  scale_y_continuous(limits = c(0, 40))+
  ggtitle("All GRIT Sensors: Tree Canopy Cover (800 Meters) and PM 2.5 Concentration ", 
          subtitle = "July 3rd vs July 4th")+
  labs(x= "Proportion of Tree Canopy Cover within 800 Meters", y  = "PM 2.5 Concentration (µg/m3)", fill = "Date",
       color = "EPA PM 2.5 standard", linetype = "EPA PM 2.5 Standards")+
  theme_bw()+
  geom_hline(data = PM2.5_EPA_short, aes(yintercept = yintercept, color = "Short-term Standard", linetype = "Short-term Standard"), linetype = "dashed") +
  # geom_hline(data = PM2.5_EPA_annual, aes(yintercept = yintercept, color = "Annual Standard", linetype = "Annual Standard"), linetype = "dashed") +
  scale_color_manual(values = c("Short-term Standard" = "red"),
                     labels = c("Short-term Standard" = "Short-term")) + # EPA primary 24-hour PM2.5 standard at the level of 35 µg/m3. 
  scale_fill_manual(values = c("July 3rd" = "lightgoldenrod", "July 4th" = "deepskyblue"))+
  theme(plot.title = element_text(face ="bold"))


box_cancov20 + facet_wrap(vars(Purple.Air.Name)) #facet wrap option remove labels from graph, delete geom_text()
#repeat for rest of canopy cover buffers

#Line graph representing each sensors x = time, y = AQ 
#fixing time stamps for plot
GRIT01$time_stamp <- gsub("T|Z", " ", GRIT01$time_stamp)
  GRIT01$time_stamp <- substr(GRIT01$time_stamp, 1, 16) 
  GRIT01$time_stamp<- as.POSIXct(GRIT01$time_stamp, format = '%Y-%m-%d %H:%M')
GRIT02$time_stamp <- gsub("T|Z", " ",GRIT02$time_stamp)
  GRIT02$time_stamp <- substr(GRIT02$time_stamp, 1, 16) 
  GRIT02$time_stamp<- as.POSIXct(GRIT02$time_stamp, format = '%Y-%m-%d %H:%M')
GRIT03$time_stamp <- gsub("T|Z", " ",GRIT03$time_stamp)
  GRIT03$time_stamp <- substr(GRIT03$time_stamp, 1, 16) 
  GRIT03$time_stamp<- as.POSIXct(GRIT03$time_stamp, format = '%Y-%m-%d %H:%M')
GRIT04$time_stamp <- gsub("T|Z", " ",GRIT03$time_stamp)
  GRIT04$time_stamp <- substr(GRIT04$time_stamp, 1, 16) 
  GRIT04$time_stamp<- as.POSIXct(GRIT04$time_stamp, format = '%Y-%m-%d %H:%M')

#line graph  with all GRIT sensors, changed date and time so it's easier to read
all_cc_GRIT01 <- all_cc_combined %>% filter(Purple.Air.Name == "GRIT01") %>% mutate(as.date(date_time = GRIT01$time_stamp))
all_cc_GRIT02 <- all_cc_combined %>% filter(Purple.Air.Name == "GRIT02") %>% mutate(as.date(date_time = GRIT02$time_stamp))
all_cc_GRIT03 <- all_cc_combined %>% filter(Purple.Air.Name == "GRIT03") %>% mutate(date_time = GRIT03$time_stamp)
all_cc_GRIT04 <- all_cc_combined %>% filter(Purple.Air.Name == "GRIT04") %>% mutate(date_time = GRIT04$time_stamp)

line_allGRIT <- ggplot()+
    geom_line(data = all_cc_GRIT01, mapping = aes(date_time, pm2.5_atm, group = 1, color = "GRIT01")) +
    geom_line(data = all_cc_GRIT02, mapping = aes(date_time, pm2.5_atm, group = 1, color = "GRIT02"))+
    geom_line(data = all_cc_GRIT03, mapping = aes(date_time, pm2.5_atm, group = 1, color = "GRIT03"))+
    geom_line (data = all_cc_GRIT04, mapping = aes(date_time, pm2.5_atm, group = 1, color = "GRIT04"))+
  geom_hline(data = PM2.5_EPA_short, aes(yintercept = yintercept, color = "Short-term Standard", linetype = "Short-term Standard")) +
  geom_hline(data = PM2.5_EPA_annual, aes(yintercept = yintercept, color = "Annual Standard", linetype = "Annual Standard")) +
  ggtitle("All GRIT Sensors: PM 2.5 Concentrations July 3rd - July 4th 2024") +
  scale_color_discrete(breaks=c("GRIT01", "GRIT02", "GRIT03", "GRIT04"))+
  #scale_color_manual(values = c('black', 'darkolivegreen3', 'gray', 'lightgoldenrod',"blue","black"))+
  scale_linetype_manual(values = c("Short-term Standard" = "dotted", "Annual Standard" = "dashed"),) +
  labs( x = "Time", y = "PM 2.5 Concentration (µg/m3)",linetype = "EPA PM 2.5 Standards", 
        color ="GRIT Sensors" )+
  theme_bw()+
  theme(plot.title = element_text(face = "bold"))


install.packages("ggprism")
library(ggprism)

line_allGRIT + guides(x = guide_prism_minor())



  


#######################################################
### Script to look at all July 3rd vs July 4th PA  ###

csv_files <- list.files(path = "~/PurpleAir/PA Data Download July 3rd and July 4th 1 hr avg", pattern = "*.csv", full.names = TRUE)


data_list <- list()

for (file in csv_files) {
  data <- read.csv(file)
  data_list[[file]] <- data
}
combined_data <- do.call(rbind, data_list)  
write.csv(combined_data,"~/Documents/GitHub/grit/data/PurpleAircombinedJuly.csv", row.names = FALSE)
PA_july <- read.csv("~/Documents/GitHub/grit/data/PurpleAir/PA Data Download July 3rd and July 4th 1 hr avg/PurpleAircombinedJuly.csv", header=TRUE)
PA_july <- PA_july %>%
          mutate(pm2.5_correct= (0.524*pm2.5_cf_1) - (0.0852*humidity) + 5.72)
unique_sensors <- unique(PA_july$sensor_index)

filtered_data <- split(PA_july, PA_july$sensor_index)

PA_101109 <- filtered_data[["101109"]]
  july3_101109<- slice(PA_101109,1:24)
  july4_101109<-slice(PA_101109,25:48)
PA_136172 <- filtered_data[["136172"]]
  july3_136172<-slice(PA_136172,1:24)
  july4_136172<-slice(PA_136172,25:48)
PA_15203 <- filtered_data[["15203"]]
  july3_15203<-slice(PA_15203,1:24)
  july4_15203<-slice(PA_15203,25:48)
PA_156499 <-filtered_data[["156499"]]
  july3_156499<-slice(PA_156499,1:24)
  july4_156499<-slice(PA_156499,25:48)
PA_167065<-filtered_data[["167065"]]
  july3_167065<-slice(PA_167065,1:24)
  july4_167065<-slice(PA_167065,25:48)
PA_167399<-filtered_data[["167399"]]
  july3_167399<-slice(PA_167399,1:24)
  july4_167399<-slice(PA_167399,25:48)
PA_167413<-filtered_data[["167413"]]
  july3_167413<-slice(PA_167413,1:24)
  july4_167413<-slice(PA_167413,25:48)
PA_171177<-filtered_data[["171177"]]
  july3_171177<-slice(PA_171177,1:24)
  july4_171177<-slice(PA_171177,25:48)
PA_171181<-filtered_data[["171181"]]
  july3_171181<-slice(PA_171181,1:24)
  july4_171181<-slice(PA_171181,25:48)
PA_171217<-filtered_data[["171217"]]
  july3_171217<-slice(PA_171217,1:24)
  july4_171217<-slice(PA_171217,25:28)
PA_173501<-filtered_data[["173501"]]
  july3_173501<-slice(PA_173501,1:24)
  july4_173501<-slice(PA_173501,25:48)
PA_174613<-filtered_data[["174613"]]
  july3_174613<-slice(PA_174613,1:24)
  july4_174613<-slice(PA_174613,25:48)
PA_17663<-filtered_data[["17663"]]
  july3_17663<-slice(PA_17663,1:24)
  july4_17663<-slice(PA_17663,25:48)
PA_177979<-filtered_data[["177979"]]
  july3_177979<-slice(PA_177979,1:24)
  july4_177979<-slice(PA_177979,25:48)
PA_183851<-filtered_data[["183851"]]
  july3_18351<-slice(PA_183851,1:24)
  july4_18351<-slice(PA_183851,25:48)
PA_183853<-filtered_data[["183853"]]
  july3_183853<-slice(PA_183853,1:21)
  july4_183853<-slice(PA_183853,22:45)
PA_183863<-filtered_data[["183863"]]
  july3_183863<-slice(PA_183863,1:24)
  july4_183863<-slice(PA_183863,25:48)
PA_183883<-filtered_data[["183883"]]
  july3_183883<-slice(PA_183883,1:24)
  july4_183883<-slice(PA_183883,25:48)
PA_185377<-filtered_data[["185377"]]
  july3_185377<-slice(PA_185377,1:24)
  july4_185377<-slice(PA_185377,25:48)
PA_192199<-filtered_data[["192199"]]
  july3_192199<-slice(PA_192199,1:24)
  july4_192199<-slice(PA_192199,25:48)
PA_194473<-filtered_data[["194473"]]
  july3_194473<-slice(PA_194473,1:24)
  july4_194473<-slice(PA_194473,25:48)
PA_199129<-filtered_data[["199129"]]
  july3_199129<-slice(PA_199129,1:24)
  july4_199129<-slice(PA_199129,25:48)
PA_26687<-filtered_data[["26687"]]
  july3_26687<-slice(PA_26687,1:24)
  july4_26687<-slice(PA_26687,25:48)
PA_50485<-filtered_data[["50485"]]
  july3_50485<-slice(PA_50485,1:24)
  july4_50485<-slice(PA_50485,25:41)
PA_51969<-filtered_data[["51969"]]
  july3_51969<-slice(PA_51969,1:24)
  july4_51969<-slice(PA_51969,25:48)
PA_71029<-filtered_data[["71029"]]
  july3_71029<-slice(PA_71029,1:24)
  july4_71029<-slice(PA_71029,25:48)
PA_7384<-filtered_data[["7384"]]
  july3_7384<-slice(PA_7384,1:24)
  july4_7384<-slice(PA_7384,25:48)
PA_86009<-filtered_data[["86009"]]
  july3_86009<-slice(PA_86009,1:24)
  july4_86099<-slice(PA_86009,25:48)
PA_9732<-filtered_data[["9732"]]
  july3_9732<-slice(PA_86009,1:24)
  july4_9732<-slice(PA_86009,25:48)
PA_98105<-filtered_data[["98105"]]
  july3_98105<-slice(PA_98105,1:24)
  july4_98105<-slice(PA_98105,25:48)
  
july3<-list(july3_101109,july3_136172,july3_15203,july3_156499,july3_167065,july3_167399,july3_167413,july3_171177,july3_171181,july3_171217,july3_173501,july3_174613,july3_17663,july3_177979,july3_18351,july3_183853,july3_183863,july3_183883,july3_185377,july3_192199,
                july3_194473,july3_199129,july3_26687,july3_50485,july3_51969,july3_71029,july3_7384,july3_86009,july3_9732,july3_98105)
all_july3<-bind_rows(july3)
write.csv(all_july3,"~/Documents/GitHub/grit/data/PurpleAir/allJuly3rd.csv",row.names = FALSE)

july4<-list(july4_101109,july4_136172,july4_15203,july4_156499,july4_167065,july4_167399,july4_167413,july4_171177,july4_171181,july4_171217,july4_173501,july4_174613,july4_17663,july4_177979,july4_18351,july4_183853,july4_183863,july4_183883,july4_185377,july4_192199,
           july4_194473,july4_199129,july4_26687,july4_50485,july4_51969,july4_71029,july4_7384,july4_86009,july4_9732,july4_98105)
all_july4<-bind_rows(july4)
write.csv(all_july4,"~/Documents/GitHub/grit/data/PurpleAir/allJuly4th.csv",row.names = FALSE)




