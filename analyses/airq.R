#######################################################
### Script to look at GRIT purpleair air quality data ###
################# Started June 20, 2024 #####################
################ ailene.ettinger@tnc.org ##############
#######################################################

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
install.packages("patchwork")
install.packages("plotly")
install.packages("gridExtra")

# load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(plotly)
library(gridExtra)


# set working directory
setwd("documents/GitHub/grit/analyses") #for apple computers add documents

#Read in data file with PurpleAir logger locations
#(notyet created)
#Read in air quality data from 2 sites
GRIT01<-read.csv("../data/PurpleAir/GRIT01-2024-07-03 2024-07-05.csv", header=TRUE) 
GRIT02<-read.csv("../data/PurpleAir/GRIT02 2024-07-03 2024-07-05.csv", header=TRUE) 
GRIT03<-read.csv("../data/PurpleAir/GRIT03 2024-07-03 2024-07-05.csv", header=TRUE) 
GRIT04<-read.csv("../data/PurpleAir/GRIT04 2024-07-03 2024-07-05.csv", header=TRUE) 

#look at the data
head(GRIT01)
summary(GRIT02)
dim(GRIT02)
#understand the data a bit more- what are these 2 columns- max and min?
range(GRIT01$pm2.5_atm_a, na.rm=TRUE) #_atm vs _cf_1, atm is for outdoor sensors, cf_1 is for indoor
range(GRIT01$pm2.5_atm_b, na.rm=TRUE)
mean(c(GRIT01$pm2.5_atm_b,GRIT01$pm2.5_atm_b, na.rm=TRUE),na.rm=TRUE)

#Separating sensors by date, cleaning data

GRIT01_July03 <- GRIT01 %>% slice(1:41)
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

#Graphs

# GRIT01 pm2.5_atm on July 3rd 
p1 <- 
  ggplot(data = GRIT01_July03,
          aes(date_time, pm2.5_atm, group = 1)) +
          geom_line(color = "darkgreen") +
          geom_point(color = "darkgreen")+
          labs(title = "GRIT01 Particulate Matter 2.5 Concentrations on July 3rd, 2024",
           x = "Time", 
           y = "PM 2.5 Concentrations (µg/m3)" ) + 
          theme(axis.text.x = element_text (angle = 90, vjust = -0.01))
print(p1)
#GRIT01 Temp and PM 2.5 on July 3rd
p2 <-ggplot(data = GRIT01_July03,
          aes(date_time, temperature, group =1)) +
          geom_line()+
          labs(x = "Time", 
            y = "Temperature Inside of Sensor (F)") + #Temp readings on .csv reflect average temp of two channels within sensor housing, 8F higher than ambient temp
          theme(axis.text.x = element_text (angle = 90, vjust = -0.01)) 

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
  labs(title = "GRIT01 Particulate Matter 2.5 Concentrations on July 4th, 2024",
       x = "Time", 
       y = "PM 2.5 Concentrations (µg/m3)" ) +
      theme(axis.text.x = element_text (angle = 90, vjust = -0.01))

#July 4th vs July 3rd pm2.5_atm 
x = ggplot() + 
    geom_line(data = GRIT_July04_p,  aes(x = date_time, y = pm2.5_atm, group = 1, color = "July 4rd")) +
    geom_line (data = GRIT01_July03, aes(date_time, pm2.5_atm, group = 1, color = "July 3rd")) +
    theme(axis.text.x = element_text (angle = 90, vjust = -0.01))+
    labs(title = "GRIT01 PM 2.5 July 3rd vs. July 4th",
        x = "Time", 
        y = "PM 2.5 Concentrations (µg/m3)",
        color = "Day") +
  scale_color_manual(values=c('darkgreen', 'blue'))
print(x)

#GRIT01 July4th Temp & PM2.5 Readings graphed together
coeff <- 0.1
p6 <- 
  ggplot (GRIT01_July04, aes (x=date_time))+
  geom_line ( aes(y = temperature, group = 1, color= "Temperature")) +
  geom_line ( aes(y = pm2.5_atm / coeff, group = 1, color = "PM 2.5 Concentration")) +
  scale_y_continuous(name = "Temperature Inside of Sensor (F)",
                     sec.axis = sec_axis(~.*coeff, name =  "PM 2.5 Concentrations (µg/m3)" ))+
  ggtitle("GRIT01 Sensor Temperature (F) and PM 2.5 (µg/m3) Concentrations on July 4th")+
  labs( x = "Time", color = "Indicator")+
  theme(axis.text.x = element_text (angle = 90, vjust = -0.01),
        plot.title = element_text(face='bold'))+
  scale_color_manual(values=c('darkgreen', 'blue'))

#GRIT01 channel a vs. channel b pm2.5_atm conc. on July 4th
p7 <-   ggplot() + 
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

