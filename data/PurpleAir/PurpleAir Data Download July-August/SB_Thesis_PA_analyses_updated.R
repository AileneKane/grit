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
library(mgcv)

setwd("/Users/samiebaclig/Documents/GitHub/grit") 
PA_locs<-read.csv("~/Documents/GitHub/grit/analyses/output/grit_aq_lc_jul_aug_updated.csv")


###separated DF
df <-
  list.files(path = "~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Data Download July-August", pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(.))

##using PA correction factor they used Cf_1 values but used atm since used for outdoor monitoring 
PA <- df %>% select(time_stamp,humidity,pressure,pm2.5_atm,temperature, sensor_index) %>%
    mutate (pm2.5_correct =(0.541*pm2.5_atm)-(0.0618*humidity) +(0.00534*temperature +3.634)) %>%
  group_by(sensor_index) %>% group_split()%>%
  setNames(df %>% group_by(sensor_index) %>% 
             group_keys() %>% 
             pull(sensor_index))

pa_7384 <- PA[['7384']]
avg_7384 <- mean(pa_7384$pm2.5_correct)
avg_9732<- mean(PA[["9732"]]$pm2.5_correct)
avg_15203 <- mean(PA[["15203"]]$pm2.5_correct)
avg_17663<-mean(PA[["17663"]]$pm2.5_correct)
avg_50485<- mean(PA[["50485"]]$pm2.5_correct)
avg_51969 <- mean(PA[["51969"]]$pm2.5_correct)
avg_71029<- mean(PA[["71029"]]$pm2.5_correct) ## this sensor 'Woodland/Summit' really high average PA' 
avg_80321<-mean(PA[["80321"]]$pm2.5_correct)
avg_94745<-mean(PA[["94745"]]$pm2.5_correct)
avg_96765<-mean(PA[["96765"]]$pm2.5_correct)
avg_98105<-mean(PA[["98105"]]$pm2.5_correct)
avg_135354<-mean(PA[["135354"]]$pm2.5_correct)
avg_136172<-mean(PA[["136172"]]$pm2.5_correct)
avg_152162<-mean(PA[["152162"]]$pm2.5_correct)
avg_154963<-mean(PA[["154963"]]$pm2.5_correct) #not in lc
avg_156499<-mean(PA[["156499"]]$pm2.5_correct)
avg_167047<-mean(PA[["167047"]]$pm2.5_correct)
avg_167065<-mean(PA[["167065"]]$pm2.5_correct)
avg_167257<-mean(PA[["167257"]]$pm2.5_correct)
avg_167399<-mean(PA[["167399"]]$pm2.5_correct)
avg_167413<-mean(PA[["167413"]]$pm2.5_correct)
avg_171177<-mean(PA[["171177"]]$pm2.5_correct)
avg_171217<-mean(PA[["171217"]]$pm2.5_correct)
avg_171237<-mean(PA[["171237"]]$pm2.5_correct)
avg_173501<-mean(PA[["173501"]]$pm2.5_correct)
avg_174613<-mean(PA[["174613"]]$pm2.5_correct)
avg_175733<- mean(PA[["175733"]][["pm2.5_correct"]])
avg_177455<- mean(PA[["177455"]][["pm2.5_correct"]]) ## artondale farm in gig harbor also very high average PA (~283), starting 07/9-07/28
avg_177979<-mean(PA[["177979"]][["pm2.5_correct"]])
avg_183851<-mean(PA[["183851"]][["pm2.5_correct"]])
avg_183853<-mean(PA[["183853"]][["pm2.5_correct"]])
avg_183863<-mean(PA[["183863"]][["pm2.5_correct"]])
avg_183883<-mean(PA[["183883"]][["pm2.5_correct"]])
avg_185377<-mean(PA[["185377"]][["pm2.5_correct"]])
avg_190605<-mean(PA[["190605"]][["pm2.5_correct"]]) # not in lc
avg_192199<-mean(PA[["192199"]][["pm2.5_correct"]])
avg_194473<-mean(PA[["194473"]][["pm2.5_correct"]])
avg_205579<-mean(PA[["205579"]][["pm2.5_correct"]])
avg_224449<-mean(PA[["224449"]][["pm2.5_correct"]])

pa_values <- ls(pattern = "^avg_[0-9]+$")

pm2.5_avg <- c(avg_135354,avg_136172,avg_15203,avg_152162,
               avg_156499,avg_167047,avg_167065,
               avg_167257,avg_167399,avg_167413,
               avg_171177,avg_171217,avg_171237,avg_173501,
               avg_174613,avg_175733,avg_17663,avg_177455,
               avg_177979,avg_183851,avg_183853,
               avg_183863,avg_183883,avg_185377,
               avg_192199,avg_194473,avg_205579,avg_224449,
               avg_50485,avg_51969,avg_71029,avg_7384,
               avg_80321,avg_94745,avg_96765,avg_9732,
               avg_98105)
sensor_indices <- c(135354, 136172, 15203, 152162, 
                    156499, 167047, 167065, 
                    167257, 167399, 167413, 
                    171177, 171217, 171237, 173501, 
                    174613, 175733, 17663, 177455, 
                    177979, 183851, 183853, 
                    183863, 183883, 185377, 
                    192199, 194473, 205579, 224449, 
                    50485, 51969, 71029, 7384, 
                    80321, 94745, 96765, 9732, 
                    98105) 
average_df <- data.frame(sensor_index= sensor_indices,avg_pm2.5 = pm2.5_avg)
lc<-read.csv("~/Documents/GitHub/grit/analyses/output/grit_aq_lc_jul_aug_updated.csv")

combined_df <- full_join(average_df, lc)
combined_df<- subset(combined_df, sensor_index !=177455)

###AQ threshold 
#jul_7384<- pa_7384 %>%
  #filter(grepl("2024-07", time_stamp))

pa_7384_above9<-length(which(pa_7384$pm2.5_correct>9))/2

pa_183863 <- PA[['183863']]  
pa_183863_above9<-length(which(pa_183863$pm2.5_correct>9))/2

pa_71029 <- PA[['71029']]
pa_71029_above9<-length(which(pa_71029$pm2.5_correct>9))/2



####Plots###
##july + august 

PM2.5_EPA_annual <- data.frame(yintercept = 9, Lines = 'Annual') # long-term standard (annual average)


##cancov avg, changing radii when saving plots
cancov_avg<- ggplot(combined_df, aes(cancov.800m,avg_pm2.5))+
  geom_point()+
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))+
  geom_hline(data = PM2.5_EPA_annual, aes(yintercept = yintercept, color = "Annual Standard", linetype = "Annual"), 
           linetype = "dashed")+
  theme(legend.position= c(0.89, 0.89))


  

