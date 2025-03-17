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
library(sf)
library(tigris)
library(sf)
library(dplyr)
library(readr)
setwd("/Users/samiebaclig/Documents/GitHub/grit") 
PA_locs<-read.csv("~/Documents/GitHub/grit/analyses/output/grit_aq_lc_jul_aug_updated.csv")
#for ailene
#PA_locs<-read.csv("analyses/output/grit_aq_lc_jul_aug_updated.csv")

###New Data###
##Roads##
roads <- st_read("~/Documents/GitHub/grit/data/pierce county roads/tl_2023_53053_roads.shp")

###separated DF
df <-
  list.files(path = "~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Data Download July-August", pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(.))
#for ailene
#df <-
#  list.files(path = "data/PurpleAir/PurpleAir Data Download July-August", pattern = "*.csv", full.names = TRUE) %>% 
#  map_df(~read_csv(.))

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
# avg_154963<-mean(PA[["154963"]]$pm2.5_correct) #not in lc
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
#avg_190605<-mean(PA[["190605"]][["pm2.5_correct"]]) # not in lc
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
imp<-read.csv("~/Documents/GitHub/grit/analyses/output/grit_aq_imp_jul_aug_updated.csv")
imp<- imp %>% as.numeric(imp$sensor_index) 

shrub_tree<-read.csv("~/Documents/GitHub/grit/analyses/output/grit_aq_shrub&canopy_jul_aug_updated.csv")
shrub_tree <- shrub_tree %>% 
  rename(
    shrubcov.10m = cancov.10m,
    shrubcov.20m = cancov.20m,
    shrubcov.30m = cancov.30m,
    shrubcov.40m = cancov.40m,
    shrubcov.50m = cancov.50m,
    shrubcov.100m = cancov.100m,
    shrubcov.200m = cancov.200m,
    shrubcov.400m = cancov.400m,
    shrubcov.800m = cancov.800m
  )
#for ailene
#lc<-read.csv("analyses/output/grit_aq_lc_jul_aug_updated.csv")

combined_df <- left_join(average_df, lc, by = "sensor_index")
combined_df <- left_join(combined_df,imp, by = "sensor_index")
combined_df <- left_join(combined_df,shrub_tree, by = "sensor_index")
combined_df <- combined_df %>% select (sensor_index, avg_pm2.5,long,lat,name,
                                       cancov.10m,cancov.20m,cancov.30m, cancov.40m, cancov.50m, cancov.100m, cancov.200m, cancov.400m, cancov.800m,
                                       impcov.10m, impcov.10m, impcov.20m, impcov.30m, impcov.40m, impcov.50m,impcov.100m,impcov.200m,impcov.400m,impcov.800m,
                                       shrubcov.10m,shrubcov.20m,shrubcov.30m,shrubcov.40m,shrubcov.50m,shrubcov.100m,shrubcov.200m,shrubcov.400m,shrubcov.800m)

###AQ threshold 
#jul_7384<- pa_7384 %>%
  #filter(grepl("2024-07", time_stamp))

above9_list <- list(
  pa_135354_above9 = length(which(PA[['135354']]$pm2.5_correct > 9)) / 2,
  pa_136172_above9 = length(which(PA[['136172']]$pm2.5_correct > 9)) / 2,
  pa_15203_above9  = length(which(PA[['15203']]$pm2.5_correct > 9)) / 2,
  pa_152162_above9 = length(which(PA[['152162']]$pm2.5_correct > 9)) / 2,
  pa_156499_above9 = length(which(PA[['156499']]$pm2.5_correct > 9)) / 2,
  pa_167047_above9 = length(which(PA[['167047']]$pm2.5_correct > 9)) / 2,
  pa_167065_above9 = length(which(PA[['167065']]$pm2.5_correct > 9)) / 2,
  pa_167257_above9 = length(which(PA[['167257']]$pm2.5_correct > 9)) / 2,
  pa_167399_above9 = length(which(PA[['167399']]$pm2.5_correct > 9)) / 2,
  pa_167413_above9 = length(which(PA[['167413']]$pm2.5_correct > 9)) / 2,
  pa_171177_above9 = length(which(PA[['171177']]$pm2.5_correct > 9)) / 2,
  pa_171217_above9 = length(which(PA[['171217']]$pm2.5_correct > 9)) / 2,
  pa_171237_above9 = length(which(PA[['171237']]$pm2.5_correct > 9)) / 2,
  pa_173501_above9 = length(which(PA[['173501']]$pm2.5_correct > 9)) / 2,
  pa_174613_above9 = length(which(PA[['174613']]$pm2.5_correct > 9)) / 2,
  pa_175733_above9 = length(which(PA[['175733']]$pm2.5_correct > 9)) / 2,
  pa_17663_above9  = length(which(PA[['17663']]$pm2.5_correct > 9)) / 2,
  pa_177455_above9 = length(which(PA[['177455']]$pm2.5_correct > 9)) / 2,
  pa_177979_above9 = length(which(PA[['177979']]$pm2.5_correct > 9)) / 2,
  pa_183851_above9 = length(which(PA[['183851']]$pm2.5_correct > 9)) / 2,
  pa_183853_above9 = length(which(PA[['183853']]$pm2.5_correct > 9)) / 2,
  pa_183863_above9 = length(which(PA[['183863']]$pm2.5_correct > 9)) / 2,
  pa_183883_above9 = length(which(PA[['183883']]$pm2.5_correct > 9)) / 2,
  pa_185377_above9 = length(which(PA[['185377']]$pm2.5_correct > 9)) / 2,
  pa_192199_above9 = length(which(PA[['192199']]$pm2.5_correct > 9)) / 2,
  pa_194473_above9 = length(which(PA[['194473']]$pm2.5_correct > 9)) / 2,
  pa_205579_above9 = length(which(PA[['205579']]$pm2.5_correct > 9)) / 2,
  pa_224449_above9 = length(which(PA[['224449']]$pm2.5_correct > 9)) / 2,
  pa_50485_above9  = length(which(PA[['50485']]$pm2.5_correct > 9)) / 2,
  pa_51969_above9  = length(which(PA[['51969']]$pm2.5_correct > 9)) / 2,
  pa_71029_above9  = length(which(PA[['71029']]$pm2.5_correct > 9)) / 2,
  pa_7384_above9   = length(which(PA[['7384']]$pm2.5_correct > 9)) / 2,
  pa_80321_above9  = length(which(PA[['80321']]$pm2.5_correct > 9)) / 2,
  pa_94745_above9  = length(which(PA[['94745']]$pm2.5_correct > 9)) / 2,
  pa_96765_above9  = length(which(PA[['96765']]$pm2.5_correct > 9)) / 2,
  pa_9732_above9   = length(which(PA[['9732']]$pm2.5_correct > 9)) / 2,
  pa_98105_above9  = length(which(PA[['98105']]$pm2.5_correct > 9)) / 2
)

above9_df <- data.frame(
  sensor_index = sensor_indices,
  above9_hour = unlist(above9_list),
  above9_days = (unlist(above9_list) /24)
)

combined_df <- dplyr::left_join(combined_df, above9_df, by = "sensor_index")



####Plots###

PM2.5_EPA_annual <- data.frame(yintercept = 9, Lines = 'Annual') # long-term standard (annual average)


##tree canopy cancov avg, changing radii when saving plots
cancov_avg<- ggplot(combined_df, aes(shrubcov.00m,avg_pm2.5))+
  geom_point()+
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))+
  geom_hline(data = PM2.5_EPA_annual, aes(yintercept = yintercept, color = "Annual Standard", linetype = "Annual"), 
           linetype = "dashed")+
  theme(legend.position= c(0.89, 0.89))

##days above EPA PM2.5 long term standard (9)
above9_days <- ggplot(combined_df, aes(cancov.800m, above9_days))+
  geom_point()+
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))+
  labs(x = "Proportion of Tree Canopy Cover within 800m",
       y = "Number of Days above 9 μg/m3")+
  ggtitle("Number of Days above EPA PM2.5 Annual Standard vs Tree Canopy Cover")



#added by ailene 28 feb 2025 
#fit a linear model- since plot looks linear
hrm10<-lm(above9_hour~cancov.10m, data=combined_df)
hrm20<-lm(above9_hour~cancov.20m, data=combined_df)
hrm30<-lm(above9_hour~cancov.30m, data=combined_df)
hrm40<-lm(above9_hour~cancov.40m, data=combined_df)
hrm50<-lm(above9_hour~cancov.50m, data=combined_df)
hrm100<-lm(above9_hour~cancov.100m, data=combined_df)
hrm200<-lm(above9_hour~cancov.200m, data=combined_df)
hrm400<-lm(above9_hour~cancov.400m, data=combined_df)
hrm800<-lm(above9_hour~cancov.800m, data=combined_df)

summary(hrm10)
coef(hrm10)
#canopy cover within 10 m has a negative effect on hours with air quality above the threshold!



###Census Information to include for proximity to hwys###
install.packages("tidycensus")
library(tidycensus)
equity_index <- st_read("~/Downloads/Equity_Index_1749294806086202155/Equity_Index.shp")
equity_index <- st_transform(equity_index, crs = 4326)
filtered_equity_index <- equity_index[grepl("^53053", equity_index$GEOID), ]

census_api_key("480d1711be5ee649f10d69553e6b42950bcf8ac3", install = TRUE)
locs <- read.csv("~/Documents/GitHub/grit/analyses/output/grit_aq_lc_jul_aug_updated.csv")
locs_sf <- st_as_sf(locs,coords = c("long", "lat"), crs = 4326)
cbg <- block_groups(state = "WA", class = "sf", year = 2024)
locs_sf <- st_transform(locs_sf, st_crs(cbg))

joined <- st_join(locs_sf, cbg, join = st_within) 
joined <- st_transform(joined, crs = 4326)
joined_filtered <- st_join(filtered_equity_index,joined)
joined <- joined %>%
  mutate(GEOID = as.character(GEOID))
filtered_equity_index <- filtered_equity_index %>%
  mutate(GEOID = as.character(GEOID))
joined_equity <- st_join(joined, filtered_equity_index)
joined_equity <- joined_equity %>% select(sensor_index, pov_rate, 
                                          FPLPct,RparkPct,CparkPct, NparkPct, PerOpSpc,
                                          AvgRdScr,TranRatScr,SelfRtHlth,
                                          PM2_5,PTRAF,AccIndx,Accessibil,EcoIndx,EconomicIn,
                                          EduIndx,EducationI,EquityInde,EquityIn_1,EnvIndx,Environmen,
                                          LivIndx,Livability)
joined_equity_df <- st_drop_geometry(joined_equity)

combined_df <- dplyr::left_join(combined_df, joined_equity_df, by = "sensor_index")
write.csv(combined_df,"~/Documents/GitHub/grit/analyses/output/grit_aq_lc&pm2.5_jul_aug_updated.csv", row.names = FALSE)


