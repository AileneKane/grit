# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
install.packages("patchwork")
install.packages("gridExtra")
install.packages("Metrics")
install.packages("multcomp")

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
library(Metrics)
library(multcomp)
setwd("/Users/samiebaclig/Documents/GitHub/grit") 
#for ailene
#PA_locs<-read.csv("analyses/output/grit_aq_lc_jul_aug_updated.csv")

## April 6 Samie Updated data frame with all info
data <- read.csv("~/Documents/GitHub/grit/analyses/output/grit_aq_lc&pm2.5_jul_aug_updated.csv")
data <- data %>%  mutate(shrubcancov.200m = shrubcov.200m + cancov.200m)
###separated DF
#df <-
  #list.files(path = "~/Documents/GitHub/grit/data/PurpleAir/PurpleAir Data Download July-August", pattern = "*.csv", full.names = TRUE) %>% 
  #map_df(~read_csv(.))
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
#average_df <- data.frame(sensor_index= sensor_indices,avg_pm2.5 = pm2.5_avg)
#lc<-read.csv("~/Documents/GitHub/grit/analyses/output/grit_aq_lc_jul_aug_updated.csv")
#imp<-read.csv("~/Documents/GitHub/grit/analyses/output/grit_aq_imp_jul_aug_updated.csv")
#for ailene:
#lc<-read.csv("analyses/output/grit_aq_lc_jul_aug_updated.csv")
#imp<-read.csv("analyses/output/grit_aq_imp_jul_aug_updated.csv")

#for ailene
#lc<-read.csv("analyses/output/grit_aq_lc_jul_aug_updated.csv")
#imp<-read.csv("analyses/output/grit_aq_imp_jul_aug_updated.csv")

#imp<- imp %>% as.numeric(imp$sensor_index) 
#for ailene: 
#imp$sensor_index<- as.numeric(imp$sensor_index) 

#shrub_tree<-read.csv("~/Documents/GitHub/grit/analyses/output/grit_aq_shrub&canopy_jul_aug_updated.csv")
#for ailene:
#shrub_tree<-read.csv("analyses/output/grit_aq_shrub&canopy_jul_aug_updated.csv")

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

#combined_df <- left_join(average_df, lc, by = "sensor_index")
#combined_df <- left_join(combined_df,imp, by = "sensor_index")
#combined_df <- left_join(combined_df,shrub_tree, by = "sensor_index")
#combined_df <- combined_df %>% select (sensor_index, avg_pm2.5,long,lat,name,
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

PM2.5_EPA_annual <- data.frame(yintercept = 9, Lines = 'Annual') # long-term standard (annual average)a

##tree canopy cancov avg, changing radii when saving plot
cancov_avg<- ggplot(data, aes(x = (cancov.200m * 100) ,y = avg_pm2.5))+
  geom_point(color="darkgreen")+
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian), color="black", size=0.5)+
  geom_hline(data = PM2.5_EPA_annual, aes(yintercept = yintercept, color = "Annual Standard", linetype = "Annual"), 
           linetype = "dashed")+
  scale_y_continuous(limits = c(0, 27))+  #there is an outlier artondale, that got cut off from
  scale_color_manual(values = c("Annual Standard" = "black"))+
  annotate("text", label ="EPA Annual Standard (9.0 μg/m3)", x=2, y = 10, size = 3)+
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position= "none")+
    labs(y= "Average PM2.5", x = "Tree Canopy Cover Within 200m (%)")+
    #ggtitle("Tree Canopy Cover Within 200m (%) vs Average PM2.5")
ggsave("cancov200.png", width = 5, height = 5, dpi = 300)


##hours above EPA PM2.5 long term standard (9)
above9_hours <- ggplot(data, aes((cancov.200m * 100) , above9_hour))+
  geom_point(color = "darkgreen")+
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian), color="black", size=0.5)+
  theme_classic()+
  labs(x = "Tree Canopy Cover within 200m (%)", y = "Number of Hours above 9.0 μg/m3")+
  theme(axis.title = element_text(face="bold"))
ggsave("cancov200hr.png", width = 5, height = 5, dpi = 300)
 # ggtitle("Number of Hours above EPA PM2.5 Annual Standard vs Tree Canopy Cover")+
  
  

all_cancov_long <- data%>%
  pivot_longer(cols = starts_with("cancov"), 
               names_to = "canopy_type", 
               values_to = "cancov_percent")
all_cancov_filtered <- all_cancov_long %>%
  filter(grepl("cancov", canopy_type)) 
all_cancov_filtered$canopy_type <- gsub("cancov\\.|_percent", "", all_cancov_filtered$canopy_type)
all_cancov_filtered$canopy_type <- trimws(all_cancov_filtered$canopy_type)
all_cancov_filter <- all_cancov_filtered %>% 
  mutate(canopy_type = factor(canopy_type, levels = c("10m", "20m","30m","40m","50m", "100m","200m","400m" ,"800m"))) %>%
  arrange(canopy_type) %>%
  filter(canopy_type == "10m" | canopy_type == "200m")
allcanpm <- ggplot(all_cancov_filter, aes(x = cancov_percent, y = avg_pm2.5)) +
  geom_point(size = 2) +  
  stat_smooth(method = "gam", method.args = list(family = gaussian)) +  # Add GAM smooth line
  labs(
    title = "PM 2.5 Concentrations vs. Tree Canopy Cover by Buffer Size",
    x = "Tree Canopy Cover",
    y = "PM 2.5 Concentration (µg/m3)") +
  scale_y_continuous(limits = c(0, 300)) +  
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"))+
  scale_color_viridis_d(option = "D")+
  facet_wrap(~canopy_type, scales = "fixed")  
ggsave("allcanpm.png", width = 8, height = 5, dpi = 300)






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

#added by ailene 4/2/2025:
#Use slopes/effect sizes from above models to make a plot of effect sizes of canopy cover on particulate matter

eff<-c(coef(hrm10)[2],coef(hrm20)[2],coef(hrm30)[2],coef(hrm40)[2],coef(hrm50)[2],
       coef(hrm100)[2],coef(hrm200)[2],coef(hrm400)[2],coef(hrm800)[2])
#To make map of loggers by their temperatures, save a csv file with tmax and tmin on hottest days in jun and jult:
ps<-c(summary(hrm10)$coef[2,4],summary(hrm20)$coef[2,4],summary(hrm30)$coef[2,4],summary(hrm40)$coef[2,4],summary(hrm50)$coef[2,4],
      summary(hrm100)$coef[2,4],summary(hrm200)$coef[2,4],summary(hrm400)$coef[2,4],summary(hrm800)$coef[2,4])
cancols=c(rep("darkgreen", times=length(ps)))
cancols2<-cancols
cancols2[which(ps>0.1)]<-"white"

png(file="analyses/PurpleAir figs/caneffects.png",width =3000, height =1500 ,res =300)
x<-barplot(eff, col=cancols2, border=cancols,ylim=c(-8000,8000), 
           cex.lab=1.3,cex.axis=1.2,cex.names=1.2,
           ylab="Effect of canopy on hours with bad air quality",xlab="Distance (m)",names.arg=c("10","20","30","40","50","100","200","400","800"))
error<-c(summary(hrm10)$coef[2,2],summary(hrm20)$coef[2,2],summary(hrm30)$coef[2,2],summary(hrm40)$coef[2,2],summary(hrm50)$coef[2,2],
         summary(hrm100)$coef[2,2],summary(hrm200)$coef[2,2],summary(hrm400)$coef[2,2],summary(hrm800)$coef[2,2])
for(i in 1:length(eff)){
  arrows(x[i],eff[i]+error[i],x[i],eff[i]-error[i], code=3, angle=90, length=0.05,  lwd=2)
}
abline(h=0)
dev.off() 



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

#### Creating linear models 4/4/25###
#response var: above 9 days
cchrm10<- lm(above9_hour ~ cancov.10m, data=data)
cchrm20<- lm(above9_hour~cancov.20m,data=data)
cchrm30<- lm(above9_hour~cancov.30m,data=data)
cchrm40<- lm(above9_hour~cancov.40m,data=data)
cchrm50<- lm(above9_hour~cancov.50m,data=data)
cchrm100<-lm(above9_hour~cancov.100m,data=data)
cchrm200<-lm(above9_hour~cancov.200m,data=data)
cchrm400<-lm(above9_hour~cancov.400m,data=data)
cchrm800<-lm(above9_hour~cancov.800m,data=data)

schrm10<- lm(above9_hour~shrubcov.10m,data=data)
schrm20<- lm(above9_hour~shrubcov.20m,data=data)
schrm30<- lm(above9_hour~shrubcov.30m,data=data)
schrm40<- lm(above9_hour~shrubcov.40m,data=data)
schrm50<- lm(above9_hour~shrubcov.50m,data=data)
schrm100<- lm(above9_hour~shrubcov.100m,data=data)
schrm200<- lm(above9_hour~shrubcov.200m,data=data)
schrm400<- lm(above9_hour~shrubcov.400m,data=data)
schrm800<- lm(above9_hour~shrubcov.800m,data=data)

sccchrm10<- lm(above9_hour~cancov.10m + shrubcov.10m, data=data)
sccchrm20<- lm(above9_hour~cancov.20m + shrubcov.20m, data=data)
sccchrm30<- lm(above9_hour~cancov.30m + shrubcov.30m, data=data)
sccchrm40<- lm(above9_hour~cancov.40m + shrubcov.40m, data=data)
sccchrm50<- lm(above9_hour~cancov.50m +shrubcov.50m, data=data)
sccchrm100<- lm(above9_hour~cancov.100m + shrubcov.100m, data=data)
sccchrm200<- lm(above9_hour~cancov.200m + shrubcov.200m, data=data)
sccchrm400<- lm(above9_hour~cancov.400m + shrubcov.400m, data=data)
sccchrm800<- lm(above9_hour~cancov.800m + shrubcov.800m, data=data)
##comparing models
AIC(cchrm10,cchrm20,cchrm30,cchrm40,cchrm50,cchrm100,cchrm200,cchrm400,cchrm800,
    schrm10,schrm20,schrm30,schrm40,schrm50,schrm100,schrm200,schrm400,schrm800,
    sccchrm10,sccchrm20,sccchrm30,sccchrm40,sccchrm50,sccchrm100,sccchrm200,sccchrm400,sccchrm800)
#sccchrm200 lowest AIC
summary(sccchrm200)
summary(sccchrm200)$r.squared 
summary(sccchrm200)$adj.r.squared
rmse(data$above9_hour, sccchrm200$fitted.values)

#response var mean pm2.5
ccmnpm.10<-lm(avg_pm2.5~cancov.10m,data=data)
ccmnpm.20<-lm(avg_pm2.5~cancov.20m,data=data)
ccmnpm.30<- lm(avg_pm2.5~cancov.30m,data=data)
ccmnpm.40<- lm(avg_pm2.5~cancov.40m,data=data)
ccmnpm.50<- lm(avg_pm2.5~cancov.50m,data=data)
ccmnpm.100<- lm(avg_pm2.5~cancov.100m, data=data)
ccmnpm.200<- lm(avg_pm2.5~cancov.200m,data=data)
ccmnpm.400<- lm(avg_pm2.5~cancov.400m,data=data)
ccmnpm.800<- lm(avg_pm2.5~cancov.800m,data=data)

scmnpm.10<-lm(avg_pm2.5~shrubcov.10m, data=data)
scmnpm.20<- lm(avg_pm2.5~shrubcov.20m,data=data)
scmnpm.30<- lm(avg_pm2.5~shrubcov.30m,data=data)
scmnpm.40<- lm(avg_pm2.5~shrubcov.40m, data=data)
scmnpm.50<- lm(avg_pm2.5~shrubcov.50m, data=data)
scmnpm.100<- lm(avg_pm2.5~shrubcov.100m, data=data)
scmnpm.200<- lm(avg_pm2.5~shrubcov.200m, data=data)
scmnpm.400m<- lm(avg_pm2.5~shrubcov.400m, data=data)
scmnpm.800m<- lm(avg_pm2.5~shrubcov.800m, data=data)

scccmnpm.10<- lm(avg_pm2.5~shrubcov.10m + cancov.10m, data=data)
scccmnpm.20<- lm(avg_pm2.5~shrubcov.20m + cancov.20m, data=data)
scccmnpm.30<- lm(avg_pm2.5~shrubcov.30m + cancov.30m, data=data)
scccmnpm.40<- lm(avg_pm2.5~shrubcov.40m + cancov.40m, data=data)
scccmnpm.50<- lm(avg_pm2.5~shrubcov.50m + cancov.50m, data=data)
scccmnpm.100<- lm(avg_pm2.5~shrubcov.100m + cancov.100m, data=data)
scccmnpm.200<- lm(avg_pm2.5~shrubcov.200m + cancov.200m, data=data)
scccmnpm.400<-lm(avg_pm2.5~shrubcov.400m+cancov.400m, data=data)
scccmnpm.800<-lm(avg_pm2.5~shrubcov.800m+cancov.800m, data=data)
##comparing models
AIC(ccmnpm.10,ccmnpm.20,ccmnpm.30,ccmnpm.40,ccmnpm.50,ccmnpm.100,ccmnpm.200,ccmnpm.400,ccmnpm.800,
    scmnpm.10,scmnpm.20,scmnpm.30,scmnpm.40,scmnpm.50,scmnpm.100,scmnpm.200,scmnpm.400m,scmnpm.800m,
    scccmnpm.10, scccmnpm.20,scccmnpm.30,scccmnpm.40,scccmnpm.50,scccmnpm.200,scccmnpm.400,scccmnpm.800)
##200 lowest AIC shrub + cc
summary(scccmnpm.200)
summary(scccmnpm.200)$r.squared
summary(scccmnpm.200)$adj.r.squared
rmse(data$avg_pm2.5, scccmnpm.200$fitted.values)


data$EquityInde<-as.factor(data$EquityInde)
data$EquityInde <- relevel(as.factor(data$EquityInde), ref = "Very Low")
ccequitymod<-lm(cancov.200m ~EquityInde, data=data)

summary(ccequitymod)
anova(ccequitymod)

post.hoc<-glht(ccequitymod,linfct=mcp(EquityInde='Tukey'))
summary(post.hoc)

###Equity Index Plots###
data$EquityInde <- factor(data$EquityInde, levels = c("Very Low", "Low", "Moderate","High","Very High"))

equityindex10<- data %>% filter(!is.na(EquityInde)) %>%
  ggplot(aes(x=EquityInde,y=(cancov.10m * 100),fill=EquityInde))+
    geom_boxplot(na.rm=TRUE)+
    labs(x = "Equity Index", y = "Tree Canopy Cover Within 10m (%)")+
    scale_fill_brewer(palette="Greens")+
    theme_bw(base_size=14)+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position="none")+
    ggtitle("Tree Canopy Cover vs. Equity Index")
ggsave("equityindex10.png", width = 8, height = 5, dpi = 300)

plotdata <- data %>% filter(!is.na(EquityInde)) %>% group_by(EquityInde)%>% summarise(mean_above9hr = mean(above9_hour),
                                                                                      median_above9 = median(above9_hour))
x <- ggplot(data = plotdata,
            aes(x=EquityInde, y = median_above9))+
  geom_bar(stat="identity")

above9equity<- data %>%   filter(!is.na(EquityInde)) %>%
  ggplot(aes(x= EquityInde, y = above9_hour, fill=EquityInde)) +
  geom_boxplot() +
  theme_bw(base_size=14)+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position="none")+
  scale_fill_brewer(palette="Greens")+
  labs(y= "Number of Hours Above 9 μg/m3", x = "Equity Index")+
  ggtitle("Hours Above Threshold vs Equity Index")
ggsave("above9equity.png", width = 8, height = 5, dpi = 300)



