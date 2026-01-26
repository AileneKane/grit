#######################################################
################## grit_aqvstrees.R ###################
######### Script to look at GRIT airtemp data #########
################ Started January 21, 2026 #############
################ ailene.ettinger@tnc.org ##############
#######################################################

#This script looks at Purple Air data with LiDAR-tree variables
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries
 library(dplyr)
# library(lme4)
 library(tidyr)
# library(car)
 library(scales)
 library(stringr)
# library(MuMIn)
# library(sjPlot)
 library(ggplot2)
# library(tidymv)
# library(mgcv)
# library(gridExtra)
# library(boot)
# set working directory
setwd("~/GitHub/grit/analyses")

# read in tree canopy, air quality, and location data
# csv file with lat long and other info about locations of GRIT sensors
locpm2.5hrs<-read.csv("output/purpleairloc_wpmhrs.csv", header=TRUE)                                          

# csv file with summary tree canopy data from lidar 
candat<-read.csv("output/canheight_within_buffers.csv", header=TRUE)

# convert candat to wide format
can_wide <- candat %>%
  pivot_wider(
    id_cols = ID,
    names_from = radius_m,
    values_from = c(mean_treeht_ft, total_conifers, total_nonconifer, n_features),
    names_glue = "{.value}_{radius_m}m"
  )

colnames(can_wide)[1]<-"Purple.Air.Name"


#merge locpm2.5 with can_wide
canaq<-left_join(locpm2.5hrs,can_wide)

#remove rows with NAs for pm.5
canaq<-canaq[!is.na(canaq$pm2.5est_sept),]
unique(canaq$n_features_200m)

#NAs in average height, number of conifers, etc should be 0s

canaq0 <- canaq %>%
  mutate(across(16:47, ~ replace_na(., 0)))

#one plot with all relationships between air quality and tree height, number of trees, etc

# ---- SET THESE ----
aq_col <- "pm2.5est_sept"   # <-- change to your air quality column name
id_col <- "Purple.Air.Name"            # <-- your site/sensor id column

df_plot <- canaq0 %>%
  pivot_longer(
    cols = matches("^(mean_treeht_ft|total_conifers|total_nonconifer|n_features)_\\d+m$"),
    names_to = c("metric", "radius_m"),
    names_pattern = "^(mean_treeht_ft|total_conifers|total_nonconifer|n_features)_(\\d+)m$",
    values_to = "x"
  ) %>%
  mutate(
    radius_m = as.integer(radius_m),
    metric = recode(metric,
                    mean_treeht_ft = "Mean tree height (ft)",
                    total_conifers = "Total conifers",
                    total_conifers = "Total nonconifers",
                    n_features     = "Number of features")
  )

p <- ggplot(df_plot, aes(x = x, y = .data[[aq_col]])) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue", linewidth = 0.9) +
  facet_grid(metric ~ radius_m, scales = "free_x") +
  labs(
    x = NULL,
    y = "PM2.5",
    title = "PM2.5 vs tree/landcover metrics by buffer radius"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p

#same plot but with all relationships between hours above threshold and tree height, number of trees, etc

# ---- SET THESE ----
aq_col2 <- "hrsabove25est"   # <-- change to your air quality column name

df_plot <- canaq0 %>%
  pivot_longer(
    cols = matches("^(mean_treeht_ft|total_conifers|total_nonconifer|n_features)_\\d+m$"),
    names_to = c("metric", "radius_m"),
    names_pattern = "^(mean_treeht_ft|total_conifers|total_nonconifer|n_features)_(\\d+)m$",
    values_to = "x"
  ) %>%
  mutate(
    radius_m = as.integer(radius_m),
    metric = recode(metric,
                    mean_treeht_ft = "Mean tree height (ft)",
                    total_conifers = "Total conifers",
                    total_conifers = "Total nonconifers",
                    n_features     = "Number of features")
  )

p2 <- ggplot(df_plot, aes(x = x, y = .data[[aq_col2]])) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue", linewidth = 0.9) +
  facet_grid(metric ~ radius_m, scales = "free_x") +
  labs(
    x = NULL,
    y = "Hours with PM2.5 >25",
    title = "Hours with PM2.5 >25 vs tree/landcover metrics by buffer radius"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p2


#now as separate plots


heightp2<-ggplot(df_tree, aes(x = treeht, y = .data[[aq_col2]])) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  facet_wrap(~ radius_m, scales = "free_x") +
  theme_bw() +
  labs(x = "Mean tree height (ft)", y = "Hours with PM2.5 >25",
       title = "PM2.5 vs mean tree height by radius")

heightp2

conifp2<-ggplot(df_conif, aes(x = conifers, y = .data[[aq_col2]])) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  scale_x_log10(labels = comma) +
  facet_wrap(~ radius_m, scales = "free_x") +
  theme_bw() +
  labs(x = "Total conifers (log10)", y = "Hours with PM2.5 >25",
       title = "PM2.5 vs total conifers by radius")
conifp2


nonconifp2<-ggplot(df_nonconif, aes(x = nonconifer, y = .data[[aq_col2]])) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  scale_x_log10(labels = comma) +
  facet_wrap(~ radius_m, scales = "free_x") +
  theme_bw() +
  labs(x = "Total nonconifers (log10)", y = "Hours with PM2.5>25",
       title = "PM2.5 vs total nonconifers by radius")
nonconifp2
# of features (=number of trees  i think)
numtreesps2<-ggplot(df_feat, aes(x = nfeat, y = .data[[aq_col]])) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  scale_x_log10(labels = comma) +
  facet_wrap(~ radius_m, scales = "free_x") +
  theme_bw() +
  labs(x = "Number of features (log10)", y = "Hours with PM2.5>25",
       title = "PM2.5 vs number of trees by radius")
numtreesps2

#fit models to test and compare across lc variables

#Average PM2.5 models
pm2.5_5m<-lm(pm2.5est_sept~mean_treeht_ft_5m, data=canaq0)
pm2.5_10m<-lm(pm2.5est_sept~mean_treeht_ft_10m, data=canaq0)
pm2.5_25m<-lm(pm2.5est_sept~mean_treeht_ft_25m, data=canaq0)
pm2.5_50m<-lm(pm2.5est_sept~mean_treeht_ft_50m, data=canaq0)
pm2.5_100m<-lm(pm2.5est_sept~mean_treeht_ft_100m, data=canaq0)
pm2.5_200m<-lm(pm2.5est_sept~mean_treeht_ft_200m, data=canaq0)
pm2.5_400m<-lm(pm2.5est_sept~mean_treeht_ft_400m, data=canaq0)
pm2.5_800m<-lm(pm2.5est_sept~mean_treeht_ft_800m, data=canaq0)



eff<-c(fixef(sumTminr2.10m)[2],fixef(sumTminr2.20m)[2],fixef(sumTminr2.30m)[2],fixef(sumTminr2.40m)[2],fixef(sumTminr2.50m)[2],
       fixef(sumTminmed.10m)[2],fixef(sumTminmed.20m)[2],fixef(sumTminmed.30m)[2],fixef(sumTminmed.40m)[2],fixef(sumTminmed.50m)[2],
       fixef(sumTminfin.10m)[2],fixef(sumTminfin.20m)[2],fixef(sumTminfin.30m)[2],fixef(sumTminfin.40m)[2],fixef(sumTminfin.50m)[2])
#To make map of loggers by their temperatures, save a csv file with tmax and tmin on hottest days in jun and jult:
ps<-c(Anova(sumTminr2.10m)[1,3],Anova(sumTminr2.20m)[1,3],Anova(sumTminr2.30m)[1,3],Anova(sumTminr2.40m)[1,3],Anova(sumTminr2.50m)[1,3],
      Anova(sumTminmed.10m)[1,3],Anova(sumTminmed.20m)[1,3],Anova(sumTminmed.30m)[1,3],Anova(sumTminmed.40m)[1,3],Anova(sumTminmed.50m)[1,3],
      Anova(sumTminfin.10m)[1,3],Anova(sumTminfin.20m)[1,3],Anova(sumTminfin.30m)[1,3],Anova(sumTminfin.40m)[1,3],Anova(sumTminfin.50m)[1,3])

vegcols=c(rep("darkgreen", times=5),rep("springgreen4",times=5),rep("springgreen", times=5))
vegcols2<-vegcols
vegcols2[which(ps>0.1)]<-"white"

png(file="figs/vegeffects.png",width =3000, height =1500 ,res =300)
x<-barplot(eff, col=vegcols2, border=vegcols, lwd=2,ylim=c(-0.1,0.1), 
           cex.lab=1.3,cex.axis=1.2,cex.names=1.2,
           ylab="Effect on minimum temperature",xlab="Distance (m)",names.arg=rep(c("10","20","30","40","50"), times=3))
error<-c(summary(sumTminr2.10m)$coef[2,2],summary(sumTminr2.20m)$coef[2,2],summary(sumTminr2.30m)$coef[2,2],summary(sumTminr2.40m)$coef[2,2],summary(sumTminr2.50m)$coef[2,2],
         summary(sumTminmed.10m)$coef[2,2],summary(sumTminmed.20m)$coef[2,2],summary(sumTminmed.30m)$coef[2,2],summary(sumTminmed.40m)$coef[2,2],summary(sumTminmed.50m)$coef[2,2],
         summary(sumTminfin.10m)$coef[2,2],summary(sumTminfin.20m)$coef[2,2],summary(sumTminfin.30m)$coef[2,2],summary(sumTminfin.40m)$coef[2,2],summary(sumTminfin.50m)$coef[2,2])

for(i in 1:length(eff)){
  arrows(x[i],eff[i]+error[i],x[i],eff[i]-error[i], code=3, angle=90, length=0.05,  lwd=2)
}
axis(side=1,at=c(3,9,15), labels=c("Coarse (Trees)","Medium (Shrubs)","Fine (Grass)"), line=3,cex=1.3, lty=0)
abline(h=0)
dev.off() 


