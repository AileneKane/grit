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
candat<-read.csv("output/alltree_and_impervious_within_buffers.csv", header=TRUE)

candat$total_area_m2<-pi*(candat$radius_m*candat$radius_m)

candat$imperv_perc<-(candat$total_imp/candat$total_area_m2)*100
#impervious numbers are very large- something is weird
candat$canopy_perc<-(candat$total_canopy/candat$total_area_m2)*100

# convert candat to wide format
can_wide <- candat %>%
  pivot_wider(
    id_cols = ID,
    names_from = radius_m,
    values_from = c(canopy_perc,mean_treeht_ft, total_conifers, total_nonconifer, n_trees),
    names_glue = "{.value}_{radius_m}m"
  )

colnames(can_wide)[1]<-"Purple.Air.Name"


#merge locpm2.5 with can_wide
canaq<-left_join(locpm2.5hrs,can_wide)

#remove rows with NAs for pm.5
canaq<-canaq[!is.na(canaq$pm2.5est_sept),]
unique(canaq$n_trees_200m)
unique(canaq$total_canopy_200m)

#NAs in average height, number of conifers, etc should be 0s

canaq0 <- canaq %>%
  mutate(across(11:50, ~ replace_na(., 0)))

#one plot with all relationships between air quality and tree height, number of trees, etc

# ---- SET THESE ----
aq_col <- "pm2.5est_jan"   # <-- change to your air quality column name
id_col <- "Purple.Air.Name"            # <-- your site/sensor id column

df_plot <- canaq0 %>%
  pivot_longer(
    cols = matches("^(canopy_perc|mean_treeht_ft|total_conifers|total_nonconifer|n_trees)_\\d+m$"),
    names_to = c("metric", "radius_m"),
    names_pattern = "^(canopy_perc|mean_treeht_ft|total_conifers|total_nonconifer|n_trees)_(\\d+)m$",
    values_to = "x"
  ) %>%
  mutate(
    radius_m = as.integer(radius_m),
    metric = dplyr::recode(metric,
                    canopy_perc = "Canopy Cover (%)",
                    mean_treeht_ft = "Mean tree height (ft)",
                    total_conifers = "Total conifers",
                    total_conifers = "Total nonconifers",
                    n_trees     = "Number of trees")
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
    cols = matches("^(canopy_perc|mean_treeht_ft|total_conifers|total_nonconifer|n_trees)_\\d+m$"),
    names_to = c("metric", "radius_m"),
    names_pattern = "^(canopy_perc|mean_treeht_ft|total_conifers|total_nonconifer|n_trees)_(\\d+)m$",
    values_to = "x"
  ) %>%
  mutate(
    radius_m = as.integer(radius_m),
    metric = dplyr::recode(metric,
                           canopy_perc = "Canopy Cover (%)",
                           mean_treeht_ft = "Mean tree height (ft)",
                           total_conifers = "Total conifers",
                           total_conifers = "Total nonconifers",
                           n_trees     = "Number of trees")
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


#now as separate plots for each metric

# Choose air quality outcome column
aq_col <- "pm2.5est_sept"   # change if you want hrsabove25est, etc.

# Choose which metrics you want to show (edit to match your df_plot$metric values)
metrics_keep <- c("Canopy Cover (%)","Mean tree height (ft)", "Total conifers", "total_nonconifer","Number of trees")

df_sub <- df_plot %>%
  filter(metric %in% metrics_keep) %>%
  mutate(
    radius_m = as.factor(radius_m),
    metric   = factor(metric, levels = metrics_keep)
  )

ggplot(df_sub, aes(x = x, y = .data[[aq_col]])) +
  geom_point(alpha = 0.6, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9, color = "steelblue") +
  facet_grid(metric ~ radius_m, scales = "free_x") +
  labs(
    x = "Predictor value (x)",
    y = aq_col,
    title = paste(aq_col, "vs tree metrics across buffer radii"),
    subtitle = "Rows = metric; Columns = buffer radius"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "grey95"),
    panel.grid.minor = element_blank()
  )


plot_one_metric <- function(df, metric_name, aq_col = "pm2.5est_sept") {
  df %>%
    filter(metric == metric_name) %>%
    ggplot(aes(x = x, y = .data[[aq_col]])) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 0.9, color = "steelblue") +
    facet_wrap(~ radius_m, scales = "free_x", nrow = 1) +
    labs(
      x = metric_name,
      y = aq_col,
      title = paste(aq_col, "vs", metric_name, "by buffer radius")
    ) +
    theme_bw(base_size = 12)
}

plot_one_metric_jan <- function(df, metric_name, aq_col = "pm2.5est_jan") {
  df %>%
    filter(metric == metric_name) %>%
    ggplot(aes(x = x, y = .data[[aq_col]])) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 0.9, color = "steelblue") +
    facet_wrap(~ radius_m, scales = "free_x", nrow = 1) +
    labs(
      x = metric_name,
      y = aq_col,
      title = paste(aq_col, "vs", metric_name, "by buffer radius")
    ) +
    theme_bw(base_size = 12)
}

p_cancov <- plot_one_metric_jan(df_plot, "Canopy Cover (%)")
p_treeht <- plot_one_metric_jan(df_plot, "Mean tree height (ft)")
p_nonconif <- plot_one_metric_jan(df_plot, "total_nonconifer")
p_conif  <- plot_one_metric_jan(df_plot, "Total conifers")
p_totaltrees  <- plot_one_metric_jan(df_plot, "Number of trees")

p_cancov
p_treeht
p_nonconif
p_conif
p_totaltrees

ggsave("PurpleAir figs/tree_height_jan_plot.png", p_treeht,
       width = 18, height = 4, dpi = 300)
ggsave("PurpleAir figs/tree_height_jan_plot.pdf", p_treeht,
       width = 18, height = 4)

ggsave("PurpleAir figs/pm25vstotaltrees_jan_plot.png", p_totaltrees,
       width = 18, height = 4, dpi = 300)
ggsave("PurpleAir figs/pm25vstotaltrees_jan_plot.pdf", p_totaltrees,
       width = 18, height = 4)
ggsave("PurpleAir figs/pm25vscancov_jan_plot.png", p_cancov,
       width = 18, height = 4, dpi = 300)
ggsave("PurpleAir figs/pm25vscancov_jan_plot.pdf", p_cancov,
       width = 18, height = 4)

p_cancov <- plot_one_metric(df_plot, "Canopy Cover (%)")
p_treeht <- plot_one_metric(df_plot, "Mean tree height (ft)")
p_nonconif <- plot_one_metric(df_plot, "total_nonconifer")
p_conif  <- plot_one_metric(df_plot, "Total conifers")
p_totaltrees  <- plot_one_metric(df_plot, "Number of trees")

p_cancov
p_treeht
p_nonconif
p_conif
p_totaltrees

ggsave("PurpleAir figs/tree_height_sept_plot.png", p_treeht,
       width = 18, height = 4, dpi = 300)
ggsave("PurpleAir figs/tree_height_sept_plot.pdf", p_treeht,
       width = 18, height = 4)

ggsave("PurpleAir figs/pm25vstotaltrees_sept_plot.png", p_totaltrees,
       width = 18, height = 4, dpi = 300)
ggsave("PurpleAir figs/pm25vstotaltrees_sept_plot.pdf", p_totaltrees,
       width = 18, height = 4)
ggsave("PurpleAir figs/pm25vscancov_sept_plot.png", p_cancov,
       width = 18, height = 4, dpi = 300)
ggsave("PurpleAir figs/pm25vscancov_sept_plot.pdf", p_cancov,
       width = 18, height = 4)




#fit models to test and compare across lc variables

#Average PM2.5 models vs tree height
pm2.5_5m<-lm(pm2.5est_sept~mean_treeht_ft_5m, data=canaq0)
pm2.5_10m<-lm(pm2.5est_sept~mean_treeht_ft_10m, data=canaq0)
pm2.5_25m<-lm(pm2.5est_sept~mean_treeht_ft_25m, data=canaq0)
pm2.5_50m<-lm(pm2.5est_sept~mean_treeht_ft_50m, data=canaq0)
pm2.5_100m<-lm(pm2.5est_sept~mean_treeht_ft_100m, data=canaq0)
pm2.5_200m<-lm(pm2.5est_sept~mean_treeht_ft_200m, data=canaq0)
pm2.5_400m<-lm(pm2.5est_sept~mean_treeht_ft_400m, data=canaq0)
pm2.5_800m<-lm(pm2.5est_sept~mean_treeht_ft_800m, data=canaq0)

#Average PM2.5 models vs tree height for jan
janpm2.5_5m<-lm(pm2.5est_jan~mean_treeht_ft_5m, data=canaq0)
janpm2.5_10m<-lm(pm2.5est_jan~mean_treeht_ft_10m, data=canaq0)
janpm2.5_25m<-lm(pm2.5est_jan~mean_treeht_ft_25m, data=canaq0)
janpm2.5_50m<-lm(pm2.5est_jan~mean_treeht_ft_50m, data=canaq0)
janpm2.5_100m<-lm(pm2.5est_jan~mean_treeht_ft_100m, data=canaq0)
janpm2.5_200m<-lm(pm2.5est_jan~mean_treeht_ft_200m, data=canaq0)
janpm2.5_400m<-lm(pm2.5est_jan~mean_treeht_ft_400m, data=canaq0)
janpm2.5_800m<-lm(pm2.5est_jan~mean_treeht_ft_800m, data=canaq0)

#which models have p<0.10?

#hours above PM2.5>25 models vs tree height
hrs_5m<-lm(hrsabove25est~mean_treeht_ft_5m, data=canaq0)
hrs_10m<-lm(hrsabove25est~mean_treeht_ft_10m, data=canaq0)
hrs_25m<-lm(hrsabove25est~mean_treeht_ft_25m, data=canaq0)
hrs_50m<-lm(hrsabove25est~mean_treeht_ft_50m, data=canaq0)
hrs_100m<-lm(hrsabove25est~mean_treeht_ft_100m, data=canaq0)
hrs_200m<-lm(hrsabove25est~mean_treeht_ft_200m, data=canaq0)
hrs_400m<-lm(hrsabove25est~mean_treeht_ft_400m, data=canaq0)
hrs_800m<-lm(hrsabove25est~mean_treeht_ft_800m, data=canaq0)

#no sig relationships for any of the above...

#############################################
# Average PM2.5 models vs total_conifers_5m #
#############################################
pm2.5_5m_ncon<-lm(pm2.5est_sept~total_conifers_5m, data=canaq0)
pm2.5_10m_ncon<-lm(pm2.5est_sept~total_conifers_10m, data=canaq0)
pm2.5_25m_ncon<-lm(pm2.5est_sept~total_conifers_25m, data=canaq0)
pm2.5_50m_ncon<-lm(pm2.5est_sept~total_conifers_50m, data=canaq0)
pm2.5_100m_ncon<-lm(pm2.5est_sept~total_conifers_100m, data=canaq0)
pm2.5_200m_ncon<-lm(pm2.5est_sept~total_conifers_200m, data=canaq0)
pm2.5_400m_ncon<-lm(pm2.5est_sept~total_conifers_400m, data=canaq0)
pm2.5_800m_ncon<-lm(pm2.5est_sept~total_conifers_800m, data=canaq0)

summary(pm2.5_25m_ncon)#sig neg
summary(pm2.5_50m_ncon)#not sig
summary(pm2.5_100m_ncon)#weakly sig neg
summary(pm2.5_200m_ncon)#sig neg
summary(pm2.5_400m_ncon)#sig neg

janpm2.5_5m_ncon<-lm(pm2.5est_sept~total_conifers_5m, data=canaq0)
janpm2.5_10m_ncon<-lm(pm2.5est_sept~total_conifers_10m, data=canaq0)
janpm2.5_25m_ncon<-lm(pm2.5est_jan~total_conifers_25m, data=canaq0)
janpm2.5_50m_ncon<-lm(pm2.5est_jan~total_conifers_50m, data=canaq0)
janpm2.5_100m_ncon<-lm(pm2.5est_jan~total_conifers_100m, data=canaq0)
janpm2.5_200m_ncon<-lm(pm2.5est_jan~total_conifers_200m, data=canaq0)
janpm2.5_400m_ncon<-lm(pm2.5est_jan~total_conifers_400m, data=canaq0)
janpm2.5_800m_ncon<-lm(pm2.5est_jan~total_conifers_800m, data=canaq0)

summary(janpm2.5_25m_ncon)#weakly sig neg
summary(janpm2.5_50m_ncon)#not sig 
summary(janpm2.5_100m_ncon)#not sig 
summary(janpm2.5_200m_ncon)#not sig 
summary(janpm2.5_400m_ncon)#not sig 

####################################################
# hours above PM2.5>25 models vs total_conifers_5m #
####################################################
hrs_5m_ncon<-lm(hrsabove25est~total_conifers_5m, data=canaq0)
hrs_10m_ncon<-lm(hrsabove25est~total_conifers_10m, data=canaq0)
hrs_25m_ncon<-lm(hrsabove25est~total_conifers_25m, data=canaq0)
hrs_50m_ncon<-lm(hrsabove25est~total_conifers_50m, data=canaq0)
hrs_100m_ncon<-lm(hrsabove25est~total_conifers_100m, data=canaq0)
hrs_200m_ncon<-lm(hrsabove25est~total_conifers_200m, data=canaq0)
hrs_400m_ncon<-lm(hrsabove25est~total_conifers_400m, data=canaq0)
hrs_800m_ncon<-lm(hrsabove25est~total_conifers_800m, data=canaq0)

#############################################
# Average PM2.5 models vs total nonconifers #
#############################################
pm2.5_5m_nnoncon<-lm(pm2.5est_sept~total_nonconifer_5m, data=canaq0)
pm2.5_10m_nnoncon<-lm(pm2.5est_sept~total_nonconifer_10m, data=canaq0)
pm2.5_25m_nnoncon<-lm(pm2.5est_sept~total_nonconifer_25m, data=canaq0)
pm2.5_50m_nnoncon<-lm(pm2.5est_sept~total_nonconifer_50m, data=canaq0)
pm2.5_100m_nnoncon<-lm(pm2.5est_sept~total_nonconifer_100m, data=canaq0)
pm2.5_200m_nnoncon<-lm(pm2.5est_sept~total_nonconifer_200m, data=canaq0)
pm2.5_400m_nnoncon<-lm(pm2.5est_sept~total_nonconifer_400m, data=canaq0)
pm2.5_800m_nnoncon<-lm(pm2.5est_sept~total_nonconifer_800m, data=canaq0)

summary(pm2.5_25m_nnoncon)#not sig
summary(pm2.5_50m_nnoncon)# sig neg
summary(pm2.5_100m_nnoncon)# sig neg
summary(pm2.5_200m_nnoncon)#sig neg
summary(pm2.5_400m_nnoncon)#sig neg

janpm2.5_5m_nnoncon<-lm(pm2.5est_jan~total_nonconifer_5m, data=canaq0)
janpm2.5_10m_nnoncon<-lm(pm2.5est_jan~total_nonconifer_10m, data=canaq0)
janpm2.5_25m_nnoncon<-lm(pm2.5est_jan~total_nonconifer_25m, data=canaq0)
janpm2.5_50m_nnoncon<-lm(pm2.5est_jan~total_nonconifer_50m, data=canaq0)
janpm2.5_100m_nnoncon<-lm(pm2.5est_sept~total_nonconifer_100m, data=canaq0)
janpm2.5_200m_nnoncon<-lm(pm2.5est_sept~total_nonconifer_200m, data=canaq0)
janpm2.5_400m_nnoncon<-lm(pm2.5est_sept~total_nonconifer_400m, data=canaq0)
janpm2.5_800m_nnoncon<-lm(pm2.5est_sept~total_nonconifer_800m, data=canaq0)

summary(janpm2.5_25m_nnoncon)#not sig
summary(janpm2.5_50m_nnoncon)#weakly sig neg
summary(janpm2.5_100m_nnoncon)# sig neg
summary(janpm2.5_200m_nnoncon)#sig neg
summary(janpm2.5_400m_nnoncon)#sig neg


####################################################
# hours above PM2.5>25 models vs total nonconifers #
####################################################
hrs_5m_nnoncon<-lm(hrsabove25est~total_nonconifer_5m, data=canaq0)
hrs_10m_nnoncon<-lm(hrsabove25est~total_nonconifer_10m, data=canaq0)
hrs_25m_nnoncon<-lm(hrsabove25est~total_nonconifer_25m, data=canaq0)
hrs_50m_nnoncon<-lm(hrsabove25est~total_nonconifer_50m, data=canaq0)
hrs_100m_nnoncon<-lm(hrsabove25est~total_nonconifer_100m, data=canaq0)
hrs_200m_nnoncon<-lm(hrsabove25est~total_nonconifer_200m, data=canaq0)
hrs_400m_nnoncon<-lm(hrsabove25est~total_nonconifer_400m, data=canaq0)
hrs_800m_nnoncon<-lm(hrsabove25est~total_nonconifer_800m, data=canaq0)
#no significant relationships

################################################
# Average PM2.5 models vs number of trees #
################################################
pm2.5_5m_tottrees<-lm(pm2.5est_sept~n_trees_5m, data=canaq0)
pm2.5_10m_tottrees<-lm(pm2.5est_sept~n_trees_10m, data=canaq0)
pm2.5_25m_tottrees<-lm(pm2.5est_sept~n_trees_25m, data=canaq0)
pm2.5_50m_tottrees<-lm(pm2.5est_sept~n_trees_50m, data=canaq0)
pm2.5_100m_tottrees<-lm(pm2.5est_sept~n_trees_100m, data=canaq0)
pm2.5_200m_tottrees<-lm(pm2.5est_sept~n_trees_200m, data=canaq0)
pm2.5_400m_tottrees<-lm(pm2.5est_sept~n_trees_400m, data=canaq0)
pm2.5_800m_tottrees<-lm(pm2.5est_sept~n_trees_800m, data=canaq0)

#summary(pm2.5_5m_tottrees)#notsig, not 10m
summary(pm2.5_10m_tottrees)#not sig
summary(pm2.5_25m_tottrees)#not sig neg
summary(pm2.5_50m_tottrees)# sig neg
summary(pm2.5_100m_tottrees)# sig neg
summary(pm2.5_200m_nnoncon)#sig neg
summary(pm2.5_400m_tottrees)#sig neg
summary(pm2.5_800m_tottrees)#sig neg


janpm2.5_5m_tottrees<-lm(pm2.5est_jan~n_trees_5m, data=canaq0)
janpm2.5_10m_tottrees<-lm(pm2.5est_jan~n_trees_10m, data=canaq0)
janpm2.5_25m_tottrees<-lm(pm2.5est_jan~n_trees_25m, data=canaq0)
janpm2.5_50m_tottrees<-lm(pm2.5est_jan~n_trees_50m, data=canaq0)
janpm2.5_100m_tottrees<-lm(pm2.5est_jan~n_trees_100m, data=canaq0)
janpm2.5_200m_tottrees<-lm(pm2.5est_jan~n_trees_200m, data=canaq0)
janpm2.5_400m_tottrees<-lm(pm2.5est_jan~n_trees_400m, data=canaq0)
janpm2.5_800m_tottrees<-lm(pm2.5est_jan~n_trees_800m, data=canaq0)

summary(pm2.5_5m_tottrees)#not sig, not 10m

summary(janpm2.5_25m_tottrees)#not sig neg
summary(janpm2.5_50m_tottrees)# weakly sig neg
summary(janpm2.5_100m_tottrees)# sig neg
summary(janpm2.5_200m_nnoncon)#sig neg
summary(janpm2.5_400m_tottrees)#sig neg

########################################################
# hours above PM2.5>25 models vs total number of trees #
########################################################
hrs_5m_tottrees<-lm(hrsabove25est~n_trees_5m, data=canaq0)
hrs_10m_tottrees<-lm(hrsabove25est~n_trees_10m, data=canaq0)
hrs_25m_tottrees<-lm(hrsabove25est~n_trees_25m, data=canaq0)
hrs_50m_tottrees<-lm(hrsabove25est~n_trees_50m, data=canaq0)
hrs_100m_tottrees<-lm(hrsabove25est~n_trees_100m, data=canaq0)
hrs_200m_tottrees<-lm(hrsabove25est~n_trees_200m, data=canaq0)
hrs_400m_tottrees<-lm(hrsabove25est~n_trees_400m, data=canaq0)
hrs_800m_tottrees<-lm(hrsabove25est~n_trees_800m, data=canaq0)

#no significant relationships
eff<-c(coef(pm2.5_10m_tottrees)[2],coef(pm2.5_25m_tottrees)[2],coef(pm2.5_50m_tottrees)[2],
       coef(pm2.5_100m_tottrees)[2],coef(pm2.5_200m_tottrees)[2],coef(pm2.5_400m_tottrees)[2],
       coef(pm2.5_800m_tottrees)[2])
#To make map of loggers by their temperatures, save a csv file with tmax and tmin on hottest days in jun and jult:
ps<-c(Anova(pm2.5_10m_tottrees)[1,4],Anova(pm2.5_25m_tottrees)[1,4],Anova(pm2.5_50m_tottrees)[1,4],
      Anova(pm2.5_100m_tottrees)[1,4],Anova(pm2.5_200m_tottrees)[1,4],Anova(pm2.5_400m_tottrees)[1,4],
      Anova(pm2.5_800m_tottrees)[1,4])

cols=c(rep("darkgreen", times=7))
cols[which(ps>0.1)]<-"white"

png(file="PurpleAir figs/tottreeseffects.png",width =3000, height =1500 ,res =300)

x<-barplot(eff, col=cols, border="darkgreen", lwd=2,ylim=c(-0.1,0.1), 
           cex.lab=1.3,cex.axis=1.2,cex.names=1.2,
           ylab="Effect on Average PM 2.5",xlab="Distance (m)",names.arg=c("10","25","50","100","200","400","800"))

error<-c(summary(pm2.5_10m_tottrees)$coef[2,2],summary(pm2.5_25m_tottrees)$coef[2,2],summary(pm2.5_50m_tottrees)$coef[2,2],
         summary(pm2.5_100m_tottrees)$coef[2,2],summary(pm2.5_200m_tottrees)$coef[2,2],summary(pm2.5_400m_tottrees)$coef[2,2],
         summary(pm2.5_800m_tottrees)$coef[2,2])

for(i in 1:length(eff)){
  arrows(x[i],eff[i]+error[i],x[i],eff[i]-error[i], code=3, angle=90, length=0.05,  lwd=2)
}
abline(h=0)

dev.off() 

################################################
# Average PM2.5 models vs canopy cover #
################################################
pm2.5_5m_cancov<-lm(pm2.5est_sept~canopy_perc_5m, data=canaq0)
pm2.5_10m_cancov<-lm(pm2.5est_sept~canopy_perc_10m, data=canaq0)
pm2.5_25m_cancov<-lm(pm2.5est_sept~canopy_perc_25m, data=canaq0)
pm2.5_50m_cancov<-lm(pm2.5est_sept~canopy_perc_50m, data=canaq0)
pm2.5_100m_cancov<-lm(pm2.5est_sept~canopy_perc_100m, data=canaq0)
pm2.5_200m_cancov<-lm(pm2.5est_sept~canopy_perc_200m, data=canaq0)
pm2.5_400m_cancov<-lm(pm2.5est_sept~canopy_perc_400m, data=canaq0)
pm2.5_800m_cancov<-lm(pm2.5est_sept~canopy_perc_800m, data=canaq0)

summary(pm2.5_5m_cancov)#notsig, not 10m
summary(pm2.5_10m_cancov)#not sig
summary(pm2.5_25m_cancov)#not sig neg
summary(pm2.5_50m_cancov)#marginally sig neg
summary(pm2.5_100m_cancov)#sig neg
summary(pm2.5_200m_cancov)#sig neg
summary(pm2.5_400m_cancov)#sig neg
summary(pm2.5_800m_cancov)#sig neg


janpm2.5_5m_cancov<-lm(pm2.5est_jan~canopy_perc_5m, data=canaq0)
janpm2.5_10m_cancov<-lm(pm2.5est_jan~canopy_perc_10m, data=canaq0)
janpm2.5_25m_cancov<-lm(pm2.5est_jan~canopy_perc_25m, data=canaq0)
janpm2.5_50m_cancov<-lm(pm2.5est_jan~canopy_perc_50m, data=canaq0)
janpm2.5_100m_cancov<-lm(pm2.5est_jan~canopy_perc_100m, data=canaq0)
janpm2.5_200m_cancov<-lm(pm2.5est_jan~canopy_perc_200m, data=canaq0)
janpm2.5_400m_cancov<-lm(pm2.5est_jan~canopy_perc_400m, data=canaq0)
janpm2.5_800m_cancov<-lm(pm2.5est_jan~canopy_perc_800m, data=canaq0)

summary(janpm2.5_5m_cancov)#not sig, 
summary(janpm2.5_10m_cancov)#not sig, 

summary(janpm2.5_25m_cancov)#not sig neg
summary(janpm2.5_50m_cancov)# weakly sig neg
summary(janpm2.5_100m_cancov)# weakly sig neg
summary(janpm2.5_200m_cancov)#sig neg
summary(janpm2.5_400m_cancov)#sig neg
summary(janpm2.5_800m_cancov)#weakly sig neg

#plot

  eff<-c(coef(pm2.5_10m_cancov)[2],coef(pm2.5_25m_cancov)[2],coef(pm2.5_50m_cancov)[2],
         coef(pm2.5_100m_cancov)[2],coef(pm2.5_200m_cancov)[2],coef(pm2.5_400m_cancov)[2],
         coef(pm2.5_800m_cancov)[2])
  #To make plot effect sizes
  ps<-c(Anova(pm2.5_10m_cancov)[1,4],Anova(pm2.5_25m_cancov)[1,4],Anova(pm2.5_50m_cancov)[1,4],
        Anova(pm2.5_100m_cancov)[1,4],Anova(pm2.5_200m_cancov)[1,4],Anova(pm2.5_400m_cancov)[1,4],
        Anova(pm2.5_800m_cancov)[1,4])
  
  cols=c(rep("darkgreen", times=7))
  cols[which(ps>0.1)]<-"white"
  png(file="PurpleAir figs/cancovffects.png",width =3000, height =1500 ,res =300)
  
  x<-barplot(eff, col=cols, border="darkgreen", lwd=2,ylim=c(-0.1,0.1), 
             cex.lab=1.3,cex.axis=1.2,cex.names=1.2,
             ylab="Effect on Average PM 2.5",xlab="Distance (m)",names.arg=c("10","25","50","100","200","400","800"))
  
  error<-c(summary(pm2.5_10m_cancov)$coef[2,2],summary(pm2.5_25m_cancov)$coef[2,2],summary(pm2.5_50m_cancov)$coef[2,2],
           summary(pm2.5_100m_cancov)$coef[2,2],summary(pm2.5_200m_cancov)$coef[2,2],summary(pm2.5_400m_cancov)$coef[2,2],
           summary(pm2.5_800m_cancov)$coef[2,2])
  
  for(i in 1:length(eff)){
    arrows(x[i],eff[i]+error[i],x[i],eff[i]-error[i], code=3, angle=90, length=0.05,  lwd=2)
  }
  abline(h=0)
  
  dev.off() 
  
  png(file="PurpleAir figs/septjancancovffects.png",width =5000, height =2000 ,res =300)
  
  par(mfrow = c(1, 2))
  mtext("A) September")
  
  mtext("B) January")
  
  #plot Januaryeffect sizes
  eff<-c(coef(janpm2.5_10m_cancov)[2],coef(janpm2.5_25m_cancov)[2],coef(janpm2.5_50m_cancov)[2],
         coef(janpm2.5_100m_cancov)[2],coef(janpm2.5_200m_cancov)[2],coef(janpm2.5_400m_cancov)[2],
         coef(janpm2.5_800m_cancov)[2])
  
  ps<-c(Anova(janpm2.5_10m_cancov)[1,4],Anova(janpm2.5_25m_cancov)[1,4],Anova(janpm2.5_50m_cancov)[1,4],
        Anova(janpm2.5_100m_cancov)[1,4],Anova(janpm2.5_200m_cancov)[1,4],Anova(janpm2.5_400m_cancov)[1,4],
        Anova(janpm2.5_800m_cancov)[1,4])
  
  cols=c(rep("darkgreen", times=7))
  cols[which(ps>0.1)]<-"white"
  
  x<-barplot(eff, col=cols, border="darkgreen", lwd=2,ylim=c(-0.1,0.1), 
             cex.lab=1.3,cex.axis=1.2,cex.names=1.2,
             ylab="Effect on Average PM 2.5",xlab="Distance (m)",names.arg=c("10","25","50","100","200","400","800"))
  
  error<-c(summary(janpm2.5_10m_cancov)$coef[2,2],summary(janpm2.5_25m_cancov)$coef[2,2],summary(janpm2.5_50m_cancov)$coef[2,2],
           summary(janpm2.5_100m_cancov)$coef[2,2],summary(janpm2.5_200m_cancov)$coef[2,2],summary(janpm2.5_400m_cancov)$coef[2,2],
           summary(janpm2.5_800m_cancov)$coef[2,2])
  
  for(i in 1:length(eff)){
    arrows(x[i],eff[i]+error[i],x[i],eff[i]-error[i], code=3, angle=90, length=0.05,  lwd=2)
  }
  abline(h=0)
  
  dev.off() 
  