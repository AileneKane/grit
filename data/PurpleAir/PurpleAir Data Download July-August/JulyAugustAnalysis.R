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

##reading in data##
setwd("~/Documents/GitHub/grit/data/PurpleAir//PurpleAir Data Download July-August")
PA_locs<-read.csv("PA_locations.csv")


PA_7384<-read.csv("7384 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_7384_jul<- PA_7384%>% slice(1:1488) 
PA_7384_aug<-PA_7384%>% slice(1489:2975)
avg_jul_7384 <- mean(PA_7384_jul$pm2.5_atm)
avg_aug_7384 <- mean(PA_7384_aug$pm2.5_atm)

PA_9732<-read.csv("9732 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_9732_jul<- PA_9732%>% slice(1:1488) 
PA_9732_aug<-PA_9732%>% slice(1489:2932)
avg_jul_9732 <- mean(PA_9732_jul$pm2.5_atm)
avg_aug_9732 <- mean(PA_9732_aug$pm2.5_atm)

PA_15203<-read.csv("15203 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_15203_jul<- PA_15203%>% slice(1:1488) 
PA_15203_aug<-PA_15203%>% slice(1489:2977)
avg_jul_15203 <- mean(PA_15203_jul$pm2.5_atm)
avg_aug_15203<- mean(PA_15203_aug$pm2.5_atm)

PA_17663<-read.csv("17663 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_17663_jul<- PA_17663%>% slice(1:1488) 
PA_17663_aug<-PA_17663%>% slice(1489:2977)
avg_jul_17663 <- mean(PA_17663_jul$pm2.5_atm)
avg_aug_17663<- mean(PA_15203_aug$pm2.5_atm)


PA_50485<-read.csv("50485 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_50485_jul<- PA_50485%>% slice(1:1488) 
PA_50485_aug<-PA_50485%>% slice(1489:2977)
avg_jul_50485<-mean(PA_50485_jul$pm2.5_atm)
avg_aug_50485<-mean(PA_50485_aug$pm2.5_atm)


PA_51969<-read.csv("51969 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_51969_jul<- PA_51969%>% slice(1:1488) 
PA_51969_aug<-PA_51969%>% slice(1489:2976)
avg_jul_51969<-mean(PA_51969_jul$pm2.5_atm)
avg_aug_51969<-mean(PA_51969_aug$pm2.5_atm)

PA_94745<-read.csv("94745 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_94745_jul<- PA_94745%>% slice(1:197) #few july obsv
PA_94745_aug<-PA_94745%>% slice(198:1685)
avg_jul_94745<-mean(PA_94745_jul$pm2.5_atm)
avg_aug_94745<-mean(PA_94745_aug$pm2.5_atm)

PA_98105<-read.csv("98105 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_98105_jul<- PA_98105%>% slice(1:1488) 
PA_98105_aug<-PA_98105%>% slice(1489:2976)
avg_jul_98105<-mean(PA_98105_jul$pm2.5_atm)
avg_aug_98105<-mean(PA_98105_aug$pm2.5_atm)

PA_136172<-read.csv("136172 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_136172_jul<- PA_136172%>% slice(1:1485) 
PA_136172_aug<-PA_136172%>% slice(1486:2970)
avg_jul_136172<-mean(PA_136172_jul$pm2.5_atm)
avg_aug_136172<-mean(PA_136172_aug$pm2.5_atm)

#only has August data
#PA_154963<-read.csv("154963 2024-07-01 2024-09-01 30-Minute Average.csv")
#PA_136172_jul<- PA_136172%>% slice(1:1485) 
#PA_136172_aug<-PA_136172%>% slice(1486:2970)
#avg_jul_136172<-mean(PA_136172_jul$pm2.5_atm)
#avg_aug_136172<-mean(PA_136172_aug$pm2.5_atm)

PA_156499<-read.csv("156499 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_156499_jul<- PA_156499%>% slice(1:1485) 
PA_156499_aug<-PA_156499%>% slice(1486:2973)
avg_jul_156499<-mean(PA_156499_jul$pm2.5_atm)
avg_aug_156499<-mean(PA_156499_aug$pm2.5_atm)

PA_167065<-read.csv("167065 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_167065_jul<- PA_167065%>% slice(1:1485) 
PA_167065_aug<-PA_167065%>% slice(1486:2973)
avg_jul_167065<-mean(PA_167065_jul$pm2.5_atm)
avg_aug_167065<-mean(PA_167065_aug$pm2.5_atm)


PA_167413<-read.csv("167413 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_167413_jul<- PA_167413%>% slice(1:1485) 
PA_167413_aug<-PA_167413%>% slice(1486:2970)
avg_jul_167413<-mean(PA_167413_jul$pm2.5_atm)
avg_aug_167413<-mean(PA_167413_aug$pm2.5_atm)

PA_171177<-read.csv("171177 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_171177_jul<- PA_171177%>% slice(1:1488) 
PA_171177_aug<-PA_171177%>% slice(1489:2976)
avg_jul_171177<-mean(PA_171177_jul$pm2.5_atm)
avg_aug_171177<-mean(PA_171177_aug$pm2.5_atm)

PA_171217<-read.csv("171217 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_171217_jul<- PA_171217%>% slice(1:1488) 
PA_171217_aug<-PA_171217%>% slice(1489:2976)
avg_jul_171217<-mean(PA_171177_jul$pm2.5_atm)
avg_aug_171177<-mean(PA_171177_aug$pm2.5_atm)

PA_173501<-read.csv("173501 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_173501_jul<- PA_173501%>% slice(1:1485) 
PA_173501_aug<-PA_173501%>% slice(1486:2822)
avg_jul_173501<-mean(PA_173501_jul$pm2.5_atm)
avg_aug_173501<-mean(PA_173501_aug$pm2.5_atm)

PA_174613<-read.csv("174613 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_174613_jul<- PA_174613%>% slice(1:1472) 
PA_174613_aug<-PA_174613%>% slice(1473:2960)
avg_jul_174613<-mean(PA_174613_jul$pm2.5_atm)
avg_aug_174613<-mean(PA_174613_aug$pm2.5_atm)

PA_177979<-read.csv("177979 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_174613_jul<- PA_174613%>% slice(1:1472) 
PA_174613_aug<-PA_174613%>% slice(1473:2960)
avg_jul_174613<-mean(PA_174613_jul$pm2.5_atm)
avg_aug_174613<-mean(PA_174613_aug$pm2.5_atm)

PA_183851<-read.csv("183851 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_183851_jul<- PA_183851%>% slice(1:1487) 
PA_183851_aug<-PA_183851%>% slice(1488:2157)
avg_jul_183851<-mean(PA_183851_jul$pm2.5_atm)
avg_aug_183851<-mean(PA_183851_aug$pm2.5_atm)

PA_183853<-read.csv("183853 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_183853_jul<- PA_183853%>% slice(1:1385) 
PA_183853_aug<-PA_183853%>% slice(1386:2873)
avg_jul_183853<-mean(PA_183853_jul$pm2.5_atm)
avg_aug_183853<-mean(PA_183853_aug$pm2.5_atm)

PA_183863<-read.csv("183863 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_183863_jul<- PA_183863%>% slice(1:1488) 
PA_183863_aug<-PA_183863%>% slice(1489:2976)
avg_jul_183863<-mean(PA_183863_jul$pm2.5_atm)
avg_aug_183863<-mean(PA_183863_aug$pm2.5_atm)

PA_183883<-read.csv("183883 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_183883_jul<- PA_183883%>% slice(1:1488) 
PA_183883_aug<-PA_183883%>% slice(1489:2976)
avg_jul_183883<-mean(PA_183883_jul$pm2.5_atm)
avg_aug_183883<-mean(PA_183883_aug$pm2.5_atm)

PA_185377<-read.csv("185377 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_185377_jul<- PA_185377%>% slice(1:1464) 
PA_185377_aug<-PA_185377%>% slice(1465:2944)
avg_jul_185377<-mean(PA_185377_jul$pm2.5_atm)
avg_aug_185377<-mean(PA_185377_aug$pm2.5_atm)

PA_190605<-read.csv("190605 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_190605_jul<- PA_190605%>% slice(1:1199) 
PA_190605_aug<-PA_190605%>% slice(1200:2177)
avg_jul_190605<-mean(PA_190605_jul$pm2.5_atm)
avg_aug_190605<-mean(PA_190605_aug$pm2.5_atm)

PA_192199<-read.csv("192199 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_192199_jul<- PA_192199%>% slice(1:1488) 
PA_192199_aug<-PA_192199%>% slice(1489:2976)
avg_jul_192199<-mean(PA_192199_jul$pm2.5_atm)
avg_aug_192199<-mean(PA_192199_aug$pm2.5_atm)

PA_194473<-read.csv("194473 2024-07-01 2024-09-01 30-Minute Average.csv")
PA_194473_jul<- PA_194473%>% slice(1:1488) 
PA_194473_aug<-PA_194473%>% slice(1489:2976)
avg_jul_194473<-mean(PA_194473_jul$pm2.5_atm)
avg_aug_194473<-mean(PA_194473_aug$pm2.5_atm)

pm2.5_jul<- c("2.116019","5.82093","5.14331","6.492079","4.611392",
              "4.251417","8.076406","0.7244946","5.797343","7.325423",
              "7.322094","8.075643","11.56277","5.243734","4.292885",
              "3.012434","4.700846","5.214288","4.842592","3.01647",
              "5.341509","3.577209")
pm2.5_aug <-c("4.18961616161616","7.51149899261249","7.7522439516129","7.47356821236559","7.41073501683502",
              "7.11229905913979","8.65268511593119","0.773956989247312","7.51149899261249","8.22910223880597",
              "8.43903696236559","8.42751142473118","10.9105114864865","9.46562116564417","7.14880309139785",
              "6.58398588709677","7.51734381701215","8.12117809139785","6.27627774041695","7.97328864247312",
              "7.23157860110803","5.7394435483871")

sensor<-c("136172","15203","156499","167065","167413",
          "171177","173501","174613","17663","183851",
          "183853","183883","185377","190605","192199",
          "194473","50485","51969","7384","94745",
          "9732","98105")
pm2.5 <- data.frame(sensor_index= sensor, pm2.5_jul= pm2.5_jul, pm2.5_aug=pm2.5_aug)


can_cov <- read.csv("grit_aq_lc_jul_aug.csv")


result <- merge(pm2.5, can_cov, by = "sensor_index", all.x = TRUE)
result$pm2.5_jul<-as.numeric(result$pm2.5_jul)
result$pm2.5_aug <- as.numeric(result$pm2.5_aug)



mod_jul<-lm(pm2.5_jul~cancov.40m, data=result)
summary(mod_jul)
mod_aug<-lm(pm2.5_aug~cancov.40m, data=result)

x <- ggplot(result, aes(x = cancov.40m)) +
  geom_point(aes(y = pm2.5_aug, color = "August")) +
  geom_point(aes(y = pm2.5_jul, color = "July")) +
  geom_abline(intercept = coef(mod_jul)[1], slope = coef(mod_jul)[2], color = "black", linetype = "dashed")+
  geom_abline(intercept = coef(mod_aug)[1], slope = coef(mod_aug)[2], color = "blue", linetype = "dashed") +
  labs(y = "PM2.5", color = "Month") +
  theme_minimal() +
  scale_color_manual(values = c("August" = "blue", "July" = "black"))

sq1_prop40 <- x + geom_abline(intercept = coef(mod)[1], slope = coef(mod)[2], color = "blue")
