#######################################################
### Script to look at GRIT airtemp data ###
################# August 24, 2022 #####################
################ ailene.ettinger@tnc.org ##############
#######################################################

#Currently, this script looks at pulls together all temperature data from from March through August 2022
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries
library(dplyr)
library(lme4)
library(tidyr)
library(car)
library(scales)
library(MuMIn)
library(sjPlot)
library(ggplot2)
library(tidymv)
library(mgcv)
# set working directory
setwd("~/GitHub/grit/analyses")

#Read in data on all temp logger locations
locs<-read.csv("../data/HoboLocations_PoleLocations.csv", header=TRUE)
source("sourced_files/clean_locs.R")

#Read in tree data 
treedat<-read.csv("../data/HoboLocations_TreeData.csv", header=TRUE)

source("sourced_files/summarize_grittreedat.R")
#Read in remote-sensed land cover estimates, derived from stormwater heat map lc data
lc<-read.csv("output/grit_logger_lc.csv", header=TRUE)

#add lc to tree dat
source("sourced_files/combine_lc_grittreedat.R")

#head(sumba)

#Before looking at temperature, look at field-collected canopy vs remote sensed land cover
png("figs/remote_field_cover.png", width=6, height=6, units="in", res=220)
field<-100-as.numeric(locs3$X1FineVeg.10mProp)
rem<-locs3$X3CoarseVeg.10mProp*100

plot(field,rem, pch=16, col="gray",
     xlab="Field Canopy Cover",
     ylab="Remote-sensed Vegetation Cover", 
     main="Remote sensed vs Field Estimates of Tree Cover")
r<-lm(rem~field)
abline(r, )
mtext(paste("r2=",round(summary(r)$r.squared, digits=3), sep=""), side=3, line=-1, adj=0)
dev.off()

#Put together all air temperature data that we have
airtempdat<-NULL
tdir<-"../data/temp_data/"
tfolds<-list.files(tdir)
for(i in tfolds){
  tempdatdir<-paste(tdir,i,sep="")
  tempfiles<-list.files(tempdatdir)
  tempfiles<-tempfiles[which(substr(tempfiles,nchar(tempfiles)-3,nchar(tempfiles))==".csv")]
  for (j in tempfiles){
    dat<-read.csv(paste(tempdatdir,"/",j,sep=""), skip=1,header=TRUE)
    colnames(dat)[2:3]<-c("date.time","airtemp_c")
    dat<-dat[,-1]
    dat$date<-substr(dat$date,1,8)
    dat$time<-substr(dat$date.time,10,20)
    dat$time<-format(strptime(dat$time, "%I:%M:%S %p"), "%H:%M:%S")
    dat$Hobo_SN<-paste(substr(j,1,nchar(j)-4))
    if(substr(j,nchar(j)-5,nchar(j)-5)=="_"){dat$Hobo_SN<-paste(substr(j,1,nchar(j)-6))}
    adat<-subset(dat,select=c(Hobo_SN,date,time, airtemp_c))
    airtempdat<-rbind(airtempdat,adat)
  }
}

dim(airtempdat)
unique(airtempdat$Hobo_SN)  
head(airtempdat)

#now format  blue tooth data
bttempdat<-read.csv("../data/BT_temp_data/AllBTLoggers_start_to_06Dec202_2022_12_06_15_49_34_PST_1.csv", header=TRUE)
#dim(bttempdat)
#colnames(bttempdat)
#bttempdat[1:5,1:3]
bttempdat<-bttempdat[,-1]
colnames(bttempdat)[2:length(colnames(bttempdat))]<-substr(colnames(bttempdat)[2:length(colnames(bttempdat))],50,nchar(colnames(bttempdat)[2:length(colnames(bttempdat))]))
#reshape to long format
bttempdat_long<-gather(bttempdat, logger, airtemp_c, colnames(bttempdat)[2:length(colnames(bttempdat))],factor_key=TRUE)
bttempdat_long$Hobo_SN<-paste("BT",bttempdat_long$logger,sep="")
bttempdat_long$date<-substr(bttempdat_long$Date,1,8)
bttempdat_long$time<-substr(bttempdat_long$Date,10,20)

btdat<-subset(bttempdat_long,select=c("Hobo_SN","date","time","airtemp_c"))
#remove the many, many rows with airtemp=NA
btdat<-btdat[-which(is.na(btdat$airtemp_c)),]
#keep only data on the hour
btdat<-btdat[which(substr(btdat$time,4,8)=="00:00"),]

#now add btdat to airtemp
allairdat<-rbind(airtempdat,btdat)

#merge in locdata
#locdat2 <- locs %>% 
#  select(Hobo_SN, Location, Latitude, Longitude, Elevation, Trees.) %>% 
#  distinct(Location, .keep_all= TRUE)
locdat2 <- subset(locs3, select=c(Hobo_SN, Location, Latitude, Longitude, Elevation, Trees., TotalBA_cm2, PercOpen_Mean,X1FineVeg.10mProp,X2MedVeg.10mProp,X3CoarseVeg.10mProp,XAllImp.10mProp))

alldat<-left_join(allairdat,locdat2, by="Hobo_SN", copy=TRUE)
alldat<-alldat[-(which(is.na(alldat$airtemp_c))),]
alldat<-alldat[-which(alldat$Hobo_SN=="21223113"),]#messed up data!
alldat<-alldat[-which(alldat$airtemp_c>36),]#inaccurate temperature readings

###some serial numbers do not line up (e.g., we ave temp data, but no location matches up to them).
###These are: unique(aldat$Hobo_SN[which(is.na(aldat$Longitude))])
# "21162467"   "21223102"   "21223125"   "BT21302946" "BT21302957" "BT21302964" "BT21302976"
#needs to be corrected but for now just removing these

alldat<-alldat[-(which(is.na(alldat$Longitude))),]


#Merge in tree data
alldat2<-alldat
#check that locations with trees have more bai:
boxplot(as.numeric(alldat2$TotalBA_cm2)~Trees., data=alldat)
#Make some plots
boxplot(as.numeric(airtemp_c)~Trees., data=alldat)
alldat2$airtemp_c<-as.numeric(alldat2$airtemp_c)
alldat2$hour<-as.integer(substr(alldat2$time,1,2))
alldat2$Trees.<-as.factor(alldat2$Trees.)
alldat2$TotalBA_cm2<-as.numeric(alldat2$TotalBA_cm2)
alldat2$TotalBA_m2<-alldat2$TotalBA_cm2/10000
alldat2$Hobo_SN <-as.factor(alldat2$Hobo_SN)
alldat2$month<-as.factor(as.character(substr(alldat2$date,1,2)))
alldat2$year<-as.factor(as.character(paste("20",substr(alldat2$date,7,8), sep="")))
alldat2$day<-0
alldat2$day[alldat2$hour>6 & alldat2$hour<19]<-1
#add column for whether logger has a sheild or not
alldat2$shield<-"YES"
alldat2$shield[alldat$Location=="Wapato Hills 2"]<-"NO"
alldat2$shield[alldat$Location=="Wapato Hills 1"]<-"NO"
alldat2$shield[alldat$Location=="South Tacoma Wetland"]<-"NO"
alldat2$shield[alldat$Location=="South Tacoma Wetland 1"]<-"NO"

juldat<-alldat2[alldat2$month=="07",]
jundat<-alldat2[alldat2$month=="06",]

boxplot(jundat$airtemp_c[jundat$day==1]~jundat$Trees.[jundat$day==1])
boxplot(jundat$airtemp_c[jundat$day==0]~jundat$Trees.[jundat$day==0])
#Which logger recorded the hottest temperature and when was it?
jundat[which(jundat$airtemp_c==max(jundat$airtemp_c)),]
juldat[which(juldat$airtemp_c==max(juldat$airtemp_c)),]

#hottest day of the year:head(jundat[jundat$date=="06/27/22",])
#remove NAs so that we can compare explanatory variables across a single dataset
length(which(is.na(jundat$X6ImpOther.10m)))
jundat<-jundat[-which(is.na(jundat$X1FineVeg.10m)),]
juldat<-juldat[-which(is.na(juldat$X1FineVeg.10m)),]

##################################################
#Fit some models to compare what best explains variation in temp
#################################################
jundat$cc.field<-100-as.numeric(jundat$PercOpen_Mean)
jundat$cc.rem<-jundat$X3CoarseVeg.10mProp*100
jundat$imp<-jundat$XAllImp.10mProp*100

juldat$cc.field<-100-as.numeric(juldat$PercOpen_Mean)
juldat$cc.rem<-juldat$X3CoarseVeg.10mProp*100
juldat$imp<-juldat$XAllImp.10mProp*100

#jundat.nona<-jundat[-which(is.na(jundat$TotalBA_cm2)),]#used this to test how Total BA compared to canopy cover, remote data- it was not as good a predictor
#juldat.nona<-juldat[-which(is.na(juldat$TotalBA_cm2)),]#used this to test how Total BA compared to canopy cover, remote data- it was not as good a predictor

junm1a<-lm(airtemp_c~Trees.*day, data=jundat)
junm1as<-lm(airtemp_c~Trees.*day, data=jundat[jundat$shield=="YES",])
#junm1b<-ldm(airtemp_c~TotalBA_m2*day, data=jundat)
junm1c<-lm(airtemp_c~cc.field*day, data=jundat)
junm1r<-lm(airtemp_c~cc.rem*day, data=jundat)
junm1imp<-lm(airtemp_c~imp*day, data=jundat)
#junm1imp has lowest AIC, followedb y r
summary(junm1r)

junm2a<-lm(airtemp_c~Trees.*hour+Elevation, data=jundat)
#junm2b<-lm(airtemp_c~TotalBA_m2*hour+Elevation, data=jundat)
junm2c<-lm(airtemp_c~cc.field*hour+Elevation, data=jundat)
junm2r<-lm(airtemp_c~cc.rem*hour+Elevation, data=jundat)
junm2imp<-lm(airtemp_c~imp*hour+Elevation, data=jundat)
summary(junm1r)

AIC(junm2a,junm2c,junm2r,junm2imp,junm1a,junm1c,junm1r,junm1imp)

summary(junm2imp)#lowest AIC
summary(junm2r)

#Fit some mixed effects models
junmm1a<-lmer(airtemp_c~Trees.*day + (1|Hobo_SN), data=jundat)
#junmm2b<-lmer(airtemp_c~TotalBA_cm2*hour+ (1|Hobo_SN), data=jundat.nona)
junmm1c<-lmer(airtemp_c~cc.field*day+ (1|Hobo_SN), data=jundat)
junmm1r<-lmer(airtemp_c~cc.rem*day+ (1|Hobo_SN), data=jundat)
junmm1imp<-lmer(airtemp_c~imp*day+ (1|Hobo_SN), data=jundat)
summary(junmm1c)
junmm2a<-lmer(airtemp_c~Trees.*hour+(1|Hobo_SN), data=jundat)
junmm2c<-lmer(airtemp_c~cc.field*hour+(1|Hobo_SN), data=jundat)
junmm2r<-lmer(airtemp_c~cc.rem*hour+(1|Hobo_SN), data=jundat)
junmm2imp<-lmer(airtemp_c~imp*hour+ (1|Hobo_SN), data=jundat)
junmm3a<-lmer(airtemp_c~Trees.*hour+Elevation +(1|Hobo_SN), data=jundat)
junmm3c<-lmer(airtemp_c~cc.field*hour+Elevation+(1|Hobo_SN), data=jundat)
junmm3r<-lmer(airtemp_c~cc.rem*hour+Elevation +(1|Hobo_SN), data=jundat)
junmm3imp<-lmer(airtemp_c~imp*hour+Elevation + (1|Hobo_SN), data=jundat)

junmm4a<-lmer(airtemp_c~Trees.+day+Trees.:day+Elevation:Trees. + (1|Hobo_SN), data=jundat)
junmm4c<-lmer(airtemp_c~cc.field+day+cc.field:day+Elevation:cc.field + (1|Hobo_SN), data=jundat)
junmm4r<-lmer(airtemp_c~cc.rem+day+cc.rem:day+Elevation:cc.rem + (1|Hobo_SN), data=jundat)
junmm4imp<-lmer(airtemp_c~imp+day+imp:day+Elevation:imp + (1|Hobo_SN), data=jundat)

#Add hour and interactions
junmm5a<-lmer(airtemp_c~Trees.+hour+day+Trees.:hour+Trees.:day+Elevation +day:hour+ (1|Hobo_SN), data=jundat)
junmm5c<-lmer(airtemp_c~cc.field+hour+day+cc.field:hour+cc.field:day+Elevation +day:hour+ (1|Hobo_SN), data=jundat)
junmm5r<-lmer(airtemp_c~cc.rem+hour+day+cc.rem:hour+cc.rem:day+Elevation +day:hour+ (1|Hobo_SN), data=jundat)
junmm5imp<-lmer(airtemp_c~imp+hour+day+imp:hour+imp:day+Elevation +day:hour+ (1|Hobo_SN), data=jundat)

AIC(junmm1a,junmm2a,junmm3a,junmm4a,junmm5a,junmm1c,junmm2c,junmm3c,junmm4c,junmm5c,junmm1r,junmm2r,junmm3r,junmm4r,junmm5r,junmm1imp,junmm2imp,junmm3imp,junmm4imp,junmm5imp)
summary(junmm3a)
summary(junm1a)
summary(junmm5r)
summary(junmm4imp)
#two best fit models:
tab_model(junmm5r, digits=3)
tab_model(junmm5imp, digits=3)


jundat$dom<- as.numeric(substr(jundat$date,4,5))

cols<-c("seagreen3","gray")

png("figs/airtempbvsBA.png", width=10, height=6, units="in", res=220)
plot(jundat$dom,jundat$airtemp_c, 
     pch=21,bg=alpha(cols[as.factor(jundat$Trees.)],.5),
     ylim=c(10,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Day of the month",ylab=c("Temperature (C)"), main="June Air temperature",bty="l")
legend("topleft",legend=c("Trees","No Trees"),pch=21, pt.bg=cols)
dev.off()

domnames<-c("",1,"",2,"",3,"",4,"",5,"",6,"",7,"",8,"",9,"",
           10,"",11,"",12,"",13,"",14,"",15,"",16,"",17,"",18,"",19,"",
           20,"",21,"",22,"",23,"",24,"",25,"",26,"",27,"",28,"",29,"",30)
png("figs/airtempvdayJuneboxplot.png", width=10, height=6, units="in", res=220)

x<-boxplot(airtemp_c ~ Trees.+dom, beside=TRUE,data = jundat,
           col = rev(cols), xaxs = n, names=FALSE,
           xlab="Day in June", ylab="Air Temperature (C)")

axis(1,seq(from =4, to=30, by=4), at=seq(from =8,to=60, by=8))

legend("topleft", legend = c("No Trees","Trees"),fill = rev(cols), bty="n")

dev.off()

png("figs/airtempvdaynightboxplot.png", width=8, height=8, units="in", res=220)

x<-boxplot(airtemp_c ~ Trees.+day, beside=TRUE,data = jundat,
           col = rev(cols), xaxs = n, names=c("No Trees","Trees","No Trees","Trees"),
           xlab="", ylab="Air Temperature (C)")
axis(1,c("Night","Day"), at=c(1.5,4), tick=FALSE, line=2)
legend("topleft", legend = c("Trees","No Trees"),fill = cols, bty="n")

dev.off()



#plot conditional effects
# 
# interplot(
#   mm12b,
#   "TotalBA_m2",
#  "temptype", data=asldat_long2)
# 
# interplot(m =  mm12b, var1 ="temptype", var2 = "TotalBA_m2", )
# 

png("figs/temp_cc_mod.png", width=10, height=5, units="in", res=220)
par(mfrow=c(1,2))
plot(jundat$cc.field, jundat$airtemp_c, 
     pch=21,bg=alpha(rev(cols)[as.factor(jundat$Trees.)], .6),
     ylim=c(1,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Canopy cover (field)",ylab=c("Temperature (C)"), main="",bty="l")
abline(a=fixef(junmm3c)[1]+8, b=fixef(junmm3c)[2], lwd=2)#effect of trees in sun, impervious surface
plot(jundat$cc.rem, jundat$airtemp_c, 
     pch=21,bg=alpha(rev(cols)[as.factor(jundat$Trees.)], .6),
     ylim=c(1,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Cover (remote-sensed)",ylab=c("Temperature (C)"), main="",bty="l")

abline(a=fixef(junmm3r)[1]+8, b=fixef(junmm3r)[2],lwd=2)#effect of trees in sun, impervious surface

dev.off()
png("figs/temp_BA_mod.png", width=10, height=5, units="in", res=220)

plot(jundat$TotalBA_cm2/10000,jundat$airtemp_c, 
     pch=21,bg=alpha(rev(cols)[as.factor(jundat$Trees.)], .5),
     ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Tree abundance (basal area)",ylab=c("Temperature (C)"), main="",bty="l")
abline(a=fixef(junmm3a)[1]+8, b=fixef(junmm3a)[2], lwd=2)#effect of trees in sun, impervious surface

dev.off()
##########################################################
############# max and min daily temperature ##############
#########################################################,

jundat.max<-aggregate(jundat$airtemp_c, by=list(jundat$Hobo_SN,jundat$Trees.,jundat$cc.field,jundat$cc.rem, jundat$imp,jundat$Elevation,jundat$dom), max)
jundat.min<-aggregate(jundat$airtemp_c, by=list(jundat$Hobo_SN,jundat$Trees.,jundat$cc.field,jundat$cc.rem, jundat$imp,jundat$Elevation,jundat$dom), min)
jundat.minmax<-cbind(jundat.max, jundat.min$x)

juldat.max<-aggregate(juldat$airtemp_c, by=list(juldat$Hobo_SN,juldat$Trees.,juldat$cc.field,juldat$cc.rem, juldat$imp,juldat$Elevation,juldat$dom), max)
juldat.min<-aggregate(juldat$airtemp_c, by=list(juldat$Hobo_SN,juldat$Trees.,juldat$cc.field,juldat$cc.rem, juldat$imp,juldat$Elevation,juldat$dom), min)
jundat.minmax<-cbind(jundat.max, jundat.min$x)
colnames(jundat.minmax)<-c("Hobo_SN","Trees.","cc.field","cc.rem", "imp","Elevation","dom","T_max","T_min")

colnames(jundat.minmax)<-c("Hobo_SN","Trees.","cc.field","cc.rem", "imp","Elevation","dom","T_max","T_min")

junTmaxa<-lm(T_max~Trees.+dom+Elevation+Trees.:Elevation+Trees.:dom, data=jundat.minmax)
junTmaxc<-lm(T_max~cc.field+dom+Elevation+cc.field:Elevation+cc.field:dom, data=jundat.minmax)
junTmaxr<-lm(T_max~cc.rem+dom+Elevation+cc.rem:Elevation+cc.rem:dom, data=jundat.minmax)
junTmaximp<-lm(T_max~imp+dom+Elevation+imp:Elevation+imp:dom, data=jundat.minmax)
AIC(junTmaxa,junTmaxc,junTmaxr,junTmaximp)
summary(junTmaxc)
tab_model(junTmaxr)
tab_model(junTmaximp)
tab_model(junTmaxc)
summary(junTmaxr)

png("figs/maxtemp_cc_mod.png", width=6, height=5, units="in", res=220)

plot(jundat.minmax$cc.field ,jundat.minmax$T_max, 
     pch=21,bg=alpha("darkgreen", .5),
     ylim=c(12,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Tree cover (%)",ylab=c("Temperature (C)"), main="",bty="l")
abline(a=coef(junTmaxc)[1]+6, b=coef(junTmaxc)[2], lwd=2)#effect of trees in sun, impervious surface

dev.off()

png(file="figs/tmax.fieldcc.png",width =1800, height =1800 ,res =200)

ggplot(jundat.minmax,
       aes(x= cc.field, y = T_max)) +
  geom_point() +
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))

dev.off()
png(file="figs/tmin.fieldcc.png",width =1800, height =1800 ,res =200)

ggplot(jundat.minmax,
       aes(x= cc.field, y = T_min)) +
  geom_point() +
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))

dev.off()

#Select only the hottest day of the year- Jun 27 for Tmax, June 26b for Tmin
jundat.minmax$dom[which(jundat.minmax$T_max==max(jundat.minmax$T_max))]
jundat.minmax$dom[which(jundat.minmax$T_min==max(jundat.minmax$T_min))]

jun27dat<-jundat.minmax[jundat.minmax$dom==27,]
jun26dat<-jundat.minmax[jundat.minmax$dom==26,]
jun20dat<-jundat.minmax[jundat.minmax$dom==20,]

png(file="figs/tmax.fieldcc.2022jun27.png",width =1800, height =1800 ,res =200)

ggplot(jun27dat,
       aes(x= cc.field, y = T_max)) +
  geom_point() +
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))

dev.off()
png(file="figs/tmin.fieldcc.2022jun26.png",width =1800, height =1800 ,res =200)

ggplot(jun26dat,
       aes(x= cc.field, y = T_min)) +
  geom_point() +
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))

dev.off()

  
png(file="figs/tmin.fieldcc.2022jun.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in 1:length(unique(jundat.minmax$dom))){
daydat<- jundat.minmax[jundat.minmax$dom==i,]
m<-lm(T_min~cc.field, data=daydat)
msum<-round(c(m$coef, summary(m)$r.squared,summary(m)$coef[2,4]), digits=3)
plms<-rbind(plms,msum)

  p[[i]]<- ggplot(daydat,
       aes(x= cc.field, y = T_min, main=i)) +
  geom_point() +
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))
}
colnames(plms)[3:4]<-c("r2","p")
tmin.field.cc<-as.data.frame(plms)

do.call(grid.arrange,p)
dev.off()


png(file="figs/tmax.fieldcc.2022jun.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in 1:length(unique(jundat.minmax$dom))){
  daydat<- jundat.minmax[jundat.minmax$dom==i,]
  m<-lm(T_max~cc.field, data=daydat)
  msum<-round(c(m$coef, summary(m)$r.squared,summary(m)$coef[2,4]), digits=3)
  plms<-rbind(plms,msum)
  
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.field, y = T_max, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
colnames(plms)[3:4]<-c("r2","p")
tmax.field.cc<-as.data.frame(plms)

do.call(grid.arrange,p)
dev.off()


png(file="figs/tmax.remcc.2022jun.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in 1:length(unique(jundat.minmax$dom))){
  daydat<- jundat.minmax[jundat.minmax$dom==i,]
  m<-lm(T_max~cc.rem, data=daydat)
  msum<-round(c(m$coef, summary(m)$r.squared,summary(m)$coef[2,4]), digits=3)
  plms<-rbind(plms,msum)
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.rem, y = T_max, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
do.call(grid.arrange,p)
dev.off()
colnames(plms)[3:4]<-c("r2","p")
tmax.rem.cc<-as.data.frame(plms)

png(file="figs/tmin.remcc.2022jun.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in 1:length(unique(jundat.minmax$dom))){
  m<-lm(T_min~cc.rem, data=daydat)
  msum<-round(c(m$coef, summary(m)$r.squared,summary(m)$coef[2,4]), digits=3)
  plms<-rbind(plms,msum)
  
  daydat<- jundat.minmax[jundat.minmax$dom==i,]
  
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.rem, y = T_min, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
do.call(grid.arrange,p)
dev.off()
colnames(plms)[3:4]<-c("r2","p")
tmin.rem.cc<-as.data.frame(plms)

#compare coefs and ps
length(which(tmax.rem.cc$p<0.05))
mean(tmax.rem.cc$cc.rem)
length(which(tmin.rem.cc$p<0.05))
mean(tmin.rem.cc$cc.rem)
length(which(tmin.field.cc$p<0.05))
mean(tmin.field.cc$cc.field)
length(which(tmax.field.cc$p<0.05))
mean(tmax.field.cc$cc.field)


#########same thing for july:
juldat$dom<- as.numeric(substr(juldat$date,4,5))

juldat.max<-aggregate(juldat$airtemp_c, by=list(juldat$Hobo_SN,juldat$Trees.,juldat$cc.field,juldat$cc.rem, juldat$imp,juldat$Elevation,juldat$dom), max)
juldat.min<-aggregate(juldat$airtemp_c, by=list(juldat$Hobo_SN,juldat$Trees.,juldat$cc.field,juldat$cc.rem, juldat$imp,juldat$Elevation,juldat$dom), min)
juldat.minmax<-cbind(juldat.max, juldat.min$x)

juldat.max<-aggregate(juldat$airtemp_c, by=list(juldat$Hobo_SN,juldat$Trees.,juldat$cc.field,juldat$cc.rem, juldat$imp,juldat$Elevation,juldat$dom), max)
juldat.min<-aggregate(juldat$airtemp_c, by=list(juldat$Hobo_SN,juldat$Trees.,juldat$cc.field,juldat$cc.rem, juldat$imp,juldat$Elevation,juldat$dom), min)
juldat.minmax<-cbind(juldat.max, juldat.min$x)
colnames(juldat.minmax)<-c("Hobo_SN","Trees.","cc.field","cc.rem", "imp","Elevation","dom","T_max","T_min")

colnames(juldat.minmax)<-c("Hobo_SN","Trees.","cc.field","cc.rem", "imp","Elevation","dom","T_max","T_min")

julTmaxa<-lm(T_max~Trees.+dom+Elevation+Trees.:Elevation+Trees.:dom, data=juldat.minmax)
julTmaxc<-lm(T_max~cc.field+dom+Elevation+cc.field:Elevation+cc.field:dom, data=juldat.minmax)
julTmaxr<-lm(T_max~cc.rem+dom+Elevation+cc.rem:Elevation+cc.rem:dom, data=juldat.minmax)
julTmaximp<-lm(T_max~imp+dom+Elevation+imp:Elevation+imp:dom, data=juldat.minmax)
AIC(julTmaxa,julTmaxc,julTmaxr,julTmaximp)
summary(julTmaxc)
tab_model(julTmaxr)
tab_model(julTmaximp)
tab_model(julTmaxc)
summary(julTmaxr)

png("figs/maxtemp_cc_mod.png", width=6, height=5, units="in", res=220)

plot(juldat.minmax$cc.field ,juldat.minmax$T_max, 
     pch=21,bg=alpha("darkgreen", .5),
     ylim=c(12,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Tree cover (%)",ylab=c("Temperature (C)"), main="",bty="l")
abline(a=coef(julTmaxc)[1]+6, b=coef(julTmaxc)[2], lwd=2)#effect of trees in sun, impervious surface

dev.off()

png(file="figs/tmax.fieldcc.png",width =1800, height =1800 ,res =200)

ggplot(juldat.minmax,
       aes(x= cc.field, y = T_max)) +
  geom_point() +
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))

dev.off()
png(file="figs/tmin.fieldcc.png",width =1800, height =1800 ,res =200)

ggplot(juldat.minmax,
       aes(x= cc.field, y = T_min)) +
  geom_point() +
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))

dev.off()

#Select only the hottest day of the year- jul 27 for Tmax, jule 26b for Tmin
juldat.minmax$dom[which(juldat.minmax$T_max==max(juldat.minmax$T_max))]
juldat.minmax$dom[which(juldat.minmax$T_min==max(juldat.minmax$T_min))]

jul27dat<-juldat.minmax[juldat.minmax$dom==27,]
jul26dat<-juldat.minmax[juldat.minmax$dom==26,]
jul20dat<-juldat.minmax[juldat.minmax$dom==20,]

png(file="figs/tmax.fieldcc.2022jul27.png",width =1800, height =1800 ,res =200)

ggplot(jul27dat,
       aes(x= cc.field, y = T_max)) +
  geom_point() +
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))

dev.off()
png(file="figs/tmin.fieldcc.2022jul26.png",width =1800, height =1800 ,res =200)

ggplot(jul26dat,
       aes(x= cc.field, y = T_min)) +
  geom_point() +
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))

dev.off()


png(file="figs/tmin.fieldcc.2022jul.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in 1:length(unique(juldat.minmax$dom))){
  daydat<- juldat.minmax[juldat.minmax$dom==i,]
  m<-lm(T_min~cc.field, data=daydat)
  msum<-round(c(m$coef, summary(m)$r.squared,summary(m)$coef[2,4]), digits=3)
  plms<-rbind(plms,msum)
  
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.field, y = T_min, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
colnames(plms)[3:4]<-c("r2","p")
tmin.field.cc<-as.data.frame(plms)

do.call(grid.arrange,p)
dev.off()


png(file="figs/tmax.fieldcc.2022jul.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in 1:length(unique(juldat.minmax$dom))){
  daydat<- juldat.minmax[juldat.minmax$dom==i,]
  m<-lm(T_max~cc.field, data=daydat)
  msum<-round(c(m$coef, summary(m)$r.squared,summary(m)$coef[2,4]), digits=3)
  plms<-rbind(plms,msum)
  
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.field, y = T_max, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
colnames(plms)[3:4]<-c("r2","p")
tmax.field.cc<-as.data.frame(plms)

do.call(grid.arrange,p)
dev.off()


png(file="figs/tmax.remcc.2022jul.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in 1:length(unique(juldat.minmax$dom))){
  daydat<- juldat.minmax[juldat.minmax$dom==i,]
  m<-lm(T_max~cc.rem, data=daydat)
  msum<-round(c(m$coef, summary(m)$r.squared,summary(m)$coef[2,4]), digits=3)
  plms<-rbind(plms,msum)
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.rem, y = T_max, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
do.call(grid.arrange,p)
dev.off()
colnames(plms)[3:4]<-c("r2","p")
tmax.rem.cc<-as.data.frame(plms)

png(file="figs/tmin.remcc.2022jul.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in 1:length(unique(juldat.minmax$dom))){
  m<-lm(T_min~cc.rem, data=daydat)
  msum<-round(c(m$coef, summary(m)$r.squared,summary(m)$coef[2,4]), digits=3)
  plms<-rbind(plms,msum)
  
  daydat<- juldat.minmax[juldat.minmax$dom==i,]
  
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.rem, y = T_min, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
do.call(grid.arrange,p)
dev.off()
colnames(plms)[3:4]<-c("r2","p")
tmin.rem.cc<-as.data.frame(plms)

#compare coefs and ps
length(which(tmax.rem.cc$p<0.05))
mean(tmax.rem.cc$cc.rem)
length(which(tmin.rem.cc$p<0.05))
mean(tmin.rem.cc$cc.rem)
length(which(tmin.field.cc$p<0.05))
mean(tmin.field.cc$cc.field)
length(which(tmax.field.cc$p<0.05))
mean(tmax.field.cc$cc.field)
#compare veg metrics

vegmetrics<-subset(locs3,select=c(TotalBA_cm2,X3CoarseVeg.10mProp,X6ImpOther.10m,XAllImp.10mProp,PercOpen_Mean))
vegmetrics$cc.field<-100-as.numeric(vegmetrics$PercOpen_Mean)
vegmetrics$cc.rem<-vegmetrics$X3CoarseVeg.10mProp*100
vegmetrics$allimp<-vegmetrics$XAllImp.10mProp*100
vegmet<-subset(vegmetrics,select=c(TotalBA_cm2,cc.field,cc.rem,allimp))
vegmet<-vegmet[-which(is.na(vegmet$TotalBA_cm2)),]
vegmet<-vegmet[-which(is.na(vegmet$allimp)),]

pairs(vegmet, pch=16,col="gray",upper.panel=panel.cor)


#compare veg metrics

vegmetrics<-subset(locs3,select=c(TotalBA_cm2,X3CoarseVeg.10mProp,X6ImpOther.10m,XAllImp.10mProp,PercOpen_Mean))
vegmetrics$cc.field<-100-as.numeric(vegmetrics$PercOpen_Mean)
vegmetrics$cc.rem<-vegmetrics$X3CoarseVeg.10mProp*100
vegmetrics$allimp<-vegmetrics$XAllImp.10mProp*100
vegmet<-subset(vegmetrics,select=c(TotalBA_cm2,cc.field,cc.rem,allimp))
vegmet<-vegmet[-which(is.na(vegmet$TotalBA_cm2)),]
vegmet<-vegmet[-which(is.na(vegmet$allimp)),]

pairs(vegmet, pch=16,col="gray",upper.panel=panel.cor)

