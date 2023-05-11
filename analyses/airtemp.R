#######################################################
### Script to look at GRIT airtemp data ###
################# Started August 24, 2022 #####################
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
library(gridExtra)
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

#Read in temperature data from Visual crossings for Tacoma
vc<-read.csv("../data/TacomaWA2022-01-01to2023-01-01.csv", header=TRUE)
#load helper functions
source("sourced_files/helper_funcs.R")

#Before looking at temperature, look at field-collected canopy vs remote sensed land cover
png("figs/remote_field_cover.png", width=6, height=6, units="in", res=220)
field<-100-as.numeric(locs3$PercOpen_Mean)
rem<-locs3$X3CoarseVeg.10mProp*100

plot(field,rem, pch=16, col="gray",
     xlab="Field Canopy Cover",
     ylab="Remote-sensed Vegetation Cover", 
     main="Remote sensed vs Field Estimates of Tree Cover")
r<-lm(rem~field)
abline(r)
x<-seq(1,100, by=1)
lines(x,x, lty=2)
mtext(paste("r2=",round(summary(r)$r.squared, digits=3),", p=",round(summary(r)$coef[2,4], digits=3), sep=""), side=3, line=-1, adj=0)
text(field,rem,labels=as.character(locs3$WptNo))#check which sites are off
dev.off()

dif<-field-rem
length(which(abs(dif)>20))#24 sites differ in estimate of canopy cover by more thaan 20%

#which are these sites?
difsites<-unique(locs3$WptNo[which(abs(dif)>20)])#in nearly all cases, fieldcc>remcc, which makes sense
#check that lat/long is correct or reasonable for each of these
cbind(locs3$WptNo[which(abs(dif)>20)],
       # locs3$Location[which(abs(dif)>20)],
      field[which(abs(dif)>20)],
      rem[which(abs(dif)>20)])

png("figs/treenum_fieldcover.png", width=6, height=6, units="in", res=220)

plot(locs3$NumTrees,field, pch=16, col="gray",
     ylab="Field Canopy Cover",
     xlab="Number of Trees within 10m")
r<-gam(field~locs3$NumTrees)

lines(locs3$NumTrees,r$fitted.values)
mtext(paste("r2=",round(summary(r)$r.sq, digits=3),", p=",round(summary(r)$pTerms.pv, digits=3), sep=""), side=3, line=-1, adj=0)
dev.off()
png("figs/streettreenum_fieldcover.png", width=6, height=6, units="in", res=220)

plot(locs3$NumTrees[locs3$shield=="YES"],field[locs3$shield=="YES"], pch=16, col="gray",
     ylab="Field Canopy Cover",
     xlab="Number of Trees within 10m")
r<-gam(field[locs3$shield=="YES"]~locs3$NumTrees[locs3$shield=="YES"])

lines(locs3$NumTrees[locs3$shield=="YES"],r$fitted.values)
mtext(paste("r2=",round(summary(r)$r.sq, digits=3),", p=",round(summary(r)$pTerms.pv, digits=3), sep=""), side=3, line=-1, adj=0)
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
locdat2 <- subset(locs3, select=c(Hobo_SN, Location, Latitude, Longitude, Elevation, Trees., TotalBA_cm2, PercOpen_Mean,X1FineVeg.10mProp,X2MedVeg.10mProp,X3CoarseVeg.10mProp,XAllImp.10mProp,X3CoarseVeg.20mProp,X3CoarseVeg.30mProp,X3CoarseVeg.40mProp,X3CoarseVeg.50mProp,X3CoarseVeg.100mProp,X3CoarseVeg.200mProp,X3CoarseVeg.400mProp,X3CoarseVeg.800mProp))

alldat<-left_join(allairdat,locdat2, by="Hobo_SN", copy=TRUE)
alldat<-alldat[-(which(is.na(alldat$airtemp_c))),]
alldat<-alldat[-which(alldat$Hobo_SN=="21223113"),]#messed up data!
#alldat<-alldat[-which(alldat$airtemp_c>36),]#inaccurate temperature readings?

#unique(alldat$Hobo_SN[which(is.na(alldat$Longitude))])
###14 serial numbers do not line up (e.g., we have temp data, but no location matches up to them).
###These are: unique(alldat$Hobo_SN[which(is.na(alldat$Longitude))])
#8 of these are pilot data loggesr and we want them removed.
###pilot loggers are: 21162466, 21162230,21162234,21162233, 21162232, 21162231, 21119950, 9768700
#The following are not pilots and I need to figure out why they are not matching:
####Mystery loggers: "9768697", "BT21302960", "BT21302963","BT21302977" -mystery logger...figure out the deal with these
####"BT21302948"-this is a temp blitz logger- no need to include
#removing these as we don't know where they're from

alldat<-alldat[-(which(is.na(alldat$Longitude))),]
#unique(alldat$Location[which(is.na(alldat$Longitude))])

#Merge in tree data
alldat2<-alldat
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
# #add column for whether logger has a sheild or not- no longer necessary as this is done in "combine_lc_grittreedat.R"
#BUT- that code must not be working, so need to look into it!
alldat2$shield<-"YES"
alldat2$shield[alldat$Location=="Wapato Hills 2"]<-"NO"
alldat2$shield[alldat$Location=="Wapato Hills 1"]<-"NO"
alldat2$shield[alldat$Location=="South Tacoma Wetland"]<-"NO"
alldat2$shield[alldat$Location=="South Tacoma Wetland 1"]<-"NO"

#remove data from loggers without a shield
alldat2<-alldat2[alldat2$shield=="YES",]

#check that locations with trees have more bai:
boxplot(TotalBA_cm2~Trees., data=alldat2)
#Make some plots
boxplot(as.numeric(airtemp_c)~Trees., data=alldat2)
summary(lm(airtemp_c~Trees., data=alldat2))#sites with trees are cooler by -0.34556 on everage

juldat<-alldat2[alldat2$month=="07",]
jundat<-alldat2[alldat2$month=="06",]
summary(lm(airtemp_c~Trees., data=jundat))
summary(lm(airtemp_c~Trees., data=juldat))
summary(lm(airtemp_c~Trees.*day, data=jundat))
summary(lm(airtemp_c~Trees.*day, data=juldat))

boxplot(jundat$airtemp_c[jundat$day==1]~jundat$Trees.[jundat$day==1])
boxplot(jundat$airtemp_c[jundat$day==0]~jundat$Trees.[jundat$day==0])
#Which logger recorded the hottest temperature and when was it?
jundat[which(jundat$airtemp_c==max(jundat$airtemp_c)),]
juldat[which(juldat$airtemp_c==max(juldat$airtemp_c)),]

#hottest day of the year:head(jundat[jundat$date=="06/27/22",])

#
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

#Fit some mixed effects models
junmm1a<-lmer(airtemp_c~Trees.*day + (1|Hobo_SN), data=jundat)
junmm2b<-lmer(airtemp_c~TotalBA_cm2*hour+ (1|Hobo_SN), data=jundat)
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

aictab<-AIC(junmm1a,junmm2a,junmm3a,junmm4a,junmm5a,junmm1c,junmm2c,junmm3c,junmm4c,junmm5c,junmm1r,junmm2r,junmm3r,junmm4r,junmm5r,junmm1imp,junmm2imp,junmm3imp,junmm4imp,junmm5imp)
aictab<-aictab[order(aictab$AIC),]
aictab
summary(junmm5a)#lowest AIC with tree cover, though all mm5s  have fairly similar AICs
summary(junmm5r)#almost
summary(junmm5imp)#lowest AIC
#the best fit models:
tab_model(junmm5r, digits=3)
tab_model(junmm5imp, digits=3)
tab_model(junmm5c, digits=3)


jundat$dom<- as.character(substr(jundat$date,4,5))

cols<-c("seagreen3","gray")

png("figs/airtempbvsBA.png", width=10, height=6, units="in", res=220)
plot(as.numeric(jundat$dom),jundat$airtemp_c, 
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
     #ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Tree abundance (basal area)",ylab=c("Temperature (C)"), main="",bty="l")
abline(a=fixef(junmm3a)[1]+8, b=fixef(junmm3a)[2], lwd=2)#effect of trees in sun, impervious surface

dev.off()

#Calculate number of hours above 89F in June and July
jundat$heattrigger<-0
jundat$heattrigger[which(jundat$airtemp_c>31.7)]<-1
juldat$heattrigger<-0
juldat$heattrigger[which(juldat$airtemp_c>31.7)]<-1

heattrighrsjun<-aggregate(jundat$heattrigger, by=list(jundat$Hobo_SN,jundat$Trees.,jundat$cc.field), sum)
heattrighrsjul<-aggregate(juldat$heattrigger, by=list(juldat$Hobo_SN,juldat$Trees.,juldat$cc.field), sum)

colnames(heattrighrsjun)<-c("Hobo_SN","Trees.","cc.field","heattrig_hrsjun")
colnames(heattrighrsjul)<-c("Hobo_SN","Trees.","cc.field","heattrig_hrsjul")

heattrighrs<-left_join(heattrighrsjun,heattrighrsjul)

#Fit a binomial model with probability of heatrigger as response, trees or cc as explanatory variable
jundaydat<-jundat[jundat$day==1,]
junmod<-glm(heattrigger~Trees., family="binomial",data=jundaydat)
junmod2<-glm(heattrigger~cc.field+Elevation, family="binomial",data=jundaydat)
summary(junmod2)
#junmmod2<-glmer(heattrigger~cc.field+Elevation+ (1|Hobo_SN), family="binomial",data=jundat)
#summary(junmmod2)
juldaydat<-juldat[juldat$day==1,]

julmod<-glm(heattrigger~Trees., family="binomial",data=juldaydat)
julmod2<-glm(heattrigger~cc.field+Elevation, family="binomial",data=juldaydat)
summary(junmod2)

coef(junmod2)

summary(junmod2)
xcanopy <- seq(0,100,1)
xelev <- rep(mean(jundaydat$Elevation),times=length(xcanopy))

#Now we use the predict() function to create the model for all of the values of xweight.

yprob <- predict(junmod2, list(cc.field = xcanopy, Elevation=xelev),type="response")


#Make a plot
heattab<-table(jundaydat$heattrigger,jundaydat$cc.field)
heatprob<-heattab[2,]/(heattab[1,]+heattab[2,])

#seprob <- predict(heatprob, list(cc.field = as.numeric(colnames(heattab)), Elevation=xelev),type="response", se.fit=TRUE)
png("figs/probheattrigs.png", width=10, height=10, units="in", res=220)
par(mar=c(4,10,4,1))

plot(as.numeric(colnames(heattab)), heatprob, pch = 16, 
     xlab = "Tree Canopy Cover (%)", 
     ylab = "Probability of Temps >89°F",
     ylim=c(0,.03),
     bty="l")

lines(xcanopy, yprob, lwd=2)
dev.off()

range(yprob)


heattrighrjun<-aggregate(jundaydat$heattrigger, by=list(jundaydat$Hobo_SN,jundaydat$Trees.,jundaydat$cc.field), sum)


#do some forecasting of effects of warming:
jundaydat$airtemp_plus.5<-jundaydat$airtemp_c+.5
jundaydat$airtemp_plus1<-jundaydat$airtemp_c+1
jundaydat$airtemp_plus2<-jundaydat$airtemp_c+2
jundaydat$heattrigger_plus1<-0
jundaydat$heattrigger_plus1[which(jundaydat$airtemp_plus1>31.7)]<-1

jundaydat$heattrigger_plus2<-0
jundaydat$heattrigger_plus2[which(jundaydat$airtemp_plus2>31.7)]<-1

jundaydat$heattrigger_plus.5<-0
jundaydat$heattrigger_plus.5[which(jundaydat$airtemp_plus.5>31.7)]<-1

junmod2plus.5<-glm(heattrigger_plus.5~cc.field+Elevation, family="binomial",data=jundaydat)
junmod2plus1<-glm(heattrigger_plus1~cc.field+Elevation, family="binomial",data=jundaydat)
junmod2plus2<-glm(heattrigger_plus2~cc.field+Elevation, family="binomial",data=jundaydat)

summary(junmod2plus2)
heattab.5<-table(jundaydat$heattrigger_plus.5,jundaydat$cc.field)
heatprob.5<-heattab.5[2,]/(heattab.5[1,]+heattab.5[2,])

##########################################################
############# max, min, mean daily temperature ##############
#########################################################,

jundat.max<-aggregate(jundat$airtemp_c, by=list(jundat$Hobo_SN,jundat$Trees.,jundat$cc.field,jundat$cc.rem, jundat$imp,jundat$Elevation,jundat$X3CoarseVeg.20mProp,jundat$X3CoarseVeg.30mProp,jundat$X3CoarseVeg.40mProp,jundat$X3CoarseVeg.50mProp,jundat$X3CoarseVeg.100mProp,jundat$X3CoarseVeg.200mProp,jundat$X3CoarseVeg.400mProp,jundat$X3CoarseVeg.800mProp,jundat$dom), max)
jundat.min<-aggregate(jundat$airtemp_c, by=list(jundat$Hobo_SN,jundat$Trees.,jundat$cc.field,jundat$cc.rem, jundat$imp,jundat$Elevation,jundat$X3CoarseVeg.20mProp,jundat$X3CoarseVeg.30mProp,jundat$X3CoarseVeg.40mProp,jundat$X3CoarseVeg.50mProp,jundat$X3CoarseVeg.100mProp,jundat$X3CoarseVeg.200mProp,jundat$X3CoarseVeg.400mProp,jundat$X3CoarseVeg.800mProp,jundat$dom), min)
jundat.ave<-aggregate(jundat$airtemp_c, by=list(jundat$Hobo_SN,jundat$Trees.,jundat$cc.field,jundat$cc.rem, jundat$imp,jundat$Elevation,jundat$X3CoarseVeg.20mProp,jundat$X3CoarseVeg.30mProp,jundat$X3CoarseVeg.40mProp,jundat$X3CoarseVeg.50mProp,jundat$X3CoarseVeg.100mProp,jundat$X3CoarseVeg.200mProp,jundat$X3CoarseVeg.400mProp,jundat$X3CoarseVeg.800mProp,jundat$dom), mean)

jundat.minmax<-cbind(jundat.max, jundat.min$x,jundat.ave$x)
colnames(jundat.minmax)<-c("Hobo_SN","Trees.","cc.field","cc.rem", "imp","Elevation","cc.rem20m","cc.rem30m","cc.rem40m","cc.rem50m","cc.rem100m","cc.rem200m","cc.rem400m","cc.rem800m","dom","T_max","T_min","T_ave")

juldat$dom<- as.character(substr(juldat$date,4,5))
juldat.max<-aggregate(juldat$airtemp_c, by=list(juldat$Hobo_SN,juldat$Trees.,juldat$cc.field,juldat$cc.rem, juldat$imp,juldat$Elevation,juldat$X3CoarseVeg.20mProp,juldat$X3CoarseVeg.30mProp,juldat$X3CoarseVeg.40mProp,juldat$X3CoarseVeg.50mProp,juldat$X3CoarseVeg.100mProp,juldat$X3CoarseVeg.200mProp,juldat$X3CoarseVeg.400mProp,juldat$X3CoarseVeg.800mProp,juldat$dom), max)
juldat.min<-aggregate(juldat$airtemp_c, by=list(juldat$Hobo_SN,juldat$Trees.,juldat$cc.field,juldat$cc.rem, juldat$imp,juldat$Elevation,juldat$X3CoarseVeg.20mProp,juldat$X3CoarseVeg.30mProp,juldat$X3CoarseVeg.40mProp,juldat$X3CoarseVeg.50mProp,juldat$X3CoarseVeg.100mProp,juldat$X3CoarseVeg.200mProp,juldat$X3CoarseVeg.400mProp,juldat$X3CoarseVeg.800mProp,juldat$dom), min)
juldat.ave<-aggregate(juldat$airtemp_c, by=list(juldat$Hobo_SN,juldat$Trees.,juldat$cc.field,juldat$cc.rem, juldat$imp,juldat$Elevation,juldat$X3CoarseVeg.20mProp,juldat$X3CoarseVeg.30mProp,juldat$X3CoarseVeg.40mProp,juldat$X3CoarseVeg.50mProp,juldat$X3CoarseVeg.100mProp,juldat$X3CoarseVeg.200mProp,juldat$X3CoarseVeg.400mProp,juldat$X3CoarseVeg.800mProp,juldat$dom), mean)
juldat.minmax<-cbind(juldat.max, juldat.min$x, juldat.ave$x)
colnames(juldat.minmax)<-colnames(jundat.minmax)
jundat.minmax$Trange<-jundat.minmax$T_max-jundat.minmax$T_min
juldat.minmax$Trange<-juldat.minmax$T_max-juldat.minmax$T_min

#Fit mixed models to account for nonindependent of days
jundat.minmax$dom<-as.factor(jundat.minmax$dom)
juldat.minmax$dom<-as.factor(juldat.minmax$dom)
# junTmaxb<-lmer(T_max~Trees.+Elevation+Trees.:Elevation+Trees.+(1|dom), data=jundat.minmax)
# junTmaxa<-lmer(T_max~Trees.+Elevation+Trees.:Elevation+Trees.+(1|dom), data=jundat.minmax)
 junTmaxc<-lmer(T_max~cc.field+Elevation+cc.field:Elevation +(1|dom), data=jundat.minmax)
# junTmaxr<-lmer(T_max~cc.rem+Elevation+cc.rem:Elevation+cc.rem+(1|dom), data=jundat.minmax)
# junTmaximp<-lmer(T_max~imp+Elevation+imp:Elevation+imp+(1|dom), data=jundat.minmax)
#junTmaxcnoelev<-lmer(T_max~cc.field+(1|dom)+(1|Hobo_SN), data=jundat.minmax)
#junTmaxrnoelev<-lmer(T_max~cc.rem+(1|dom)+(1|Hobo_SN), data=jundat.minmax)
#junTmaxanoelev<-lmer(T_max~Trees.+(1|dom)+(1|Hobo_SN), data=jundat.minmax)

#aictab<-AIC(junTmaxa,junTmaxc,junTmaxr,junTmaximp)
#aictab<-aictab[order(aictab$AIC),]
#aictab
tab_model(junTmaxc, digits=3)
jundat.minmax$T_max_F<-(jundat.minmax$T_max* 9/5) + 32 

#find best random effects structure
# jundat.minmax$cc.field.st<-(jundat.minmax$cc.field-mean(jundat.minmax$cc.field)/sd(jundat.minmax$cc.field))
# junTmaxcnoelev<-lmer(T_max~cc.field.st+(cc.field.st|dom), data=jundat.minmax)
# junTmaxc<-lmer(T_max~cc.field+Elevation+cc.field:Elevation +(1|dom), data=jundat.minmax, REML=TRUE)
junTmaxc2<-lmer(T_max~cc.field+Elevation+cc.field:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax, REML=TRUE)
junTmaxc2F<-lmer(T_max_F~cc.field+Elevation+cc.field:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax, REML=TRUE)

# junTmaxc3<-lmer(T_max~cc.field+Elevation+cc.field:Elevation +(cc.field|dom), data=jundat.minmax, REML=TRUE)
# junTmaxc4<-lmer(T_max~cc.field+Elevation+cc.field:Elevation+(cc.field|dom)+(1|Hobo_SN), data=jundat.minmax, REML=TRUE)
# 
# aictab<-AIC(junTmaxc,junTmaxc2,junTmaxc3,junTmaxc4)
# aictab<-aictab[order(aictab$AIC),]
# aictab
fixef(junTmaxc2)#best fit model
fixef(junTmaxc2F)#best fit model

tab_model(junTmaxc2, digits=4)#model to report in paper
#prep rem sensed data to be on the same scale
jundat.minmax$cc.rem20m<-jundat.minmax$cc.rem20m*100
jundat.minmax$cc.rem30m<-jundat.minmax$cc.rem30m*100
jundat.minmax$cc.rem40m<-jundat.minmax$cc.rem40m*100
jundat.minmax$cc.rem50m<-jundat.minmax$cc.rem50m*100
jundat.minmax$cc.rem100m<-jundat.minmax$cc.rem100m*100
jundat.minmax$cc.rem200m<-jundat.minmax$cc.rem200m*100
jundat.minmax$cc.rem400m<-jundat.minmax$cc.rem400m*100
jundat.minmax$cc.rem800m<-jundat.minmax$cc.rem800m*100


#now compare canopy at different distances away, and remote vs field
junTmaxr2.10m<-lmer(T_max~cc.rem+Elevation+cc.rem:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxr2.20m<-lmer(T_max~cc.rem20m+Elevation+cc.rem20m:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxr2.30m<-lmer(T_max~cc.rem30m+Elevation+cc.rem30m:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxr2.40m<-lmer(T_max~cc.rem40m+Elevation+cc.rem40m:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxr2.50m<-lmer(T_max~cc.rem50m+Elevation+cc.rem50m:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxr2.100m<-lmer(T_max~cc.rem100m+Elevation+cc.rem100m:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxr2.200m<-lmer(T_max~cc.rem200m+Elevation+cc.rem200m:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxr2.400m<-lmer(T_max~cc.rem400m+Elevation+cc.rem400m:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxr2.800m<-lmer(T_max~cc.rem800m+Elevation+cc.rem800m:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)

junTmaxrc2.10m<-lmer(T_max~cc.field+cc.rem+Elevation+cc.rem:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxrc2.20m<-lmer(T_max~cc.field+cc.rem20m+Elevation+cc.rem20m:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxrc2.30m<-lmer(T_max~cc.field+cc.rem30m+Elevation+cc.rem30m:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxrc2.40m<-lmer(T_max~cc.field+cc.rem40m+Elevation+cc.rem40m:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxrc2.50m<-lmer(T_max~cc.field+cc.rem50m+Elevation+cc.rem50m:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxrc2.100m<-lmer(T_max~cc.field+cc.rem100m+Elevation+cc.rem100m:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxrc2.200m<-lmer(T_max~cc.field+cc.field+cc.rem200m+Elevation+cc.rem200m:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxrc2.400m<-lmer(T_max~cc.field+cc.rem400m+Elevation+cc.rem400m:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxrc2.800m<-lmer(T_max~cc.field+cc.rem800m+Elevation+cc.rem800m:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxrc2int.800m<-lmer(T_max~cc.field+cc.rem800m+Elevation+cc.field:Elevation +cc.field:cc.rem800m+(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxc2imp.800m<-lmer(T_max~cc.field+imp+Elevation+ +cc.field:imp+(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxrc2int2.800m<-lmer(T_max~cc.field+cc.rem800m+Elevation+cc.field:Elevation+(1|dom)+(1|Hobo_SN), data=jundat.minmax)

aictab<-AIC(junTmaxc2,junTmaxrc2int.800m,junTmaxc2imp.800m,junTmaxrc2int2.800m,
            junTmaxr2.10m,junTmaxr2.20m,junTmaxr2.30m,junTmaxr2.40m,junTmaxr2.50m,junTmaxr2.100m,junTmaxr2.200m,junTmaxr2.400m,junTmaxr2.800m,
            junTmaxrc2.10m,junTmaxrc2.20m,junTmaxrc2.30m,junTmaxrc2.40m,junTmaxrc2.50m,junTmaxrc2.100m,junTmaxrc2.200m,junTmaxrc2.400m,junTmaxrc2.800m)
aictab<-aictab[order(aictab$AIC),]
aictab
#c2 is best, r2.20m next best
fixef(junTmaxc2)
fixef(junTmaxr2.10m)

fixef(junTmaxr2.20m)
fixef(junTmaxr2.30m)
fixef(junTmaxr2.200m)
tab_model(junTmaxc2, digits=3)

jundat.minmax$T_min_F<-(jundat.minmax$T_min* 9/5) + 32 
jundat.minmax$T_ave_F<-(jundat.minmax$T_ave* 9/5) + 32 

junTmina<-lmer(T_min~Trees.+Elevation+Trees.:Elevation+Trees.+(1|dom), data=jundat.minmax)
junTminc<-lmer(T_min~cc.field+Elevation+cc.field:Elevation+cc.field +(1|dom), data=jundat.minmax)
junTminr<-lmer(T_min~cc.rem+Elevation+cc.rem:Elevation+cc.rem+(1|dom), data=jundat.minmax)
junTminimp<-lmer(T_min~imp+Elevation+imp:Elevation+imp+(1|dom), data=jundat.minmax)
junTminanoelev<-lmer(T_min~Trees.+(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmincnoelev<-lmer(T_min~cc.field+(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTminrnoelev<-lmer(T_min~cc.rem+(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTminc2<-lmer(T_min~cc.field+Elevation+cc.field:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTminc2F<-lmer(T_min_F~cc.field+Elevation+cc.field:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)

AIC(junTmina,junTminc2,junTminc2F,junTminc,junTminr,junTminimp,junTminanoelev,junTmincnoelev,junTminrnoelev)

junTavea<-lmer(T_ave~Trees.+Elevation+Trees.:Elevation+Trees.+(1|dom), data=jundat.minmax)
junTavec<-lmer(T_ave~cc.field+Elevation+cc.field:Elevation+cc.field +(1|dom), data=jundat.minmax)
junTaver<-lmer(T_ave~cc.rem+Elevation+cc.rem:Elevation+cc.rem+(1|dom), data=jundat.minmax)
junTaveimp<-lmer(T_ave~imp+Elevation+imp:Elevation+imp+(1|dom), data=jundat.minmax)
junTavecnoelev<-lmer(T_ave~cc.field +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTavec2<-lmer(T_ave~cc.field+Elevation+cc.field:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax, REML=TRUE)
junTavec2F<-lmer(T_ave_F~cc.field+Elevation+cc.field:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax, REML=TRUE)

AIC(junTavea,junTavec,junTaver,junTaveimp,junTavecnoelev)

julTmaxa<-lmer(T_max~Trees.+Elevation+Trees.:Elevation+Trees.+(1|dom), data=juldat.minmax)
julTmaxc<-lmer(T_max~cc.field+Elevation+cc.field:Elevation+cc.field +(1|dom), data=juldat.minmax)
julTmaxr<-lmer(T_max~cc.rem+Elevation+cc.rem:Elevation+cc.rem+(1|dom), data=juldat.minmax)
julTmaximp<-lmer(T_max~imp+Elevation+imp:Elevation+imp+(1|dom), data=juldat.minmax)
julTmaxcnoelev<-lmer(T_max~cc.field+(1|dom)+(1|Hobo_SN), data=juldat.minmax)
AIC(julTmaxa,julTmaxc,julTmaxr,julTmaximp,julTmaxcnoelev)

julTmina<-lmer(T_min~Trees.+Elevation+Trees.:Elevation+Trees.+(1|dom), data=juldat.minmax)
julTminc<-lmer(T_min~cc.field+Elevation+cc.field:Elevation+cc.field +(1|dom), data=juldat.minmax)
julTminr<-lmer(T_min~cc.rem+Elevation+cc.rem:Elevation+cc.rem+(1|dom), data=juldat.minmax)
julTminimp<-lmer(T_max~imp+Elevation+imp:Elevation+imp+(1|dom), data=juldat.minmax)
julTmincnoelev<-lmer(T_min~cc.field+(1|dom)+(1|Hobo_SN), data=juldat.minmax)
AIC(julTmina,julTminc,julTminr,julTminimp,julTmincnoelev)

julTavea<-lmer(T_ave~Trees.+Elevation+Trees.:Elevation+(1|dom), data=juldat.minmax)
julTavec<-lmer(T_ave~cc.field+Elevation+cc.field:Elevation +(1|dom), data=juldat.minmax)
julTaver<-lmer(T_ave~cc.rem+Elevation+cc.rem:Elevation+(1|dom), data=juldat.minmax)
julTaveimp<-lmer(T_ave~imp+Elevation+imp:Elevation+(1|dom), data=juldat.minmax)
julTavecnoelev<-lmer(T_ave~cc.field+(1|dom)+(1|Hobo_SN), data=juldat.minmax)
AIC(julTavea,julTavec,julTaver,julTaveimp,julTavecnoelev)

junTrangec2<-lmer(Trange~cc.field+Elevation+cc.field:Elevation+cc.field +(1|dom)+(1|Hobo_SN), data=jundat.minmax)
julTrangec2<-lmer(Trange~cc.field+Elevation+cc.field:Elevation+cc.field +(1|dom)+(1|Hobo_SN), data=juldat.minmax)
summary(junTrangec)


#Models to present in report
tab_model(junTmaxc2, digits=3)
tab_model(junTminc2, digits=3)
tab_model(junTavec2, digits=3)
tab_model(junTrangec2, digits=3)

tab_model(julTmaxc, digits=3)
tab_model(julTminc2, digits=3)
tab_model(julTavec, digits=3)
tab_model(julTrangec, digits=3)
png("figs/maxminavtemp_cc_mod.png", width=10, height=5, units="in", res=220)
par(mfrow=c(2,3), mar=c(4,10,4,1))

plot(jundat.minmax$cc.field ,jundat.minmax$T_max, 
     pch=16,col=alpha("darkgreen", .2),
     #ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     main="Tmax",
     xlab="Tree cover (%)",ylab=c("Temperature (C)"), bty="l")
abline(fixef(junTmaxc)[1:2], lwd=4)#effect of trees in sun, impervious surface
mtext("A)", side=3,line=1,adj=-.2)
mtext("June", side=3, line=-10, adj=-.2, outer=TRUE, cex=2)

plot(jundat.minmax$cc.field ,jundat.minmax$T_min, 
     pch=16,col=alpha("darkgreen", .2),
     #ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     main="Tmin",
     xlab="Tree cover (%)",ylab=c("Temperature (C)"),bty="l")
abline(fixef(junTmincnoelev)[1:2], lwd=4)#effect of trees in sun, impervious surface
mtext("B)", side=3,line=1,adj=-.2)

plot(jundat.minmax$cc.field ,jundat.minmax$T_ave, 
     pch=16,col=alpha("darkgreen", .2),
     #ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     main="Tmean",
     xlab="Tree cover (%)",ylab=c("Temperature (C)"), bty="l")
abline(fixef(junTavec)[1:2], lwd=4)#effect of trees in sun, impervious surface
mtext("C)", side=3,line=1,adj=-.2)


###Compare to data from vc
head(jundat.minmax)
vc$mon<-as.factor(substr(vc$datetime,6,7))
vc$year<-substr(vc$datetime,1,4)
vc$dom<-substr(vc$datetime,9,10)

junvc<-vc[vc$mon=="06",]
jundat.minmax<-left_join(jundat.minmax,junvc,by="dom", copy=TRUE)
jundat.minmax$T_maxdif<-jundat.minmax$T_max-jundat.minmax$tempmax
jundat.minmax$T_mindif<-jundat.minmax$T_min-jundat.minmax$tempmin
mean(jundat.minmax$T_maxdif)
range(jundat.minmax$T_maxdif)
hist(jundat.minmax$T_maxdif)
#Fit models to T_maxdf
junTmaxdifa<-lmer(T_maxdif~Trees.+Elevation+Trees.:Elevation+Trees.+(1|dom), data=jundat.minmax)
junTmaxdifc<-lmer(T_maxdif~cc.field+Elevation+cc.field:Elevation+cc.field +(1|dom), data=jundat.minmax)
junTmaxdifr<-lmer(T_maxdif~cc.rem+Elevation+cc.rem:Elevation+cc.rem+(1|dom), data=jundat.minmax)
junTmaxdifimp<-lmer(T_maxdif~imp+Elevation+imp:Elevation+imp+(1|dom), data=jundat.minmax)
junTmaxdifanoelev<-lmer(T_maxdif~Trees.+(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxcdifnoelev<-lmer(T_maxdif~cc.field+(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxdifrnoelev<-lmer(T_maxdif~cc.rem+(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmaxdifc2<-lmer(T_maxdif~cc.field+Elevation+cc.field:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)

AIC(junTmaxdifa,junTmaxdifc2,junTmaxdifc,junTmaxdifr,junTmaxdifimp,junTmaxdifanoelev)
#Fit models to T_mindf
junTmindifa<-lmer(T_mindif~Trees.+Elevation+Trees.:Elevation+Trees.+(1|dom), data=jundat.minmax)
junTmindifc<-lmer(T_mindif~cc.field+Elevation+cc.field:Elevation+cc.field +(1|dom), data=jundat.minmax)
junTmindifr<-lmer(T_mindif~cc.rem+Elevation+cc.rem:Elevation+cc.rem+(1|dom), data=jundat.minmax)
junTmindifimp<-lmer(T_mindif~imp+Elevation+imp:Elevation+imp+(1|dom), data=jundat.minmax)
junTmindifanoelev<-lmer(T_mindif~Trees.+(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmincdifnoelev<-lmer(T_mindif~cc.field+(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmindifrnoelev<-lmer(T_mindif~cc.rem+(1|dom)+(1|Hobo_SN), data=jundat.minmax)
junTmindifc2<-lmer(T_mindif~cc.field+Elevation+cc.field:Elevation +(1|dom)+(1|Hobo_SN), data=jundat.minmax)

AIC(junTmindifa,junTmindifc2,junTmindifc,junTmindifr,junTmindifimp,junTmindifanoelev)

#July temperature
plot(juldat.minmax$cc.field ,juldat.minmax$T_max, 
     pch=16,col=alpha("darkgreen", .2),
     #ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Tree cover (%)",ylab=c("Temperature (C)"), bty="l")
abline(fixef(julTmaxc)[1:2], lwd=4)#effect of trees in sun, impervious surface
mtext("D)", side=3,line=1,adj=-.2)
mtext("July", side=3, line=-10, adj=-.2, outer=TRUE, cex=2)

plot(juldat.minmax$cc.field ,juldat.minmax$T_min, 
     pch=16,col=alpha("darkgreen", .2),
     #ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Tree cover (%)",ylab=c("Temperature (C)"), bty="l")
abline(fixef(julTminc)[1:2], lwd=4)#effect of trees in sun, impervious surface
mtext("E)", side=3,line=1,adj=-.2)

plot(juldat.minmax$cc.field ,juldat.minmax$T_ave, 
     pch=16,col=alpha("darkgreen", .2),
     #ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Tree cover (%)",ylab=c("Temperature (C)"), bty="l")
abline(fixef(julTavec)[1:2], lwd=4)#effect of trees in sun, impervious surface
mtext("F)", side=3,line=1,adj=-.2)

dev.off()



png("figs/maxminavtemp_cc_mod_lines.png", width=10, height=5, units="in", res=220)
par(mfrow=c(2,3), mar=c(4,10,4,1))

plot(jundat.minmax$cc.field ,jundat.minmax$T_max, 
     pch=16,col=alpha("darkgreen", .2),
     #ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     main="Tmax",
     xlab="Tree cover (%)",ylab=c("Temperature (C)"), bty="l")
abline(fixef(junTmaxcnoelev), lwd=4)#effect of trees in sun, impervious surface
mtext("A)", side=3,line=1,adj=-.2)
mtext("June", side=3, line=-10, adj=-.2, outer=TRUE, cex=2)

plot(jundat.minmax$cc.field ,jundat.minmax$T_min, 
     pch=16,col=alpha("darkgreen", .2),
     #ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     main="Tmin",
     xlab="Tree cover (%)",ylab=c("Temperature (C)"),bty="l")
abline(fixef(junTmincnoelev), lwd=4)#effect of trees in sun, impervious surface
mtext("B)", side=3,line=1,adj=-.2)

plot(jundat.minmax$cc.field ,jundat.minmax$T_ave, 
     pch=16,col=alpha("darkgreen", .2),
     #ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     main="Tmean",
     xlab="Tree cover (%)",ylab=c("Temperature (C)"), bty="l")
abline(fixef(junTavecnoelev), lwd=4)#effect of trees in sun, impervious surface
mtext("C)", side=3,line=1,adj=-.2)

#July temperature
plot(juldat.minmax$cc.field ,juldat.minmax$T_max, 
     pch=16,col=alpha("darkgreen", .2),
     #ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Tree cover (%)",ylab=c("Temperature (C)"), bty="l")
abline(fixef(julTmaxcnoelev), lwd=4)#effect of trees in sun, impervious surface
mtext("D)", side=3,line=1,adj=-.2)
mtext("July", side=3, line=-10, adj=-.2, outer=TRUE, cex=2)

plot(juldat.minmax$cc.field ,juldat.minmax$T_min, 
     pch=16,col=alpha("darkgreen", .2),
     #ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Tree cover (%)",ylab=c("Temperature (C)"), bty="l")
abline(fixef(julTmincnoelev), lwd=4)#effect of trees in sun, impervious surface
mtext("E)", side=3,line=1,adj=-.2)

plot(juldat.minmax$cc.field ,juldat.minmax$T_ave, 
     pch=16,col=alpha("darkgreen", .2),
     #ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     xlab="Tree cover (%)",ylab=c("Temperature (C)"), bty="l")
abline(fixef(julTavecnoelev), lwd=4)#effect of trees in sun, impervious surface
mtext("F)", side=3,line=1,adj=-.2)

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
jundat.minmax$dolm[which(jundat.minmax$T_max==max(jundat.minmax$T_max))]
jundat.minmax$dom[which(jundat.minmax$T_min==max(jundat.minmax$T_min))]

jun27dat<-jundat.minmax[jundat.minmax$dom==27,]
jun26dat<-jundat.minmax[jundat.minmax$dom==26,]
jun20dat<-jundat.minmax[jundat.minmax$dom==20,]

png(file="figs/tmax.fieldcc.2022jun27.png",width =1800, height =1800 ,res =200)
ylab <- expression(paste("Maximum Temperature (",degree,"C)", sep=""))

ggplot(jun27dat,
       aes(x= cc.field, y = T_max)) +
  geom_point(size = 3) +
  theme(text = element_text(size = 20)) +
  xlab("Canopy Cover (%)") + ylab("Maximum Temperature (°C)")+
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))
#theme() 

dev.off()
#same plot but in degrees F
png(file="figs/tmax.fieldcc.2022jun27_F.png",width =1800, height =1800 ,res =200)
ylab <- expression(paste("Maximum Temperature (",degree,"F)", sep=""))
jun27dat$T_max_F<-(jun27dat$T_max* 9/5) + 32 

ggplot(jun27dat,
       aes(x= cc.field, y = T_max_F)) +
  geom_point(size = 3) +
  theme(text = element_text(size = 20)) +
  xlab("Canopy Cover (%)") + ylab("Maximum Temperature (°F)")+
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))
#theme() 

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
  daydat<- jundat.minmax[jundat.minmax$dom==i,]
  
  m<-lm(T_min~cc.rem, data=daydat)
  msum<-round(c(m$coef, summary(m)$r.squared,summary(m)$coef[2,4]), digits=3)
  plms<-rbind(plms,msum)
  
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

png("figs/maxtempjul_cc_mod.png", width=6, height=5, units="in", res=220)

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

#Select only the hottest day of the year- jul 31 for Tmax, jule 29 for Tmin
juldat.minmax$dom[which(juldat.minmax$T_max==max(juldat.minmax$T_max))]
juldat.minmax$dom[which(juldat.minmax$T_min==max(juldat.minmax$T_min))]

jul31dat<-juldat.minmax[juldat.minmax$dom==31,]
jul29dat<-juldat.minmax[juldat.minmax$dom==29,]

#which logger had the hottest temp on the hottest day in July?
jul31dat[which(jul31dat$T_max==(max(jul31dat$T_max))),]
jul31dat[which(jul31dat$T_min==(max(jul31dat$T_min))),]

png(file="figs/tmax.fieldcc.2022jul31.png",width =1800, height =1800 ,res =200)

ggplot(jul31dat,
       aes(x= cc.field, y = T_max)) +
  geom_point() +
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))

dev.off()
png(file="figs/tmin.fieldcc.2022jul29.png",width =1800, height =1800 ,res =200)

ggplot(jul29dat,
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
tminjul.field.cc<-as.data.frame(plms)

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
tmaxjul.field.cc<-as.data.frame(plms)

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
tmaxjul.rem.cc<-as.data.frame(plms)

png(file="figs/tmin.remcc.2022jul.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in 1:length(unique(juldat.minmax$dom))){
  daydat<- juldat.minmax[juldat.minmax$dom==i,]
  
   m<-lm(T_min~cc.rem, data=daydat)
  msum<-round(c(m$coef, summary(m)$r.squared,summary(m)$coef[2,4]), digits=3)
  plms<-rbind(plms,msum)
  
  
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.rem, y = T_min, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
do.call(grid.arrange,p)
dev.off()
colnames(plms)[3:4]<-c("r2","p")
tminjul.rem.cc<-as.data.frame(plms)

#compare coefs and ps
length(which(tmaxjul.rem.cc$p<0.05))
mean(tmaxjul.rem.cc$cc.rem)
length(which(tminjul.rem.cc$p<0.05))
mean(tminjul.rem.cc$cc.rem)
length(which(tminjul.field.cc$p<0.05))
mean(tminjul.field.cc$cc.field)
length(which(tmaxjul.field.cc$p<0.05))
mean(tmaxjul.field.cc$cc.field)

##Look at daily patterns on hottest days in june and july
jun27hrdat<-jundat[jundat$dom=="27",]
jul31hrdat<-juldat[juldat$dom=="31",]


png(file="figs/hrly.fieldcc.2022jul31.png",width =1800, height =1800 ,res =200)

plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', 
     ylim=c(15,40),xlim=range(jul31hrdat$hour),
     ylab = 'Temperature (C)', xlab = 'Time of day (hour)')

for(i in unique(jul31hrdat$Hobo_SN)){
 logdat<- jul31hrdat[jul31hrdat$Hobo_SN==i,]
 if(unique(logdat$Trees.)=="N"){treecol="black"}
 if(unique(logdat$Trees.)=="Y"){treecol="darkgreen"}
 
  lines(logdat$hour, logdat$airtemp_c, col=alpha(treecol,.2), lwd=3)
}

dev.off()

#Plots of temperature range-redo this with box plots beside each other

png(file="figs/boxplothrly.fieldcc.2022jul31.png",width =1800, height =800 ,res =200)
par(mfrow=c(1,2))
boxplot(jul31hrdat$airtemp_c[jul31hrdat$Trees.=="N"]~jul31hrdat$hour[jul31hrdat$Trees.=="N"],
        ylim=c(15,38),
        xlab="Time of day (hour)", ylab="Temperature (C)")
boxplot(jul31hrdat$airtemp_c[jul31hrdat$Trees.=="Y"]~jul31hrdat$hour[jul31hrdat$Trees.=="Y"],
        ylim=c(15,38),
        xlab="Time of day (hour)", ylab="Temperature (C)", col="darkgreen")
dev.off()


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

#To make map of loggers by their temperatures, save a csv file with tmax and tmin on hottest days in jun and jult:

jun27datll<-left_join(jun27dat, locs3, by=c("Hobo_SN","Trees.","cc.field", "cc.rem","imp", "Elevation"), suffixes=c("moist", "temp"))

#which logger had the hottest temp on the hottest day in July?
jul31dat[which(jul31dat$T_max==(max(jul31dat$T_max))),]
jul31dat[which(jul31dat$T_min==(max(jul31dat$T_min))),]

templatlonjul31
templatlon
ggplot(alleff, aes(x = Longitude, y = Latitude, color=Tmax)) +
  geom_point(aes(size = Tmax), alpha = 0.5) +
  scale_colour_viridis_c()

#Use temperature anomoly approach and look at cloudiness

# Human health: Days with temps above threshold temp (Flunker et al 2022)
#  The trigger in the current WA permanent heat rule is an air temperature of 89 ◦F (31.7 ◦C) for workers wearing regularwork clothes [24,25]
jundat.minmax$heattrigger<-0
jundat.minmax$heattrigger[which(jundat.minmax$T_max>31.7)]<-1
juldat.minmax$heattrigger<-0
juldat.minmax$heattrigger[which(juldat.minmax$T_max>31.7)]<-1


heattrigjun<-aggregate(jundat.minmax$heattrigger, by=list(jundat.minmax$Hobo_SN,jundat.minmax$Trees.,jundat.minmax$cc.field), sum)
heattrigjul<-aggregate(juldat.minmax$heattrigger, by=list(juldat.minmax$Hobo_SN,juldat.minmax$Trees.,juldat.minmax$cc.field), sum)

colnames(heattrigjun)<-c("Hobo_SN","Trees.","cc.field","heattrig_daysjun")
colnames(heattrigjul)<-c("Hobo_SN","Trees.","cc.field","heattrig_daysjul")

heattrig<-left_join(heattrigjun,heattrigjul)

#Fit a binomial model with probability of heatrigger as response, trees or cc as explanatory variable
mod<-glm(heattrigger~Trees., family="binomial",data=jundat.minmax)
mod2<-glm(heattrigger~cc.field+Elevation, family="binomial",data=jundat.minmax)
summary(mod2)
