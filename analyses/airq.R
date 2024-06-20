#######################################################
### Script to look at GRIT purpleair air quality data ###
################# Started June 20, 2024 #####################
################ ailene.ettinger@tnc.org ##############
#######################################################

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# set working directory
setwd("~/GitHub/grit/analyses")

#Read in data file with PurpleAir logger locations
#(notyet created)
#Read in air quality data from 2 sites
aqd2<-read.csv("../data/PurpleAir/us-epa-pm25-aqi-test2.csv", header=TRUE)

#look at the data
head(aqd2)
#understand the data a bit more- what are these 2 columns- max and min?

range(aqd2$X350Tacoma01.A, na.rm=TRUE)
range(aqd2$X350Tacoma01.B, na.rm=TRUE)
#the two channels at 350Tacoma have very different readings
range(aqd2$Whitman.Elementary.School.A, na.rm=TRUE)
range(aqd2$Whitman.Elementary.School.B, na.rm=TRUE)

mean(c(aqd$X350Tacoma01.A,aqd$X350Tacoma01.B))

#plot A and B
boxplot(aqd[,3:4])
#Read in temperature data from Visual crossings for Tacoma
vc<-read.csv("../data/TacomaWA2022-01-01to2023-01-01.csv", header=TRUE)
#load helper functions
source("sourced_files/helper_funcs.R")

#Before looking at temperature, look at field-collected canopy vs remote sensed land cover
png("figs/remote_field_cover.png", width=6, height=6, units="in", res=220)
#pdf("figs/remote_field_cover.pdf", width=6, height=6)

field<-100-as.numeric(locs3$PercOpen_Mean)
rem<-locs3$X3CoarseVeg.10mProp*100

plot(field,rem, pch=16, col="gray",
     xlab="Field Canopy Cover",
     ylab="Remote-sensed Vegetation Cover", 
     bty="l",cex=1.5,cex.axis=1.5,cex.lab=1.5)
    # main="Remote sensed vs Field Estimates of Tree Cover")
r<-lm(rem~field)
abline(r,lwd=4)
x<-seq(1,100, by=1)
lines(x,x, lty=2, lwd=4)
mtext(paste(" r2=",round(summary(r)$r.squared, digits=3),", p<001", sep=""), side=3, line=-1, adj=0)
#text(field,rem,labels=as.character(locs3$WptNo))#check which sites are off


dev.off()

dif<-field-rem
length(which(abs(dif)>20))#12 sites differ in estimate of canopy cover by more thaan 20%

#which are these sites?
difsites<-unique(locs3$WptNo[which(abs(dif)>20)])#in nearly all cases, fieldcc>remcc, which makes sense
#check that lat/long is correct or reasonable for each of these
cbind(locs3$WptNo[which(abs(dif)>20)],
       # locs3$Location[which(abs(dif)>20)],
      field[which(abs(dif)>20)],
      rem[which(abs(dif)>20)])

png("figs/treenum_fieldcover_noline.png", width=6, height=6, units="in", res=220)

plot(locs3$NumTrees,field, pch=16, col="gray",
     ylab="Field Canopy Cover",
     xlab="Number of Trees within 10m")
r<-gam(field~locs3$NumTrees)

#lines(locs3$NumTrees,r$fitted.values)
#mtext(paste("r2=",round(summary(r)$r.sq, digits=3),", p=",round(summary(r)$pTerms.pv, digits=3), sep=""), side=3, line=-1, adj=0)
dev.off()
png("figs/streettreenum_fieldcover.png", width=6, height=6, units="in", res=220)

plot(locs3$NumTrees[locs3$shield=="YES"],field[locs3$shield=="YES"], pch=16, col="gray",
     ylab="Field Canopy Cover",
     xlab="Number of Trees within 10m")
r<-gam(field[locs3$shield=="YES"]~locs3$NumTrees[locs3$shield=="YES"])

lines(locs3$NumTrees[locs3$shield=="YES"],r$fitted.values)
mtext(paste("r2=",round(summary(r)$r.sq, digits=3),", p=",round(summary(r)$pTerms.pv, digits=3), sep=""), side=3, line=-1, adj=0)
dev.off()
##############################################
### Combine logger info with location info ###
##############################################\

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

dim(airtempdat)#586957 4 on 1/16/2024
unique(airtempdat$Hobo_SN)#55 on 1/16/2024
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

#remove some of the recordings that are NAs
allairdat<-allairdat[which(substr(allairdat$time,4,8)=="00:00"),]
dim(allairdat)#526460 on 1/16/2024

#figure out how many are mismatching and why:
tempdat.logs<-unique(allairdat$Hobo_SN)
mynotes.logs<-unique(logs$Hobo_SN)
numatches<-c()
for (i in 1:length(tempdat.logs)){
  m<-which(mynotes.logs==tempdat.logs[i])
  print(tempdat.logs[i]);print(mynotes.logs[m]);print(i)
}
#missing matches: 
#i=53 "9768697"; #march 2022-august 2022
#i=54 "21119948"#dec 2022-may2023
#i=58 "BT21302977"#march29, 2022-june 29/2022
#i=74 "BT21302948"#only 3 days= 8/9-8/11/2022
numatches<-c()
for (i in 1:length(mynotes.logs)){
  m<-which(tempdat.logs==mynotes.logs[i])
  print(mynotes.logs[i]);print(tempdat.logs[m]);print(i)
}
#missing matches
#i=3 "BT21616940";
#i=6 "BT21616925"
#i=10 "BT21616936"
#i=12 "21302968"
#i=13 "BT21302968"
#i=15 "BT21616941"
#i=17"BT21616949"
#i=20""BT21302942"
#i=22 ""BT21302958"
#i=24 "BT21616951"
#i=26
#...and more- these were used during temp blitz/jamboree/other things

#Put these together
allairdat2<-left_join(allairdat,logs, by="Hobo_SN", copy=TRUE)
#dim(allairdat2)#52640     25 on 6/27/2023
#trim allairdat based on install dates
allairdat2$date<-as.Date(allairdat2$date,format="%m/%d/%y")
allairdat2$Install_Date<-as.Date(allairdat2$Install_Date,format="%m/%d/%Y")
allairdat2$Data_collection_StartDate<-as.Date(allairdat2$Data_collection_StartDate,format="%m/%d/%Y")
allairdat2$Data_collection_EndDate<-as.Date(allairdat2$Data_collection_EndDate,format="%m/%d/%Y")
#check#
#unique(allairdat2$date[allairdat2$date<allairdat2$Install_Date])
#remove rows of temp recorded before logger installed
allairdat2<-allairdat2[-which(allairdat2$date<allairdat2$Install_Date),]
#remove rows of temp recorded afterlogger removed
#skipfornow-not workingallairdat2<-allairdat2[-which(allairdat2$date>allairdat2$Data_collection_EndDate),]

#merge in locdata
#locdat2 <- locs %>% 
#  select(Hobo_SN, Location, Latitude, Longitude, Elevation, Trees.) %>% 
#  distinct(Location, .keep_all= TRUE)

locdat2 <- subset(locs3, select=c(WptNo,Pole_No,Location, Latitude, Longitude, Elevation, Trees., NumTrees,TotalBA_cm2, PercOpen_Mean,X1FineVeg.10mProp,X2MedVeg.10mProp,X3CoarseVeg.10mProp,XAllImp.10mProp,X3CoarseVeg.20mProp,X3CoarseVeg.30mProp,X3CoarseVeg.40mProp,X3CoarseVeg.50mProp,X3CoarseVeg.100mProp,X3CoarseVeg.200mProp,X3CoarseVeg.400mProp,X3CoarseVeg.800mProp))

alldat<-left_join(allairdat2,locdat2, by=c("WptNo","Pole_No"), copy=TRUE)
#alldat<-alldat[-which(alldat$Hobo_SN=="21223113"),]#messed up data
#alldat<-alldat[-which(alldat$airtemp_c>36),]#inaccurate temperature readings?
#the following SNs have WptNo=NA
#unique(alldat$Hobo_SN[which(is.na(alldat$Longitude))])
###12 serial numbers do not line up (e.g., we have temp data, but no location matches up to them).
###These are: unique(alldat$Hobo_SN[which(is.na(alldat$Longitude))])
#8 of these are pilot data loggesr and we want them removed.
###pilot loggers are: 21162466, 21162230,21162234,21162233, 21162232, 21162231, 21119950, 9768700
#The following are not pilots and I need to figure out why they are not matching:
####Mystery loggers: "9768697", "BT21302960", "BT21302963","BT21302977" -mystery logger...figure out the deal with these
####"BT21302948"-this is a temp blitz logger- no need to include
#removing these as we don't know where they're from

alldat<-alldat[-(which(is.na(alldat$Longitude))),]
#unique(alldat$Location[which(is.na(alldat$Longitude))])

alldat2<-alldat
alldat2$airtemp_c<-as.numeric(alldat2$airtemp_c)
alldat2$hour<-as.integer(substr(alldat2$time,1,2))
alldat2$Trees.<-as.factor(alldat2$Trees.)
alldat2$TotalBA_cm2<-as.numeric(alldat2$TotalBA_cm2)
alldat2$TotalBA_m2<-alldat2$TotalBA_cm2/10000
alldat2$Hobo_SN <-as.factor(alldat2$Hobo_SN)
alldat2$Pole_No <-as.factor(alldat2$Pole_No)

alldat2$month<-as.factor(as.character(substr(alldat2$date,6,7)))
alldat2$year<-as.factor(substr(alldat2$date,1,4))
alldat2$day<-0
alldat2$day[alldat2$hour>6 & alldat2$hour<19]<-1

sumdat<-alldat2[alldat2$month=="06"|alldat2$month=="07"|alldat2$month=="08",]
sumdat$cc.field<-100-as.numeric(sumdat$PercOpen_Mean)
sumdat$cc.rem<-sumdat$X3CoarseVeg.10mProp*100
sumdat$imp<-sumdat$XAllImp.10mProp*100

sum22dat<-sumdat[sumdat$year=="2022",]
sum22dat<-sum22dat[sum22dat$Shield.=="Yes",]

#remove extra data measured during temperature blitz
sum22dat<-sum22dat[which(substr(sum22dat$time,4,5)=="00"),]
#dim(sum22dat)[1] 88124    53 on 1/16/2024
jundat<-sum22dat[sum22dat$month=="06",]
juldat<-sum22dat[sum22dat$month=="07",]
augdat<-sum22dat[sum22dat$month=="08",]

#Which logger recorded the hottest temperature and when was it?
jundat[which(jundat$airtemp_c==max(jundat$airtemp_c)),]#34.585  on 2022-06-27
juldat[which(juldat$airtemp_c==max(juldat$airtemp_c)),]# 36.89  on 2022-07-31
sum22dat[which(sum22dat$airtemp_c==max(sum22dat$airtemp_c)),]#36.89  on 2022-07-31

#day in summer which the hottest temp recorded= july 31, Pine btwn 40 & 43 (no trees)- 46.979 (116.5622)- 2 trees, but no canopy cover
#day in june on which the hottest temp recorded= june 26, 45th at Cedar location (no trees)- 43.359C (110 F)

length(unique(sum22dat$Pole_No))#46
table(sum22dat$Pole_No,sum22dat$date)

##################################################
#Fit some models to compare what best explains variation in temp
#########################################################
# #jundat.nona<-jundat[-which(is.na(jundat$TotalBA_cm2)),]#used this to test how Total BA compared to canopy cover, remote data- it was not as good a predictor
# #juldat.nona<-juldat[-which(is.na(juldat$TotalBA_cm2)),]#used this to test how Total BA compared to canopy cover, remote data- it was not as good a predictor
# 
# #Fit some mixed effects models
# junmm1a<-lmer(airtemp_c~Trees.*day + (1|Hobo_SN)+(1), data=jundat)
# junmm2b<-lmer(airtemp_c~TotalBA_cm2*hour+ (1|Hobo_SN), data=jundat)
# junmm1c<-lmer(airtemp_c~cc.field*day+ (1|Hobo_SN), data=jundat)
# junmm1r<-lmer(airtemp_c~cc.rem*day+ (1|Hobo_SN), data=jundat)
# junmm1imp<-lmer(airtemp_c~imp*day+ (1|Hobo_SN), data=jundat)
# junmm2a<-lmer(airtemp_c~Trees.*hour+(1|Hobo_SN), data=jundat)
# junmm2c<-lmer(airtemp_c~cc.field*hour+(1|Hobo_SN), data=jundat)
# junmm2r<-lmer(airtemp_c~cc.rem*hour+(1|Hobo_SN), data=jundat)
# junmm2imp<-lmer(airtemp_c~imp*hour+ (1|Hobo_SN), data=jundat)
# junmm3a<-lmer(airtemp_c~Trees.*hour+Elevation +(1|Hobo_SN), data=jundat)
# junmm3c<-lmer(airtemp_c~cc.field*hour+Elevation+(1|Hobo_SN), data=jundat)
# junmm3r<-lmer(airtemp_c~cc.rem*hour+Elevation +(1|Hobo_SN), data=jundat)
# junmm3imp<-lmer(airtemp_c~imp*hour+Elevation + (1|Hobo_SN), data=jundat)
# junmm4a<-lmer(airtemp_c~Trees.+day+Trees.:day+Elevation + (1|Hobo_SN), data=jundat)
# junmm4c<-lmer(airtemp_c~cc.field+day+cc.field:day+Elevation:cc.field + (1|Hobo_SN), data=jundat)
# junmm4r<-lmer(airtemp_c~cc.rem+day+cc.rem:day+Elevation:cc.rem + (1|Hobo_SN), data=jundat)
# junmm4imp<-lmer(airtemp_c~imp+day+imp:day+Elevation:imp + (1|Hobo_SN), data=jundat)
# 
# #Add hour and interactions
# junmm5a<-lmer(airtemp_c~Trees.+hour+day+Trees.:hour+Trees.:day+Elevation +day:hour+ (1|Hobo_SN), data=jundat)
# junmm5c<-lmer(airtemp_c~cc.field+hour+day+cc.field:hour+cc.field:day+Elevation +day:hour+ (1|Hobo_SN), data=jundat)
# junmm5r<-lmer(airtemp_c~cc.rem+hour+day+cc.rem:hour+cc.rem:day+Elevation +day:hour+ (1|Hobo_SN), data=jundat)
# junmm5imp<-lmer(airtemp_c~imp+hour+day+imp:hour+imp:day+Elevation +day:hour+ (1|Hobo_SN), data=jundat)
# 
# aictab<-AIC(junmm1a,junmm2a,junmm3a,junmm4a,junmm5a,junmm1c,junmm2c,junmm3c,junmm4c,junmm5c,junmm1r,junmm2r,junmm3r,junmm4r,junmm5r,junmm1imp,junmm2imp,junmm3imp,junmm4imp,junmm5imp)
# aictab<-aictab[order(aictab$AIC),]
# aictab
# summary(junmm5imp)#lowest AIC
# summary(junmm5a)#almost
# summary(junmm5c)#lowest AIC
# #the best fit models:
# tab_model(junmm5r, digits=3)
# tab_model(junmm5imp, digits=3)
# tab_model(junmm5c, digits=3)
# 
# 
# jundat$dom<- as.character(substr(jundat$date,9,10))
# 
 cols<-c("gray","darkgreen")
# 
# png("figs/airtempbvsBA.png", width=10, height=6, units="in", res=220)
# plot(as.numeric(jundat$dom),jundat$airtemp_c, 
#      pch=21,bg=alpha(cols[as.factor(jundat$Trees.)],.5),
#      ylim=c(10,50),cex=1.5,
#      cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
#      xlab="Day of the month",ylab=c("Temperature (C)"), main="June Air temperature",bty="l")
# legend("topleft",legend=c("Trees","No Trees"),pch=21, pt.bg=rev(cols))
# dev.off()
# 
# domnames<-c("",1,"",2,"",3,"",4,"",5,"",6,"",7,"",8,"",9,"",
#            10,"",11,"",12,"",13,"",14,"",15,"",16,"",17,"",18,"",19,"",
#            20,"",21,"",22,"",23,"",24,"",25,"",26,"",27,"",28,"",29,"",30)
png("figs/airtempvdaysummerboxplot.png", width=10, height=6, units="in", res=220)

x<-boxplot(airtemp_c ~ Trees.+date, beside=TRUE,data = sum22dat,
           col = cols, xaxs = n, names=FALSE,range=0,
           xlab="Day of Summer 2022", ylab="Air Temperature (°C)")

#axis(1,seq(from =1, to=92, by=2), at=seq(from =8,to=60, by=8))

legend("topleft", legend = c("Trees","No Trees"),fill = rev(cols), bty="n")

dev.off()



# 
png("figs/airtempvdaynightboxplot.png", width=8, height=8, units="in", res=220)

x<-boxplot(airtemp_c ~ Trees.+day, beside=TRUE,data = jundat,
           col = cols, xaxs = n, names=c("No Trees","Trees","No Trees","Trees"),
           xlab="", ylab="Air Temperature (°C)", cex.axis=1.3, cex.lab=1.3, cex.names=1.3, range=0)
axis(1,c("Night","Day"), at=c(1.5,3.5), tick=FALSE, line=2, cex.axis=1.5)
#legend("topleft", legend = c("No Trees","Trees"),fill = cols, bty="n")

dev.off()
sum22dat$day.Trees<-paste(sum22dat$day, sum22dat$Trees., sep=",")
summm4afig<-lmer(airtemp_c~-1+day.Trees +Elevation+ (1|date)+(1|Pole_No), data=sum22dat)

fixs<-fixef(summm4afig)[1:4]
error<-confint(summm4afig)[4:7,]

png(file="figs/Fig2daynighteffects.png",width =3000, height =1500 ,res =300)
x<-barplot(fixs, col=c("gray", "dark green","gray", "dark green"), lwd=2,ylim=c(0,25), 
           cex.lab=1.3,cex.axis=1.3,cex.names=1.4,,ylab="Temperature (°C)",names.arg=c("No Trees","Trees","No Trees","Trees"))

for(i in 1:length(fixs)){
  arrows(x[i],error[i,1],x[i],error[i,2], code=3, angle=90, length=0.05,  lwd=2)
}
axis(side=1,at=c(1.5,3.25), labels=c("Night","Day"), line=2,cex.axis=1.5, lty=0)
abline(h=0)
dev.off() 

#Calculate number of hours above regulated heat thresholds of 80F(26.7C) and 90F(32.2C)
ht<-26.7
sum22dat$heattrigger<-0
sum22dat$heattrigger[which(sum22dat$airtemp_c>=ht)]<-1
heattrighrs<-aggregate(sum22dat$heattrigger, by=list(sum22dat$Hobo_SN,sum22dat$Trees.,sum22dat$cc.field), sum)

colnames(heattrighrs)<-c("Hobo_SN","Trees.","cc.field","heattrig_hrsjun")

#Fit a binomial model with probability of heatrigger as response, trees or cc as explanatory variable

sumdaydat<-sum22dat[sum22dat$day==1,]
sumdaydat.nona<-sumdaydat[-which(is.na(sumdaydat$cc.rem)),]#used this to test how Total BA compared to canopy cover, remote data- it was not as good a predictor
sumdat.nona<-sumdat[!which(is.na(sumdat$TotalBA_cm2)),]#used this to test how Total BA compared to canopy cover, remote data- it was not as good a predictor
d=sumdaydat
summod0<-glm(heattrigger~1, family="binomial",data=d)
summary(summod0)
summod<-glm(heattrigger~Trees.+Elevation, family="binomial",data=d)
summodc<-glm(heattrigger~Trees.+Elevation+day+Trees.:day, family="binomial",data=d)

summod1<-glm(heattrigger~TotalBA_cm2+Elevation, family="binomial",data=d)

summod2<-glm(heattrigger~cc.field+Elevation, family="binomial",data=d)
summod2x<-glm(heattrigger~cc.field, family="binomial",data=d)
summod2c<-glm(heattrigger~cc.field+Elevation+cc.field:Elevation, family="binomial",data=d)

summod3<-glm(heattrigger~cc.rem+Elevation, family="binomial",data=d)
summod3x<-glm(heattrigger~cc.rem, family="binomial",data=d)
summod3c<-glm(heattrigger~cc.rem+Elevation+cc.rem:Elevation, family="binomial",data=d)

summod4<-glm(heattrigger~X3CoarseVeg.20mProp+Elevation+X3CoarseVeg.20mProp:Elevation, family="binomial",data=d)
summod5<-glm(heattrigger~X3CoarseVeg.30mProp+Elevation+X3CoarseVeg.30mProp:Elevation, family="binomial",data=d)
summod6<-glm(heattrigger~X3CoarseVeg.40mProp+Elevation+X3CoarseVeg.40mProp:Elevation, family="binomial",data=d)
summod7<-glm(heattrigger~X3CoarseVeg.50mProp+Elevation+X3CoarseVeg.50mProp:Elevation, family="binomial",data=d)
summod8<-glm(heattrigger~X3CoarseVeg.100mProp+Elevation, family="binomial",data=d)
summod9<-glm(heattrigger~X3CoarseVeg.200mProp+Elevation, family="binomial",data=d)
summod10<-glm(heattrigger~X3CoarseVeg.400mProp+Elevation, family="binomial",data=d)
summod11<-glm(heattrigger~X3CoarseVeg.800mProp+Elevation, family="binomial",data=d)

aictab<-AIC(summod,summodc,summod1,summod2,summod2x,summod2c,summod3,summod3x,summod3c,summod4,summod5,summod6, summod7, summod8, summod9, summod10,summod11)

aictab<-aictab[order(aictab$AIC),]
aictab
summary(summod2c)#lowest aic for sum22dat
tab_model(summod2c)#lowest aic for sum22dat


xcanopy <- seq(from =1,to=100,by=1)
elev<-rep(40, times=length(xcanopy))
#Now we use the predict() function to create the model for all of the values of xweight.

yprob <- predict(summod2c, list(cc.field= xcanopy, Elevation=elev),type="response")


#Make a plot
heattab<-table(d$heattrigger,d$cc.field)
heatprob<-heattab[2,]/(heattab[1,]+heattab[2,])

#total probability and number of days
sum(d$heattrigger)/length(d$heattrigger)

inv.logit(coef(summod0))
inv.logit(confint(summod0))
#how many days had temps over heattreeger threshold?
length(unique(d$date[d$heattrigger==1]))#42
length(unique(d$Hobo_SN[d$heattrigger==1]))#51

table(d$Hobo_SN[d$heattrigger==1],d$date[d$heattrigger==1],d$heattrigger[d$heattrigger==1])

#seprob <- predict(heatprob, list(cc.field = as.numeric(colnames(heattab)), Elevation=xelev),type="response", se.fit=TRUE)
if(ht==26.7){png("figs/probheattrigday26.7C.png", width=8, height=8, units="in", res=320)}
if(ht==32.2){png("figs/probheattrigday32.2C.png", width=8, height=8, units="in", res=320)}
par(mar=c(4,10,4,1))

plot(as.numeric(colnames(heattab)), heatprob, pch = 16, col=alpha("black",.5),
     cex=1.8, cex.axis=1.5, cex.lab=1.8,
     xlab = "Tree Canopy Cover (%)", 
     ylab = "Probability of Temperature >26.7°C",
     ylim=c(0,.2),
     xlim=c(0,100),
     bty="l")

lines(xcanopy, yprob, lwd=3)
#Now we use the predict() function to show effect at different elevations

elev<-rep(60, times=length(xcanopy))
yprob <- predict(summod2c, list(cc.field= xcanopy, Elevation=elev),type="response")
lines(xcanopy, yprob, lwd=3, col="gray")

elev<-rep(80, times=length(xcanopy))
yprob <- predict(summod2c, list(cc.field= xcanopy, Elevation=elev),type="response")
lines(xcanopy, yprob, lwd=3, col="lightgray")

dev.off()

heattab<-table(d$heattrigger,d$cc.field)
heatprob<-heattab[2,]/(heattab[1,]+heattab[2,])

predict(summod2c, list(cc.field= 100, Elevation=40),type="response",se.fit=TRUE)#0.00581963 for 90
predict(summod2c, list(cc.field= 50, Elevation=40),type="response",se.fit=TRUE)#0.0141184 for 90 
predict(summod2c, list(cc.field= 0, Elevation=40),type="response",se.fit=TRUE)#0.03384827 for 90

#do some forecasting of effects of warming
d$airtemp_plus.1<-d$airtemp_c+.1
d$airtemp_plus.2<-d$airtemp_c+.2
d$airtemp_plus.3<-d$airtemp_c+.3
d$airtemp_plus.4<-d$airtemp_c+.4
d$airtemp_plus.5<-d$airtemp_c+.5
d$airtemp_plus1<-d$airtemp_c+1.5
d$airtemp_plus1.5<-d$airtemp_c+2
d$airtemp_plus2<-d$airtemp_c+2
d$heattrigger_plus1<-0
d$heattrigger_plus1[which(d$airtemp_plus1>ht)]<-1
d$heattrigger_plus1.5<-0
d$heattrigger_plus1.5[which(d$airtemp_plus1>ht)]<-1

d$heattrigger_plus2<-0
d$heattrigger_plus2[which(d$airtemp_plus2>=ht)]<-1

d$heattrigger_plus.1<-0
d$heattrigger_plus.1[which(d$airtemp_plus.1>ht)]<-1
d$heattrigger_plus.2<-0
d$heattrigger_plus.2[which(d$airtemp_plus.2>ht)]<-1
d$heattrigger_plus.3<-0
d$heattrigger_plus.3[which(d$airtemp_plus.3>ht)]<-1
d$heattrigger_plus.4<-0
d$heattrigger_plus.4[which(d$airtemp_plus.4>ht)]<-1
d$heattrigger_plus.5<-0
d$heattrigger_plus.5[which(d$airtemp_plus.5>ht)]<-1

heattab.5<-table(d$heattrigger_plus.5,d$cc.field)
heatprob.5<-heattab.5[2,]/(heattab.5[1,]+heattab.5[2,])
heattab1<-table(d$heattrigger_plus1,d$cc.field)
heatprob1<-heattab1[2,]/(heattab1[1,]+heattab1[2,])
heattab1.5<-table(d$heattrigger_plus.5,d$cc.field)
heatprob1.5<-heattab.5[2,]/(heattab.5[1,]+heattab.5[2,])

heattab2<-table(d$heattrigger_plus2,d$cc.field)
heatprob2<-heattab2[2,]/(heattab2[1,]+heattab2[2,])

#total probability and number of days
heatwarm<-c(sum(d$heattrigger)/length(d$heattrigger),
    sum(d$heattrigger_plus.1)/length(d$heattrigger),
    sum(d$heattrigger_plus.2)/length(d$heattrigger),
    sum(d$heattrigger_plus.3)/length(d$heattrigger),
    sum(d$heattrigger_plus.4)/length(d$heattrigger),
    sum(d$heattrigger_plus.5)/length(d$heattrigger),
    sum(d$heattrigger_plus1)/length(d$heattrigger),
    sum(d$heattrigger_plus1.5)/length(d$heattrigger),
    sum(d$heattrigger_plus2)/length(d$heattrigger))
    
length(unique(d$date[d$heattrigger==1]))#11 for 90, 42 for 80
length(unique(d$date[d$heattrigger_plus.1==1]))#11 for 90, 42 for 80
length(unique(d$date[d$heattrigger_plus.2==1]))#12 for 90, 42 for 80
length(unique(d$date[d$heattrigger_plus.3==1]))#15 for 90, 42 for 80
length(unique(d$date[d$heattrigger_plus.4==1]))#12 for 90, 43 for 80
length(unique(d$date[d$heattrigger_plus.5==1]))#13 for 90, 44 for 80
length(unique(d$date[d$heattrigger_plus1==1]))#20 for 90, 52 for 80
length(unique(d$date[d$heattrigger_plus1.5==1]))#20 for 90, 52 for 80
length(unique(d$date[d$heattrigger_plus2==1]))#23 for 90, 55 for 80
summod0plus2<-glm(heattrigger_plus2~1, family="binomial",data=d)
inv.logit(coef(summod0plus2))
inv.logit(confint(summod0plus2))

#############################################################
###############Effect of trees on hourly temperature across the summer
##########################################################################
sum22dat$airtemp_f<-(sum22dat$airtemp_c*(9/5))+32
summm4a<-lmer(airtemp_c~Trees.*day+Elevation+ (1|date)+(1|Pole_No), data=sum22dat)
summm4ax<-lmer(airtemp_c~Trees.*day+ (1|date)+(1|Pole_No), data=sum22dat)
summm4az<-lmer(airtemp_c~Trees.+day+Elevation+Trees.:day+Trees.:Elevation + (1|date)+(1|Pole_No), data=sum22dat)

summm4b<-lmer(airtemp_c~TotalBA_m2*day+Elevation+ (1|date)+(1|Pole_No), data=sum22dat)
summm4bx<-lmer(airtemp_c~TotalBA_m2*day+ (1|date)+(1|Pole_No), data=sum22dat)
summm4bz<-lmer(airtemp_c~TotalBA_m2+day+Elevation+TotalBA_m2:day+TotalBA_m2:Elevation + (1|date)+(1|Pole_No), data=sum22dat)

summm4c<-lmer(airtemp_c~cc.field*day+Elevation+ (1|date)+(1|Pole_No), data=sum22dat)
summm4c1<-lmer(airtemp_c~cc.field*Elevation+ day+(1|date)+(1|Pole_No), data=sum22dat)
summm4cx<-lmer(airtemp_c~cc.field*day+ (1|date)+(1|Pole_No), data=sum22dat)
summm4cz<-lmer(airtemp_c~cc.field+day+Elevation+cc.field:day+cc.field:Elevation + (1|date)+(1|Pole_No), data=sum22dat)

summm4r<-lmer(airtemp_c~cc.rem*day+Elevation+ (1|date)+(1|Pole_No), data=sum22dat)
summm4rx<-lmer(airtemp_c~cc.rem*day+ (1|date)+(1|Pole_No), data=sum22dat)
summm4rz<-lmer(airtemp_c~cc.rem+day+Elevation+cc.rem:day+cc.rem:Elevation + (1|date)+(1|Pole_No), data=sum22dat)

summm4n<-lmer(airtemp_c~ NumTrees*day+Elevation+ (1|date)+(1|Pole_No), data=sum22dat)
summm4nx<-lmer(airtemp_c~ NumTrees*day+ (1|date)+(1|Pole_No), data=sum22dat)
summm4nz<-lmer(airtemp_c~NumTrees+day+Elevation+NumTrees:day+NumTrees:Elevation + (1|date)+(1|Pole_No), data=sum22dat)

aictab<-AIC(summm4a,summm4ax,summm4az,summm4b, summm4bx,summm4bz, summm4c,summm4c1,summm4cx, summm4cz,summm4r,summm4rx,summm4rz,summm4n,summm4nx,summm4nz)
aictab<-aictab[order(aictab$AIC),]
aictab

aictab.forms<-AIC(summm4a,summm4b, summm4c,summm4n)
aictab.forms<-aictab.forms[order(aictab.forms$AIC),]
aictab.forms


tab_model(summm4r, digits=4)
tab_model(summm4c, digits=4)
tab_model(summm4b, digits=4)
tab_model(summm4a, digits=4)
tab_model(summm4n, digits=6)

round(sqrt(mean(summary(summm4r)$residuals^2)), digits=4)
round(sqrt(mean(summary(summm4c)$residuals^2)), digits=4)
round(sqrt(mean(summary(summm4b)$residuals^2)), digits=4)
round(sqrt(mean(summary(summm4a)$residuals^2)), digits=6)
round(sqrt(mean(summary(summm4n)$residuals^2)), digits=6)


#make comparison table
rws<-c(1,2,4,5,6,7,8)
hrlymodsumtab<-round(cbind(
  c(as.numeric(unname(summary(summm4c)$var)),fixef(summm4c)),confint(summm4c)[rws,], 
  c(as.numeric(unname(summary(summm4c)$var)),fixef(summm4a)), confint(summm4a)[rws,], 
  c(as.numeric(unname(summary(summm4c)$var)),fixef(summm4b)), confint(summm4b)[rws,], 
  c(as.numeric(unname(summary(summm4c)$var)),fixef(summm4n)), confint(summm4n)[rws,]), digits=4)

colnames(hrlymodsumtab)<-c("Canopy Cover (%)","2.5%","97.5%","Presence", "2.5%","97.5%","Basal Area (m2)","2.5%","97.5%","Number","2.5%","97.5 %")
write.csv(hrlymodsumtab, "output/hrlymodcomp.csv")

summm4af<-lmer(airtemp_f~Trees.*day+Elevation+ (1|date)+(1|Pole_No), data=sum22dat)
summary(summm4af)
summary(summm4a)

#calculate variation between temperature readings at any one time
sum22dat$date.time<-as.factor(paste(sum22dat$date,sum22dat$time,sep="."))

varmod<-lmer(airtemp_c~day+(1|date.time), data=sum22dat)
summary(varmod)
hrlymax<-aggregate(sum22dat$airtemp_c, by=list(sum22dat$date, sum22dat$hour), max)
hrlymin<-aggregate(sum22dat$airtemp_c, by=list(sum22dat$date, sum22dat$hour), min)
hrlymaxmin<-cbind(hrlymax,hrlymin$x)
colnames(hrlymaxmin)<-c("date","time","tmax","tmin")
hrlymaxmin$tdif<-hrlymaxmin$tmax-hrlymaxmin$tmin
round(range(hrlymaxmin$tdif), digits=3)
round(mean(hrlymaxmin$tdif), digits=3)#5.74
hrlymaxmin$day<-0
hrlymaxmin$day[hrlymaxmin$time>6 & hrlymaxmin$time<19]<-1

tdifmod<-lm(tdif~day, data=hrlymaxmin)
summary(tdifmod)

hrlymaxday<-aggregate(sum22dat$airtemp_c[sum22dat$day==1], by=list(sum22dat$date[sum22dat$day==1], sum22dat$hour[sum22dat$day==1]), max)
hrlyminday<-aggregate(sum22dat$airtemp_c[sum22dat$day==1], by=list(sum22dat$date[sum22dat$day==1], sum22dat$hour[sum22dat$day==1]), min)
hrlymaxminday<-cbind(hrlymaxday,hrlyminday$x)
colnames(hrlymaxminday)<-c("date","time","tmax","tmin")
hrlymaxminday$tdif<-hrlymaxminday$tmax-hrlymaxminday$tmin
round(range(hrlymaxminday$tdif), digits=3)
round(mean(hrlymaxminday$tdif), digits=3)#5.583416

hrlymaxnight<-aggregate(sum22dat$airtemp_c[sum22dat$day==0], by=list(sum22dat$date[sum22dat$day==0], sum22dat$hour[sum22dat$day==0]), max)
hrlyminnight<-aggregate(sum22dat$airtemp_c[sum22dat$day==0], by=list(sum22dat$date[sum22dat$day==0], sum22dat$hour[sum22dat$day==0]), min)
hrlymaxminnight<-cbind(hrlymaxnight,hrlyminnight$x)
colnames(hrlymaxminnight)<-c("date","time","tmax","tmin")
hrlymaxminnight$tdif<-hrlymaxminnight$tmax-hrlymaxminnight$tmin
range(hrlymaxminnight$tdif)
mean(hrlymaxminnight$tdif)#5.583416

#make some figures:

############# max, min, mean daily temperature ##############
# #########################################################,
# jundat.max<-aggregate(jundat$airtemp_c, by=list(jundat$Hobo_SN,jundat$Trees.,jundat$TotalBA_m2,jundat$cc.field,jundat$cc.rem, jundat$imp,jundat$Elevation,jundat$X3CoarseVeg.20mProp,jundat$X3CoarseVeg.30mProp,jundat$X3CoarseVeg.40mProp,jundat$X3CoarseVeg.50mProp,jundat$X3CoarseVeg.100mProp,jundat$X3CoarseVeg.200mProp,jundat$X3CoarseVeg.400mProp,jundat$X3CoarseVeg.800mProp,jundat$dom), max)
# jundat.min<-aggregate(jundat$airtemp_c, by=list(jundat$Hobo_SN,jundat$Trees.,jundat$TotalBA_m2,jundat$cc.field,jundat$cc.rem, jundat$imp,jundat$Elevation,jundat$X3CoarseVeg.20mProp,jundat$X3CoarseVeg.30mProp,jundat$X3CoarseVeg.40mProp,jundat$X3CoarseVeg.50mProp,jundat$X3CoarseVeg.100mProp,jundat$X3CoarseVeg.200mProp,jundat$X3CoarseVeg.400mProp,jundat$X3CoarseVeg.800mProp,jundat$dom), min)
# jundat.ave<-aggregate(jundat$airtemp_c, by=list(jundat$Hobo_SN,jundat$Trees.,jundat$TotalBA_m2,jundat$cc.field,jundat$cc.rem, jundat$imp,jundat$Elevation,jundat$X3CoarseVeg.20mProp,jundat$X3CoarseVeg.30mProp,jundat$X3CoarseVeg.40mProp,jundat$X3CoarseVeg.50mProp,jundat$X3CoarseVeg.100mProp,jundat$X3CoarseVeg.200mProp,jundat$X3CoarseVeg.400mProp,jundat$X3CoarseVeg.800mProp,jundat$dom), mean)
# jundat.minmax<-cbind(jundat.max, jundat.min$x,jundat.ave$x)
# colnames(jundat.minmax)<-c("Hobo_SN","Trees.","BA_m2","cc.field","cc.rem", "imp","Elevation","cc.rem20m","cc.rem30m","cc.rem40m","cc.rem50m","cc.rem100m","cc.rem200m","cc.rem400m","cc.rem800m","dom","T_max","T_min","T_ave")
#sum22datnoext<-sum22dat[-which(sum22dat$airtemp_c>36),]#inaccurate temperature readings?

sumdat.max<-aggregate(sum22dat$airtemp_c, by=list(sum22dat$Hobo_SN,sum22dat$Pole_No, sum22dat$Trees.,sum22dat$TotalBA_m2,sum22dat$cc.field,sum22dat$cc.rem,sum22dat$imp,sum22dat$Elevation,sum22dat$X3CoarseVeg.20mProp,sum22dat$X3CoarseVeg.30mProp,sum22dat$X3CoarseVeg.40mProp,sum22dat$X3CoarseVeg.50mProp,sum22dat$X3CoarseVeg.100mProp,sum22dat$X3CoarseVeg.200mProp,sum22dat$X3CoarseVeg.400mProp,sum22dat$X3CoarseVeg.800mProp,sum22dat$date), max)
sumdat.min<-aggregate(sum22dat$airtemp_c, by=list(sum22dat$Hobo_SN,sum22dat$Pole_No, sum22dat$Trees.,sum22dat$TotalBA_m2,sum22dat$cc.field,sum22dat$cc.rem,sum22dat$imp,sum22dat$Elevation,sum22dat$X3CoarseVeg.20mProp,sum22dat$X3CoarseVeg.30mProp,sum22dat$X3CoarseVeg.40mProp,sum22dat$X3CoarseVeg.50mProp,sum22dat$X3CoarseVeg.100mProp,sum22dat$X3CoarseVeg.200mProp,sum22dat$X3CoarseVeg.400mProp,sum22dat$X3CoarseVeg.800mProp,sum22dat$date), min)
sumdat.ave<-aggregate(sum22dat$airtemp_c, by=list(sum22dat$Hobo_SN,sum22dat$Pole_No, sum22dat$Trees.,sum22dat$TotalBA_m2,sum22dat$cc.field,sum22dat$cc.rem,sum22dat$imp,sum22dat$Elevation,sum22dat$X3CoarseVeg.20mProp,sum22dat$X3CoarseVeg.30mProp,sum22dat$X3CoarseVeg.40mProp,sum22dat$X3CoarseVeg.50mProp,sum22dat$X3CoarseVeg.100mProp,sum22dat$X3CoarseVeg.200mProp,sum22dat$X3CoarseVeg.400mProp,sum22dat$X3CoarseVeg.800mProp,sum22dat$date), mean)
sumdat.minmax<-cbind(sumdat.max, sumdat.min$x, sumdat.ave$x)
colnames(sumdat.minmax)<-c("Hobo_SN","Pole_No","Trees.","BA_m2","cc.field","cc.rem", "imp","Elevation","cc.rem20m","cc.rem30m","cc.rem40m","cc.rem50m","cc.rem100m","cc.rem200m","cc.rem400m","cc.rem800m","date","T_max","T_min","T_ave")
colnames(sumdat.ave)<-c("Hobo_SN","Pole_No","Trees.","BA_m2","cc.field","cc.rem", "imp","Elevation","cc.rem20m","cc.rem30m","cc.rem40m","cc.rem50m","cc.rem100m","cc.rem200m","cc.rem400m","cc.rem800m","date","T_ave")

sumdat.minmax$Trange<-sumdat.minmax$T_max-sumdat.minmax$T_min
#which date has the max average T?
sumdat.minmax$date[which(sumdat.minmax$T_ave==max(sumdat.minmax$T_ave))]
#1] "2022-07-30"

#prep rem sensed data to be on the same scale
sumdat.minmax$cc.rem20m<-sumdat.minmax$cc.rem20m*100
sumdat.minmax$cc.rem30m<-sumdat.minmax$cc.rem30m*100
sumdat.minmax$cc.rem40m<-sumdat.minmax$cc.rem40m*100
sumdat.minmax$cc.rem50m<-sumdat.minmax$cc.rem50m*100

sumdat.minmax$Pole_No<-as.factor(sumdat.minmax$Pole_No)
#standardize predictors
sumdat.minmax$cc.rem.st<-(sumdat.minmax$cc.rem-mean(sumdat.minmax$cc.rem)/sd(sumdat.minmax$cc.rem))
sumdat.minmax$cc.field.st<-(sumdat.minmax$cc.field-mean(sumdat.minmax$cc.field)/sd(sumdat.minmax$cc.field))
sumdat.minmax$Elevation.st<-(sumdat.minmax$Elevation-mean(sumdat.minmax$Elevation)/sd(sumdat.minmax$Elevation))
sumdat.minmax$cc.rem20m.st<-(sumdat.minmax$cc.rem20m-mean(sumdat.minmax$cc.rem20m)/sd(sumdat.minmax$cc.rem20m))
sumdat.minmax$cc.rem30m.st<-(sumdat.minmax$cc.rem30m-mean(sumdat.minmax$cc.rem30m)/sd(sumdat.minmax$cc.rem30m))
sumdat.minmax$cc.rem40m.st<-(sumdat.minmax$cc.rem40m-mean(sumdat.minmax$cc.rem40m)/sd(sumdat.minmax$cc.rem40m))
sumdat.minmax$cc.rem50m.st<-(sumdat.minmax$cc.rem50m-mean(sumdat.minmax$cc.rem50m)/sd(sumdat.minmax$cc.rem50m))
#sumdat.minmax$cc.rem100m.st<-(sumdat.minmax$cc.rem100m-mean(sumdat.minmax$cc.rem100m)/sd(sumdat.minmax$cc.rem100m))
#sumdat.minmax$cc.rem200m.st<-(sumdat.minmax$cc.rem200m-mean(sumdat.minmax$cc.rem200m)/sd(sumdat.minmax$cc.rem200m))
#sumdat.minmax$cc.rem400m.st<-(sumdat.minmax$cc.rem400m-mean(sumdat.minmax$cc.rem400m)/sd(sumdat.minmax$cc.rem400m))
#sumdat.minmax$cc.rem800m.st<-(sumdat.minmax$cc.rem800m-mean(sumdat.minmax$cc.rem800m)/sd(sumdat.minmax$cc.rem800m))

#Before looking at temperature, look at field-collected canopy vs remote sensed land cover and ploty
png("figs/remote_cover_scale.png", width=6, height=6, units="in", res=220)
#pdf("figs/remote_field_cover.pdf", width=6, height=6)
plot(field,rem, pch=16, col="gray",
     xlab="Field Canopy Cover",
     ylab="Remote-sensed Vegetation Cover", 
     bty="l",cex=1.5,cex.axis=1.5,cex.lab=1.5)
# main="Remote sensed vs Field Estimates of Tree Cover")
r<-lm(rem~field)
abline(r,lwd=4)
x<-seq(1,100, by=1)
lines(x,x, lty=2, lwd=4)
mtext(paste(" r2=",round(summary(r)$r.squared, digits=3),", p<0.001", sep=""), side=3, line=-1, adj=0)
#text(field,rem,labels=as.character(locs3$WptNo))#check which sites are off

dev.off()

##Merge in vc data with min max from grit neighborhood
sumdat.minmax$date<-as.character(sumdat.minmax$date)
colnames(vc)[2]<-"date"
sumdat.minmax<-left_join(sumdat.minmax,vc, by="date")
#get anomoalys
sumdat.minmax$tmaxanom<-sumdat.minmax$T_max-sumdat.minmax$tempmax
sumdat.minmax$tminanom<-sumdat.minmax$T_min-sumdat.minmax$tempmin
sumdat.minmax$trananom<-sumdat.minmax$Trange-(sumdat.minmax$tempmax-sumdat.minmax$tempmin)
sumdat.minmax$date<-as.factor(sumdat.minmax$date)
hist(sumdat.minmax$tmaxanom)
range(sumdat.minmax$tmaxanom)
mean(sumdat.minmax$tmaxanom)
mean(sumdat.minmax$tminanom)
length(sumdat.minmax$tmaxanom[which(sumdat.minmax$tmaxanom< -3)])#appears to be only one outlier
sumdat.minmax[sumdat.minmax$tmaxanom< -3,]
length(unique(sumdat.minmax$date[which(sumdat.minmax$tmaxanom<0)]))
length(unique(sumdat.minmax$date))
length(sumdat.minmax$date[which(sumdat.minmax$tmaxanom<0)])
length(sumdat.minmax$date[which(sumdat.minmax$tmaxanom>0)])

#Find best random effects structure 
# sumTmaxc1<-lmer(tmaxanom~cc.field.st+Elevation.st+cc.field.st:Elevation.st +(1|Hobo_SN), data=sumdat.minmax, REML=TRUE)
# sumTmaxc2<-lmer(tmaxanom~cc.field.st+Elevation.st+cc.field.st:Elevation.st +(1|date), data=sumdat.minmax, REML=TRUE)
# sumTmaxc3<-lmer(tmaxanom~cc.field.st+Elevation.st+cc.field.st:Elevation.st +(cc.field.st|date), data=sumdat.minmax, REML=TRUE)
# sumTmaxc4<-lmer(tmaxanom~cc.field.st+Elevation.st+cc.field.st:Elevation.st +(1|date)+(1|Hobo_SN), data=sumdat.minmax, REML=TRUE)
# AIC(sumTmaxc1,sumTmaxc2, sumTmaxc3, sumTmaxc4)
# 
# summary(sumTmaxc4)

#Tmax
sum
sumTmaxc<-lmer(tmaxanom~cc.field+Elevation+cc.field:Elevation+(1|date) +(1|Pole_No), data=sumdat.minmax)
sumTmaxr<-lmer(tmaxanom~cc.rem+Elevation+cc.rem:Elevation+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaximp<-lmer(tmaxanom~imp+Elevation+imp:Elevation+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxcnoelev<-lmer(tmaxanom~cc.field+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxrnoelev<-lmer(tmaxanom~cc.rem+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaximpnoelev<-lmer(tmaxanom~imp+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxrcc<-lmer(tmaxanom~cc.rem*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaximpcc<-lmer(tmaxanom~imp*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmax)

sumTmaxccc<-lmer(tmaxanom~cc.field*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmax)

sumTmaxrhum<-lmer(tmaxanom~cc.rem*humidity+(1|date)+(1|Pole_No), data=sumdat.minmax)

sumTmaximphum<-lmer(tmaxanom~imp*humidity+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxchum<-lmer(tmaxanom~cc.field*humidity+(1|date)+(1|Pole_No), data=sumdat.minmax)

aictab<-AIC(sumTmaxc,sumTmaxr,sumTmaximp,sumTmaxcnoelev,sumTmaxrnoelev,sumTmaximpnoelev,sumTmaxrcc,sumTmaximpcc,sumTmaxccc,sumTmaxrhum,sumTmaximphum,sumTmaxchum)

aictab<-aictab[order(aictab$AIC),]
aictab
tab_model(sumTmaxrnoelev, digits=3)
tab_model(sumTmaxcnoelev, digits=3)
sumdat.minmax$T_max_F<-(sumdat.minmax$T_max* 9/5) + 32 
Anova(sumTmaxccc, type="III")

#now compare canopy at different distances away, and remote vs field
sumTmaxr2.10m<-lmer(T_max~cc.rem+(1|date)+(1|Pole_No), data=sumdat.minmax)


sumTmaxr2.20m<-lmer(T_max~cc.rem20m +(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxr2.30m<-lmer(T_max~cc.rem30m +(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxr2.40m<-lmer(T_max~cc.rem40m +(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxr2.50m<-lmer(T_max~cc.rem50m+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxr2.100m<-lmer(T_max~cc.rem100m+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxr2.200m<-lmer(T_max~cc.rem200m+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxr2.400m<-lmer(T_max~cc.rem400m+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxr2.800m<-lmer(T_max~cc.rem800m +(1|date)+(1|Pole_No), data=sumdat.minmax)

sumTmaxrc2.10m<-lmer(T_max~cc.field+cc.rem+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxrc2.20m<-lmer(T_max~cc.field+cc.rem20m+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxrc2.30m<-lmer(T_max~cc.field+cc.rem30m+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxrc2.40m<-lmer(T_max~cc.field+cc.rem40m+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxrc2.50m<-lmer(T_max~cc.field+cc.rem50m+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxrc2.100m<-lmer(T_max~cc.field+cc.rem100m+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxrc2.200m<-lmer(T_max~cc.field+cc.rem200m +(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxrc2.400m<-lmer(T_max~cc.field+cc.rem400m +(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxrc2.800m<-lmer(T_max~cc.field+cc.rem800m +(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmaxrc2int.800m<-lmer(T_max~cc.field+cc.rem800m+cc.field:cc.rem800m+(1|date)+(1|Hobo_SN), data=sumdat.minmax)
aictab<-AIC(sumTmaxc,sumTmaxr,sumTmaximp,sumTmaxcnoelev,sumTmaxrnoelev,sumTmaximpnoelev,sumTmaxrcc,sumTmaximpcc,sumTmaxccc,sumTmaxrhum,sumTmaximphum,sumTmaxchum,
            sumTmaxr2.10m,sumTmaxr2.20m,sumTmaxr2.30m,sumTmaxr2.40m,sumTmaxr2.50m,
            sumTmaxrc2.10m,sumTmaxrc2.20m,sumTmaxrc2.30m,sumTmaxrc2.40m,sumTmaxrc2.50m
)
aictab<-aictab[order(aictab$AIC),]
aictab
#bestfit
summary(sumTmaxrnoelev)
tab_model(sumTmaxrnoelev, digits=3)
#for comparison, second best- field measured cc


#Tmin
sumTminc<-lmer(tminanom~cc.field+Elevation+cc.field:Elevation+(1|date) +(1|Pole_No), data=sumdat.minmax)
sumTminr<-lmer(tminanom~cc.rem+Elevation+cc.rem:Elevation+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTminimp<-lmer(tminanom~imp+Elevation+imp:Elevation+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTmincnoelev<-lmer(tminanom~cc.field+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTminrnoelev<-lmer(tminanom~cc.rem+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTminimpnoelev<-lmer(tminanom~imp+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTminrcc<-lmer(tminanom~cc.rem*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTminimpcc<-lmer(tminanom~imp*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTminccc<-lmer(tminanom~cc.field*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTminrhum<-lmer(tminanom~cc.rem*humidity+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTminimphum<-lmer(tminanom~imp*humidity+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTminchum<-lmer(tminanom~cc.field*humidity+(1|date)+(1|Pole_No), data=sumdat.minmax)

aictab<-AIC(sumTminc,sumTminr,sumTminimp,sumTmincnoelev,sumTminrnoelev,sumTminimpnoelev,sumTminrcc,sumTminimpcc,sumTminccc,sumTminrhum,sumTminimphum,sumTminchum)

aictab<-aictab[order(aictab$AIC),]
aictab
tab_model(sumTminimpcc, digits=3)
tab_model(sumTminccc, digits=3)
sumdat.minmax$T_max_F<-(sumdat.minmax$T_max* 9/5) + 32 
Anova(sumTminccc, type="III")

aictab<-aictab[order(aictab$AIC),]
aictab
#bestfit
summary(sumTminccc)
tab_model(sumTminccc, digits=3)
#for comparison, second best- field measured cc


#compare nonlinear vs linear fit
#fit nonlinear model in brms
plot(sumTmincnoelev)
plot(sumTmaxcnoelev)

#now compare canopy at different distances away, and remote vs field
sumTminr2.10m<-lmer(tminanom~cc.rem+(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTminr2.20m<-lmer(tminanom~cc.rem20m +(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTminr2.30m<-lmer(tminanom~cc.rem30m +(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTminr2.40m<-lmer(tminanom~cc.rem40m +(1|date)+(1|Pole_No), data=sumdat.minmax)
sumTminr2.50m<-lmer(tminanom~cc.rem50m+(1|date)+(1|Pole_No), data=sumdat.minmax)

aictab<-AIC(sumTminc,sumTminr,sumTminimp,sumTmincnoelev,sumTminrnoelev,sumTminimpnoelev,sumTminrcc,sumTminimpcc,sumTminccc,sumTminrhum,sumTminimphum,sumTminchum,
            sumTminr2.10m,sumTminr2.20m,sumTminr2.30m,sumTminr2.40m,sumTminr2.50m,
            sumTminrc2.10m,sumTminrc2.20m,sumTminrc2.30m,sumTminrc2.40m,sumTminrc2.50m
)
aictab<-aictab[order(aictab$AIC),]
aictab
#bestfit
summary(sumTminccc)
tab_model(sumTminccc, digits=3)
tab_model(sumTminrcc, digits=3)

#for comparison, second best- field measured cc

#for comparison, second remote sensed cc

summary(sumTminrcc)
tab_model(sumTminrcc, digits=3)

#Models to present in main ms
tab_model(sumTmaxccc, digits=3)
tab_model(sumTmaxrcc, digits=3)
tab_model(sumTminccc, digits=3)
tab_model(sumTminrcc, digits=3)



png("figs/maxmintemp_cc_mod.png", width=10, height=10, units="in", res=260)
par(mfrow=c(2,2), mar=c(4,8,4,1))

plot(sumdat.minmax$cc.field ,sumdat.minmax$tmaxanom, 
     pch=16,col=alpha("darkgreen", .2),
     #ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     main="Maximum temperature",ylim=c(-2,4),
     xlab="Tree cover (%), field measured",ylab=c("Temperature (°C)"), bty="l")
abline(fixef(sumTmaxcnoelev)[1:2], lwd=4)#effect of trees in sun, impervious surface
mtext("(a)", side=3,line=1,adj=-.2)
#mtext("Field data", side=2, line=10, adj=.5, las=2, cex=1.5)

plot(sumdat.minmax$cc.field ,sumdat.minmax$tminanom, 
     pch=16,col=alpha("darkgreen", .2),
     ylim=c(-2,4),,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     main="Minimum temperature",
     xlab="Tree cover (%), field measured",ylab=c("Temperature (°C)"),bty="l")
abline(fixef(sumTmincnoelev)[1:2], lwd=4)#effect of trees in sun, impervious surface
mtext("(b)", side=3,line=1,adj=-.2)

plot(sumdat.minmax$cc.rem ,sumdat.minmax$tmaxanom, 
     pch=16,col=alpha("darkgreen", .2),
     #ylim=c(5,40),cex=1.5,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     main="",ylim=c(-2,4),
     xlab="Tree cover (%), remote estimate",ylab=c("Temperature (°C)"), bty="l")
abline(fixef(sumTmaxcnoelev)[1:2], lwd=4)#effect of trees in sun, impervious surface
mtext("(c)", side=3,line=1,adj=-.2)
#mtext("Remote-sensed data", side=2, line=10, adj=.5, las=2, cex=1.5)

plot(sumdat.minmax$cc.rem ,sumdat.minmax$tminanom, 
     pch=16,col=alpha("darkgreen", .2),
     ylim=c(-2,4),,
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,
     main="",
     xlab="Tree cover (%), remote estimate",ylab=c("Temperature (°C)"),bty="l")
abline(fixef(sumTmincnoelev)[1:2], lwd=4)#effect of trees in sun, impervious surface
mtext("(d)", side=3,line=1,adj=-.2)

dev.off()


jul30dat<-sumdat.minmax[sumdat.minmax$date=="2022-07-30",]
jun27dat<-sumdat.minmax[sumdat.minmax$date=="2022-06-27",]
jun26dat<-sumdat.minmax[sumdat.minmax$date=="2022-06-2",]

png(file="figs/tmax.fieldcc.2022jun27.png",width =1800, height =1800 ,res =200)
ylab <- expression(paste("Maximum Temperature (",degree,"C)", sep=""))

ggplot(jun27dat,
       aes(x= cc.field, y = T_max)) +
  geom_point(size = 3,color=alpha("darkgreen", alpha=.8)) +
  theme_classic((base_size = 18)) +
  xlab("Canopy Cover (%)") + ylab("Maximum Temperature (°C)")+
  stat_smooth(method = "lm", color="black",
              method.args = list(family = gaussian))
#theme() 
dev.off()
png(file="figs/tmin.fieldcc.2022jun27.png",width =1800, height =1800 ,res =200)
ylab <- expression(paste("Minimum Temperature (",degree,"C)", sep=""))

ggplot(jun27dat,
       aes(x= cc.field, y = T_min)) +
  geom_point(size = 3,color=alpha("darkgreen", alpha=.8)) +
  theme_classic((base_size = 18)) +
  xlab("Canopy Cover (%)") + ylab("Minimum Temperature (°C)")+
  stat_smooth(method = "lm", color="black",
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
  stat_smooth(method = "lm", 
              method.args = list(family = gaussian))
#theme() 

dev.off()
png(file="figs/tmin.fieldcc.2022jun27.png",width =1800, height =1800 ,res =200)

ggplot(jun27dat,
       aes(x= cc.field, y = T_min)) +
  geom_point() +
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))

dev.off()
sumdat.minmax$month<-substr(sumdat.minmax$date,6,7)
jundat.minmax<-sumdat.minmax[sumdat.minmax$month=="06",]
juldat.minmax<-sumdat.minmax[sumdat.minmax$month=="07",]
augdat.minmax<-sumdat.minmax[sumdat.minmax$month=="08",]

  
png(file="figs/tmin.fieldcc.2022jun.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in unique(jundat.minmax$date)){
daydat<- jundat.minmax[jundat.minmax$date==i,]
m<-lm(T_min~cc.field, data=daydat)
msum<-round(c(summary(m)$adj.r.squared,summary(m)$coef[2,4]), digits=3)
rmse<- round(sqrt(mean(m$residuals^2)), digits=3)
nlm<-gam(T_min~s(cc.field), data=daydat)
nlmsum<-round(c(summary(nlm)$r.sq,summary(nlm)$s.table[4]), digits=3)
nlm.rmse<- round(sqrt(mean(nlm$residuals^2)), digits=3)
maic<-round(AIC(m), digits=3)
gamaic<-round(AIC(nlm), digits=3)
plms<-rbind(plms,c(msum, rmse,maic,nlmsum,nlm.rmse,gamaic))

  p[[i]]<- ggplot(daydat,
       aes(x= cc.field, y = T_min, main=i)) +
  geom_point() +
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))
}
colnames(plms)<-c("lm.r2","lm.p","lm.rmse","lm.aic","gam.r2","gam.p","gam.rmse","gam.aic")
juntmin.field.ccmodcomp<-as.data.frame(plms)

do.call(grid.arrange,p)
dev.off()


png(file="figs/tmax.fieldcc.2022jun.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in unique(jundat.minmax$date)){
  daydat<- jundat.minmax[jundat.minmax$date==i,]
  m<-lm(T_max~cc.field, data=daydat)
  msum<-round(c(summary(m)$adj.r.squared,summary(m)$coef[2,4]), digits=3)
  rmse<- round(sqrt(mean(m$residuals^2)), digits=3)
  nlm<-gam(T_max~s(cc.field), data=daydat)
  nlmsum<-round(c(summary(nlm)$r.sq,summary(nlm)$s.table[4]), digits=3)
  nlm.rmse<- round(sqrt(mean(nlm$residuals^2)), digits=3)
  maic<-round(AIC(m), digits=3)
  gamaic<-round(AIC(nlm), digits=3)
  plms<-rbind(plms,c(msum, rmse,maic,nlmsum,nlm.rmse,gamaic))  
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.field, y = T_max, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
colnames(plms)<-c("lm.r2","lm.p","lm.rmse","lm.aic","gam.r2","gam.p","gam.rmse","gam.aic")
juntmax.field.ccmodcomp<-as.data.frame(plms)
do.call(grid.arrange,p)
dev.off()

png(file="figs/tmax.remcc.2022jun.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in unique(jundat.minmax$date)){
  daydat<- jundat.minmax[jundat.minmax$date==i,]
  m<-lm(T_max~cc.rem, data=daydat)
  msum<-round(c(summary(m)$adj.r.squared,summary(m)$coef[2,4]), digits=3)
  rmse<- round(sqrt(mean(m$residuals^2)), digits=3)
  nlm<-gam(T_max~s(cc.rem), data=daydat)
  nlmsum<-round(c(summary(nlm)$r.sq,summary(nlm)$s.table[4]), digits=3)
  nlm.rmse<- round(sqrt(mean(nlm$residuals^2)), digits=3)
  maic<-round(AIC(m), digits=3)
  gamaic<-round(AIC(nlm), digits=3)
  plms<-rbind(plms,c(msum, rmse,maic,nlmsum,nlm.rmse,gamaic))
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.rem, y = T_max, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
do.call(grid.arrange,p)
dev.off()
colnames(plms)<-c("lm.r2","lm.p","lm.rmse","lm.aic","gam.r2","gam.p","gam.rmse","gam.aic")
juntmax.rem.ccmodcomp<-as.data.frame(plms)

png(file="figs/tmin.remcc.2022jun.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in unique(jundat.minmax$date)){
  daydat<- jundat.minmax[jundat.minmax$date==i,]
  m<-lm(T_min~cc.rem, data=daydat)
  msum<-round(c(summary(m)$adj.r.squared,summary(m)$coef[2,4]), digits=3)
  rmse<- round(sqrt(mean(m$residuals^2)), digits=3)
  nlm<-gam(T_min~s(cc.rem), data=daydat)
  nlmsum<-round(c(summary(nlm)$r.sq,summary(nlm)$s.table[4]), digits=3)
  nlm.rmse<- round(sqrt(mean(nlm$residuals^2)), digits=3)
  maic<-round(AIC(m), digits=3)
  gamaic<-round(AIC(nlm), digits=3)
  plms<-rbind(plms,c(msum, rmse,maic,nlmsum,nlm.rmse,gamaic))
  
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.rem, y = T_min, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
do.call(grid.arrange,p)
dev.off()
colnames(plms)<-c("lm.r2","lm.p","lm.rmse","lm.aic","gam.r2","gam.p","gam.rmse","gam.aic")

juntim.rem.ccmodcomp<-as.data.frame(plms)


#summarize across all days- primarily interested in field data
juntmin.field.ccmodcomp$gamlower<-0
juntmin.field.ccmodcomp$gamlower[juntmin.field.ccmodcomp$gam.rmse<juntmin.field.ccmodcomp$lm.rmse]<-1
juntmin.field.ccmodcomp$gamAIClower<-0
juntmin.field.ccmodcomp$gamAIClower[juntmin.field.ccmodcomp$gam.aic<juntmin.field.ccmodcomp$lm.aic]<-1
juntmax.field.ccmodcomp$gamlower<-0
juntmax.field.ccmodcomp$gamlower[juntmax.field.ccmodcomp$gam.rmse<juntmax.field.ccmodcomp$lm.rmse]<-1
juntmax.field.ccmodcomp$gamAIClower<-0
juntmax.field.ccmodcomp$gamAIClower[juntmax.field.ccmodcomp$gam.aic<juntmax.field.ccmodcomp$lm.aic]<-1

sum(juntmax.field.ccmodcomp$gamlower)#8 days had better nonlinear fits
sum(juntmin.field.ccmodcomp$gamlower)#9 days had better nonlinear fits

#compare ps
length(which(juntmin.field.ccmodcomp$lm.p<0.05))#7
length(which(juntmin.field.ccmodcomp$gam.p<0.05))#8
length(which(juntmax.field.ccmodcomp$lm.p<0.05))#21
length(which(juntmax.field.ccmodcomp$gam.p<0.05))#20

#Select only the hottest day of the year- jul 31 for Tmax, july 31 for Tmin
sumdat.minmax$date[which(sumdat.minmax$T_max==max(sumdat.minmax$T_max))]

jul31dat<-sumdat.minmax[sumdat.minmax$date=="2022-07-31",]

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
png(file="figs/tmin.fieldcc.2022jul31.png",width =1800, height =1800 ,res =200)

ggplot(jul31dat,
       aes(x= cc.field, y = T_min)) +
  geom_point() +
  stat_smooth(method = "gam", 
              method.args = list(family = gaussian))

dev.off()

png(file="figs/tmax.fieldcc.2022jul.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in unique(juldat.minmax$date)){
  daydat<- juldat.minmax[juldat.minmax$date==i,]
  m<-lm(T_max~cc.field, data=daydat)
  msum<-round(c(summary(m)$adj.r.squared,summary(m)$coef[2,4]), digits=3)
  rmse<- round(sqrt(mean(m$residuals^2)), digits=3)
  nlm<-gam(T_max~s(cc.field), data=daydat)
  nlmsum<-round(c(summary(nlm)$r.sq,summary(nlm)$s.table[4]), digits=3)
  nlm.rmse<- round(sqrt(mean(nlm$residuals^2)), digits=3)
  maic<-round(AIC(m), digits=3)
  gamaic<-round(AIC(nlm), digits=3)
  plms<-rbind(plms,c(msum, rmse,maic,nlmsum,nlm.rmse,gamaic))
  
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.field, y = T_max, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
colnames(plms)<-c("lm.r2","lm.p","lm.rmse","lm.aic","gam.r2","gam.p","gam.rmse","gam.aic")
tmaxjul.field.cc<-as.data.frame(plms)

do.call(grid.arrange,p)
dev.off()


png(file="figs/tmin.fieldcc.2022jul.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in unique(juldat.minmax$date)){
  daydat<- juldat.minmax[juldat.minmax$date==i,]
  m<-lm(T_min~cc.field, data=daydat)
  msum<-round(c(summary(m)$adj.r.squared,summary(m)$coef[2,4]), digits=3)
  rmse<- round(sqrt(mean(m$residuals^2)), digits=3)
  nlm<-gam(T_min~s(cc.field), data=daydat)
  nlmsum<-round(c(summary(nlm)$r.sq,summary(nlm)$s.table[4]), digits=3)
  nlm.rmse<- round(sqrt(mean(nlm$residuals^2)), digits=3)
  maic<-round(AIC(m), digits=3)
  gamaic<-round(AIC(nlm), digits=3)
  plms<-rbind(plms,c(msum, rmse,maic,nlmsum,nlm.rmse,gamaic))
  
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.field, y = T_min, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
colnames(plms)<-c("lm.r2","lm.p","lm.rmse","lm.aic","gam.r2","gam.p","gam.rmse","gam.aic")
tminjul.field.cc<-as.data.frame(plms)

do.call(grid.arrange,p)
dev.off()

png(file="figs/tmax.remcc.2022jul.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in unique(juldat.minmax$date)){
  daydat<- juldat.minmax[juldat.minmax$date==i,]
  m<-lm(T_max~cc.rem, data=daydat)
  msum<-round(c(summary(m)$adj.r.squared,summary(m)$coef[2,4]), digits=3)
  rmse<- round(sqrt(mean(m$residuals^2)), digits=3)
  nlm<-gam(T_max~s(cc.rem), data=daydat)
  nlmsum<-round(c(summary(nlm)$r.sq,summary(nlm)$s.table[4]), digits=3)
  nlm.rmse<- round(sqrt(mean(nlm$residuals^2)), digits=3)
  lmaic<-AIC(m)
  gamaic<-AIC(nlm)
  plms<-rbind(plms,c(msum, rmse,lmaic,nlmsum,nlm.rmse,gamaic))
  
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.rem, y = T_max, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
do.call(grid.arrange,p)
dev.off()
colnames(plms)<-c("lm.r2","lm.p","lm.rmse","lm.aic","gam.r2","gam.p","gam.rmse","gam.aic")
tmaxjul.rem.cc<-as.data.frame(plms)

png(file="figs/tmin.remcc.2022jul.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in unique(juldat.minmax$date)){
  daydat<- juldat.minmax[juldat.minmax$date==i,]
  m<-lm(T_min~cc.rem, data=daydat)
  msum<-round(c(summary(m)$adj.r.squared,summary(m)$coef[2,4]), digits=3)
  rmse<- round(sqrt(mean(m$residuals^2)), digits=3)
  nlm<-gam(T_min~s(cc.rem), data=daydat)
  nlmsum<-round(c(summary(nlm)$r.sq,summary(nlm)$s.table[4]), digits=3)
  nlm.rmse<- round(sqrt(mean(nlm$residuals^2)), digits=3)
  lmaic<-round(AIC(m), digits=3)
  gamaic<-round(AIC(nlm), digits=3)
  plms<-rbind(plms,c(msum, rmse,maic,nlmsum,nlm.rmse,gamaic))
  
  
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.rem, y = T_min, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
do.call(grid.arrange,p)
dev.off()
colnames(plms)<-c("lm.r2","lm.p","lm.rmse","lm.aic","gam.r2","gam.p","gam.rmse","gam.aic")
tminjul.rem.cc<-as.data.frame(plms)


png(file="figs/tmin.fieldcc.2022aug.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in unique(augdat.minmax$date)){
  daydat<- augdat.minmax[augdat.minmax$date==i,]
  m<-lm(T_min~cc.field, data=daydat)
  msum<-round(c(summary(m)$adj.r.squared,summary(m)$coef[2,4]), digits=3)
  rmse<- round(sqrt(mean(m$residuals^2)), digits=3)
  nlm<-gam(T_min~s(cc.field), data=daydat)
  nlmsum<-round(c(summary(nlm)$r.sq,summary(nlm)$s.table[4]), digits=3)
  nlm.rmse<- round(sqrt(mean(nlm$residuals^2)), digits=3)
  maic<-round(AIC(m), digits=3)
  gamaic<-round(AIC(nlm), digits=3)
  plms<-rbind(plms,c(msum, rmse,maic,nlmsum,nlm.rmse,gamaic))
  
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.field, y = T_min, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
colnames(plms)<-c("lm.r2","lm.p","lm.rmse","lm.aic","gam.r2","gam.p","gam.rmse","gam.aic")
tminaug.field.cc<-as.data.frame(plms)

do.call(grid.arrange,p)
dev.off()


png(file="figs/tmax.fieldcc.2022aug.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in unique(augdat.minmax$date)){
  daydat<- augdat.minmax[augdat.minmax$date==i,]
  m<-lm(T_max~cc.field, data=daydat)
  msum<-round(c(summary(m)$adj.r.squared,summary(m)$coef[2,4]), digits=3)
  rmse<- round(sqrt(mean(m$residuals^2)), digits=3)
  nlm<-gam(T_max~s(cc.field), data=daydat)
  nlmsum<-round(c(summary(nlm)$r.sq,summary(nlm)$s.table[4]), digits=3)
  nlm.rmse<- round(sqrt(mean(nlm$residuals^2)), digits=3)
  maic<-round(AIC(m), digits=3)
  gamaic<-round(AIC(nlm), digits=3)
  plms<-rbind(plms,c(msum, rmse,maic,nlmsum,nlm.rmse,gamaic))
  
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.field, y = T_max, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
colnames(plms)<-c("lm.r2","lm.p","lm.rmse","lm.aic","gam.r2","gam.p","gam.rmse","gam.aic")
tmaxaug.field.cc<-as.data.frame(plms)

do.call(grid.arrange,p)
dev.off()


png(file="figs/tmin.remcc.2022aug.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in unique(augdat.minmax$date)){
  daydat<- augdat.minmax[augdat.minmax$date==i,]
  m<-lm(T_min~cc.rem, data=daydat)
  msum<-round(c(summary(m)$adj.r.squared,summary(m)$coef[2,4]), digits=3)
  rmse<- round(sqrt(mean(m$residuals^2)), digits=3)
  nlm<-gam(T_min~s(cc.rem), data=daydat)
  nlmsum<-round(c(summary(nlm)$r.sq,summary(nlm)$s.table[4]), digits=3)
  nlm.rmse<- round(sqrt(mean(nlm$residuals^2)), digits=3)
  maic<-round(AIC(m), digits=3)
  gamaic<-round(AIC(nlm), digits=3)
  plms<-rbind(plms,c(msum, rmse,maic,nlmsum,nlm.rmse,gamaic))
  
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.rem, y = T_min, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
colnames(plms)<-c("lm.r2","lm.p","lm.rmse","lm.aic","gam.r2","gam.p","gam.rmse","gam.aic")
tminaug.rem.cc<-as.data.frame(plms)

do.call(grid.arrange,p)
dev.off()


png(file="figs/tmax.remcc.2022aug.png",width =1800, height =1800 ,res =200)
p<-list()
plms<-c()
for(i in unique(augdat.minmax$date)){
  daydat<- augdat.minmax[augdat.minmax$date==i,]
  m<-lm(T_max~cc.rem, data=daydat)
  msum<-round(c(summary(m)$adj.r.squared,summary(m)$coef[2,4]), digits=3)
  rmse<- round(sqrt(mean(m$residuals^2)), digits=3)
  nlm<-gam(T_max~s(cc.rem), data=daydat)
  nlmsum<-round(c(summary(nlm)$r.sq,summary(nlm)$s.table[4]), digits=3)
  nlm.rmse<- round(sqrt(mean(nlm$residuals^2)), digits=3)
  maic<-round(AIC(m), digits=3)
  gamaic<-round(AIC(nlm), digits=3)
  plms<-rbind(plms,c(msum, rmse,maic,nlmsum,nlm.rmse,gamaic))
  
  p[[i]]<- ggplot(daydat,
                  aes(x= cc.rem, y = T_max, main=i)) +
    geom_point() +
    stat_smooth(method = "gam", 
                method.args = list(family = gaussian))
}
colnames(plms)<-c("lm.r2","lm.p","lm.rmse","lm.aic","gam.r2","gam.p","gam.rmse","gam.aic")
tmaxaug.rem.cc<-as.data.frame(plms)

do.call(grid.arrange,p)
dev.off()


#summarize across all days- primarily interested in field data
tmaxjul.field.cc$gamlower<-0
tmaxjul.field.cc$gamlower[tmaxjul.field.cc$gam.rmse<tmaxjul.field.cc$lm.rmse]<-1

tminjul.field.cc$gamlower<-0
tminjul.field.cc$gamlower[tminjul.field.cc$gam.rmse<tminjul.field.cc$lm.rmse]<-1

tmaxaug.field.cc$gamlower<-0
tmaxaug.field.cc$gamlower[tmaxaug.field.cc$gam.rmse<tmaxaug.field.cc$lm.rmse]<-1

tminaug.field.cc$gamlower<-0
tminaug.field.cc$gamlower[tminaug.field.cc$gam.rmse<tminaug.field.cc$lm.rmse]<-1

sum(tmaxjul.field.cc$gamlower)#7 days hadlower rmse
sum(tminjul.field.cc$gamlower)#6 days had better nonlinear fits
sum(juntmax.field.ccmodcomp$gamlower)#8 days had better nonlinear fits
sum(juntmin.field.ccmodcomp$gamlower)#9 days had better nonlinear fits
sum(tmaxaug.field.cc$gamlower)#14 days hadlower rmse

#compare ps
length(which(juntmin.field.ccmodcomp$lm.p<0.05))#7
length(which(juntmin.field.ccmodcomp$gam.p<0.05))#8
length(which(juntmax.field.ccmodcomp$lm.p<0.05))#21
length(which(juntmax.field.ccmodcomp$gam.p<0.05))#20

length(which(tminjul.field.cc$lm.p<0.05))#8
length(which(tminjul.field.cc$gam.p<0.05))#9
length(which(tmaxjul.field.cc$lm.p<0.05))#16
length(which(tmaxjul.field.cc$gam.p<0.05))#14

length(which(tminaug.field.cc$lm.p<0.05))#13
length(which(tminaug.field.cc$gam.p<0.05))#10
length(which(tmaxaug.field.cc$lm.p<0.05))#10
length(which(tmaxaug.field.cc$gam.p<0.05))#8


#Sum across all days: 
length(which(juntmin.field.ccmodcomp$lm.p<0.05))+length(which(tminjul.field.cc$lm.p<0.05))+length(which(tminaug.field.cc$lm.p<0.05))#28
length(which(juntmin.field.ccmodcomp$gam.p<0.05))+length(which(tminjul.field.cc$gam.p<0.05))+length(which(tminaug.field.cc$gam.p<0.05))#27
length(which(juntmax.field.ccmodcomp$lm.p<0.05))+length(which(tmaxjul.field.cc$lm.p<0.05))+length(which(tmaxaug.field.cc$lm.p<0.05))#47
length(which(juntmax.field.ccmodcomp$gam.p<0.05))+length(which(tmaxjul.field.cc$gam.p<0.05))+length(which(tmaxaug.field.cc$gam.p<0.05))#42

1-sum(sum(juntmax.field.ccmodcomp$gamlower)+sum(tmaxjul.field.cc$gamlower)+sum(tmaxaug.field.cc$gamlower))/sum(length(juntmax.field.ccmodcomp$gamlower)+length(tmaxjul.field.cc$gamlower)+length(tmaxaug.field.cc$gamlower))##29 days hadlower rmse
1-sum(sum(juntmin.field.ccmodcomp$gamlower)+sum(tminjul.field.cc$gamlower)+sum(tmaxaug.field.cc$gamlower))/92


##Look at daily patterns on hottest days in june and july
jun27hrdat<-jundat[jundat$date=="2022-06-27",]
jul31hrdat<-juldat[juldat$date=="2022-07-31",]


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
x<-boxplot(jul31hrdat$airtemp_c[jul31hrdat$Trees.=="N"]~jul31hrdat$hour[jul31hrdat$Trees.=="N"],
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

#Do model comparison between Tmax and Tmin mods and other vege
#merge vegmetrics in with sumdat.minmax
sumdat.minmaxv<-left_join(sumdat.minmax,locs3,by=c("Pole_No","Trees.","Elevation"))
#Tmin
sumTminr2.10m<-lmer(T_min~cc.rem*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmaxv)
sumTminr2.20m<-lmer(T_min~cc.rem20m*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmaxv)
sumTminr2.30m<-lmer(T_min~cc.rem30m*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmaxv)
sumTminr2.40m<-lmer(T_min~cc.rem40m*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmaxv)
sumTminr2.50m<-lmer(T_min~cc.rem50m*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmaxv)

sumdat.minmaxv$med10m<-sumdat.minmaxv$X2MedVeg.10mProp*100
sumdat.minmaxv$med20m<-sumdat.minmaxv$X2MedVeg.20mProp*100
sumdat.minmaxv$med30m<-sumdat.minmaxv$X2MedVeg.30m*100
sumdat.minmaxv$med40m<-sumdat.minmaxv$X2MedVeg.40m*100
sumdat.minmaxv$med50m<-sumdat.minmaxv$X2MedVeg.50m*100
sumdat.minmaxv$med10m.st<-(sumdat.minmaxv$med10m-mean(sumdat.minmaxv$med10m)/sd(sumdat.minmaxv$med10m))
sumdat.minmaxv$med20m.st<-(sumdat.minmaxv$med20m-mean(sumdat.minmaxv$med20m)/sd(sumdat.minmaxv$med20m))
sumdat.minmaxv$med30m.st<-(sumdat.minmaxv$med30m-mean(sumdat.minmaxv$med30m)/sd(sumdat.minmaxv$med30m))
sumdat.minmaxv$med40m.st<-(sumdat.minmaxv$med40m-mean(sumdat.minmaxv$med40m)/sd(sumdat.minmaxv$med40m))
sumdat.minmaxv$med50m.st<-(sumdat.minmaxv$med50m-mean(sumdat.minmaxv$med50m)/sd(sumdat.minmaxv$med50m))
sumdat.minmaxv$fin10m<-sumdat.minmaxv$X1FineVeg.10m*100
sumdat.minmaxv$fin20m<-sumdat.minmaxv$X1FineVeg.20m*100
sumdat.minmaxv$fin30m<-sumdat.minmaxv$X1FineVeg.30m*100
sumdat.minmaxv$fin40m<-sumdat.minmaxv$X1FineVeg.40m*100
sumdat.minmaxv$fin50m<-sumdat.minmaxv$X1FineVeg.50m*100
sumdat.minmaxv$fin10m.st<-(sumdat.minmaxv$fin10m-mean(sumdat.minmaxv$fin10m)/sd(sumdat.minmaxv$fin10m))
sumdat.minmaxv$fin20m.st<-(sumdat.minmaxv$fin20m-mean(sumdat.minmaxv$fin20m)/sd(sumdat.minmaxv$fin20m))
sumdat.minmaxv$fin30m.st<-(sumdat.minmaxv$fin30m-mean(sumdat.minmaxv$fin30m)/sd(sumdat.minmaxv$fin30m))
sumdat.minmaxv$fin40m.st<-(sumdat.minmaxv$fin40m-mean(sumdat.minmaxv$fin40m)/sd(sumdat.minmaxv$fin40m))
sumdat.minmaxv$fin50m.st<-(sumdat.minmaxv$fin50m-mean(sumdat.minmaxv$fin50m)/sd(sumdat.minmaxv$fin50m))

sumTminmed.10m<-lmer(T_min~med10m*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmaxv)
sumTminmed.20m<-lmer(T_min~med20m*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmaxv)
sumTminmed.30m<-lmer(T_min~med30m*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmaxv)
sumTminmed.40m<-lmer(T_min~med40m*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmaxv)
sumTminmed.50m<-lmer(T_min~med50m*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmaxv)
sumTminfin.10m<-lmer(T_min~fin10m*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmaxv)
sumTminfin.20m<-lmer(T_min~fin20m*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmaxv)
sumTminfin.30m<-lmer(T_min~fin30m*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmaxv)
sumTminfin.40m<-lmer(T_min~fin40m*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmaxv)
sumTminfin.50m<-lmer(T_min~fin50m*cloudcover+(1|date)+(1|Pole_No), data=sumdat.minmaxv)

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

modtab<-round(rbind(
  cbind(summary(sumTminr2.10m)$coef,Anova(sumTminr2.10m, type="III"),summary(sumTminr2.20m)$coef,Anova(sumTminr2.20m, type="III"),summary(sumTminr2.30m)$coef,Anova(sumTminr2.30m, type="III"),summary(sumTminr2.40m)$coef,Anova(sumTminr2.40m,type="III"),summary(sumTminr2.50m)$coef,Anova(sumTminr2.50m, type="III")),
  cbind(summary(sumTminmed.10m)$coef,Anova(sumTminmed.10m, type="III"),summary(sumTminmed.20m)$coef,Anova(sumTminmed.20m, type="III"),summary(sumTminmed.30m)$coef,Anova(sumTminmed.30m, type="III"),summary(sumTminmed.40m)$coef,Anova(sumTminmed.40m, type="III"),summary(sumTminmed.50m)$coef,Anova(sumTminmed.50m, type="III")),
  cbind(summary(sumTminfin.10m)$coef,Anova(sumTminfin.10m, type="III"),summary(sumTminfin.20m)$coef,Anova(sumTminfin.20m, type="III"),summary(sumTminfin.30m)$coef,Anova(sumTminfin.30m, type="III"),summary(sumTminfin.40m)$coef,Anova(sumTminfin.40m, type="III"),summary(sumTminfin.50m)$coef,Anova(sumTminfin.50m, type="III"))), digits=3)

write.csv(modtab,"output/vegeffectsmodsummaries.csv")




jun27datll<-left_join(jun27dat, locs3, by=c("Hobo_SN","Trees.","cc.field", "cc.rem","imp", "Elevation"), suffixes=c("moist", "temp"))

#which logger had the hottest temp on the hottest day in July?
jul31dat[which(jul31dat$T_max==(max(jul31dat$T_max))),]
jul31dat[which(jul31dat$T_min==(max(jul31dat$T_min))),]

templatlonjul31<-left_join(jul31dat, locs3, by=c("Hobo_SN","Trees.","Elevation"))
templatlonjun27<-left_join(jun27dat, locs3, by=c("Hobo_SN","Trees.","Elevation"))

templatlonjul31
ggplot(templatlonjul31, aes(x = Longitude, y = Latitude, color=T_min)) +
  geom_point(aes(size = T_min), alpha = 0.5) +
  theme_classic((base_size = 18)) +
  scale_colour_gradient(low = "yellow",high="red")

ggplot(templatlonjun27, aes(x = Longitude, y = Latitude, color=T_min)) +
  geom_point(aes(size = T_min), alpha = 0.8) +
  theme_classic((base_size = 18)) +
  scale_colour_gradient(low = "yellow",high="red")

  
meanTmax<-aggregate(sumdat.minmax$T_max, by=list(sumdat.minmax$Hobo_SN), mean)
colnames(meanTmax)<-c("Hobo_SN","meanTmax")
meanTmax<-left_join(meanTmax, locs3, by=c("Hobo_SN"))
ggplot(meanTmax, aes(x = Longitude, y = Latitude, color=meanTmax)) +
  geom_point(aes(size = meanTmax), alpha = 0.8) +
  theme_classic((base_size = 18)) +
  scale_colour_gradient(low = "yellow",high="red")

meanTmin<-aggregate(sumdat.minmax$T_min, by=list(sumdat.minmax$Hobo_SN), mean)
colnames(meanTmin)<-c("Hobo_SN","meanTmin")
meanTmin<-left_join(meanTmin, locs3, by=c("Hobo_SN"))

ggplot(meanTmin, aes(x = Longitude, y = Latitude, color=meanTmin)) +
  geom_point(aes(size = meanTmin), alpha = 0.8) +
  theme_classic((base_size = 18)) +
  scale_colour_gradient(low = "yellow",high="red")
write.csv(meanTmin,"output/meanTmin.csv")

#To compare our work to Ziter2018 
source("compare_GRIT_Ziter2018.R")



