##############################################################
### Script to combine GRIT tree abundance data w/landcover ###
################# January 2022 ############################
################ ailene.ettinger@tnc.org #####################
##############################################################

#combine locs with basal area data(sumba
locs2<-left_join(locs,sumba, by=c("WptNo","Pole_No"))
locs2$TotalBA_cm2[which(is.na(locs2$TotalBA_cm2))]<-0
locs2$NumTrees[which(is.na(locs2$NumTrees))]<-0
locs2$NumTrees[locs2$TotalBA_cm2==0]<-0

#colnames(locs2)[2]<-"Hobo_SN"
#check for mismatches- i.e. sites with no trees, but have BA:
#locs2[locs2$Trees.=="N" & locs2$TotalBA_cm2>0,]#Wypt 23, fixed
#locs2$WptNo[locs2$Trees.=="Y" & locs2$TotalBA_cm2==0]# 25 28 55 59

locs2$NumTrees[locs2$WptNo=="28"]<-0

#add land cover data
locs3<-left_join(locs2,lc, by=c("Pole_No", "Latitude","Longitude"), copy=TRUE)
locs3$X3CoarseVeg.10mProp<-locs3$X3CoarseVeg.10m/314.016
locs3$X3CoarseVeg.20mProp<-locs3$X3CoarseVeg.20m/1256.06
locs3$X3CoarseVeg.30mProp<-locs3$X3CoarseVeg.30m/2826.14
locs3$X3CoarseVeg.40mProp<-locs3$X3CoarseVeg.40m/5024.25
locs3$X3CoarseVeg.50mProp<-locs3$X3CoarseVeg.50m/7850.39
locs3$X3CoarseVeg.100mProp<-locs3$X3CoarseVeg.100m/31415.9
locs3$X3CoarseVeg.200mProp<-locs3$X3CoarseVeg.200m/125664
locs3$X3CoarseVeg.400mProp<-locs3$X3CoarseVeg.400m/502655
locs3$X3CoarseVeg.800mProp<-locs3$X3CoarseVeg.800m/2010619


locs3$XAllImp.10mProp<-(locs3$X6ImpOther.10m+ locs3$X7ImpRoofs.10m)/314.016
locs3$X1FineVeg.10mProp<-locs3$X1FineVeg.10m/314.016
locs3$X1FineVeg.20mProp<-locs3$X1FineVeg.20m/1256.06
locs3$X1FineVeg.30mProp<-locs3$X1FineVeg.30m/2826.14
locs3$X1FineVeg.40mProp<-locs3$X1FineVeg.40m/5024.25
locs3$X1FineVeg.50mProp<-locs3$X1FineVeg.50m/7850.39
locs3$X2MedVeg.10mProp<-locs3$X2MedVeg.10m/314.016
locs3$X2MedVeg.20mProp<-locs3$X2MedVeg.20m/1256.06
locs3$X2MedVeg.30mProp<-locs3$X2MedVeg.30m/2826.14
locs3$X2MedVeg.40mProp<-locs3$X2MedVeg.40m/5024.25
locs3$X2MedVeg.50mProp<-locs3$X2MedVeg.50m/7850.39

#add column for whether logger has a sheild or not
locs3$shield<-"YES"
locs3$shield[locs3$Location=="Wapato Hills 2"]<-"NO"
locs3$shield[locs3$Location=="Wapato Hills 1"]<-"NO"
locs3$shield[locs3$Location=="South Tacoma Wetland"]<-"NO"
locs3$shield[locs3$Location=="South Tacoma Wetland 1"]<-"NO"
#locs3<-locs3[1:81,]
