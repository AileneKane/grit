##############################################################
### Script to combine GRIT tree abundance data w/landcover ###
################# January 2022 ############################
################ ailene.ettinger@tnc.org #####################
##############################################################

#combine locs with basal area data(sumba
locs2<-left_join(locs,sumba, by=c("Hobo_SN","WptNo","Pole_No"))
locs2$TotalBA_cm2[which(is.na(locs2$TotalBA_cm2))]<-0
locs2$NumTrees[which(is.na(locs2$NumTrees))]<-0
#add land cover data
locs3<-left_join(locs2,lc, by=c("Pole_No", "Latitude","Longitude"), copy=TRUE)
locs3$X3CoarseVeg.10mProp<-locs3$X3CoarseVeg.10m/314.016
locs3$X3CoarseVeg.20mProp<-locs3$X3CoarseVeg.20m/1256.06
locs3$X3CoarseVeg.30mProp<-locs3$X3CoarseVeg.30m/2826.14
locs3$X3CoarseVeg.40mProp<-locs3$X3CoarseVeg.40m/5024.25
locs3$X3CoarseVeg.50mProp<-locs3$X3CoarseVeg.50m/7850.39

locs3$XAllImp.10mProp<-(locs3$X6ImpOther.10m+ locs3$X7ImpRoofs.10m)/314.016
locs3$X1FineVeg.10mProp<-locs3$X1FineVeg.10m/314.016
locs3$X2MedVeg.10mProp<-locs3$X2MedVeg.10m/314.016
#add column for whether logger has a sheild or not
locs3$shield<-"YES"
locs3$shield[locs3$Location=="Wapato Hills 2"]<-"NO"
locs3$shield[locs3$Location=="Wapato Hills 1"]<-"NO"
locs3$shield[locs3$Location=="South Tacoma Wetland"]<-"NO"
locs3$shield[locs3$Location=="South Tacoma Wetland 1"]<-"NO"
