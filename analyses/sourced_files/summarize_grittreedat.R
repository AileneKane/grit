##############################################################
### Script to summarize tree abundance data around loggers ###
################# August 24, 2022 ############################
################ ailene.ettinger@tnc.org #####################
##############################################################
treedat<-treedat[,1:7]

#Do some cleaning:
treedat$Species[treedat$Species=="Psemen"]<-"Pseudotsuga"
treedat$Species[treedat$Species=="Unknwn"]<-"Unknown"
treedat$Species[treedat$Species=="Quegar" ]<-"Quercus"
treedat$Species[treedat$Species=="Rhacas?" ]<-"Rhamnus"
treedat$Species[treedat$Species=="Lquidumbar"]<-"Liquidumbar"
treedat$Species[treedat$Species==""]<-NA
treedat$Species[treedat$Species=="?"]<-"Unknown"
treedat$Species[treedat$Species== "Quercus garryana"]<-"Quercus"

#calculate basal area
treedat$DBH_cm<-as.numeric(treedat$DBH_cm)
treedat$BA<-pi*treedat$DBH_cm*treedat$DBH_cm

#now get sum of basal area for each HOBO_SN
sumba<-aggregate(treedat$BA, by=list(treedat$Hobo_SN,treedat$WptNo,treedat$Pole_No), sum, na.rm=TRUE)
sumtreeno<-aggregate(treedat$BA, by=list(treedat$Hobo_SN,treedat$WptNo,treedat$Pole_No), length)
sumba<-cbind(sumba,sumtreeno$x)
colnames(sumba)<-c("Hobo_SN","WptNo","Pole_No","TotalBA_cm2","NumTrees")
