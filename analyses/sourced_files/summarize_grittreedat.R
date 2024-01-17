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
treedat$Species[treedat$Species=="Acer macrophyllum"]<-"Acer"
treedat$Species[treedat$Species=="Chamaecyparis lawsoniana"]<-"Chamaecyparis"
treedat$Species[treedat$Species=="Lilac"]<-"Syringa"

#calculate basal area
treedat$DBH_cm<-as.numeric(treedat$DBH_cm)
treedat$BA<-pi*treedat$DBH_cm*treedat$DBH_cm

#now get sum of basal area for each HOBO_SN
sumba<-aggregate(treedat$BA, by=list(treedat$Hobo_SN,treedat$WptNo,treedat$Pole_No), sum, na.rm=TRUE)
sumtreeno<-aggregate(treedat$BA, by=list(treedat$Hobo_SN,treedat$WptNo,treedat$Pole_No), length)
sumba<-cbind(sumba,sumtreeno$x)
colnames(sumba)<-c("Hobo_SN","WptNo","Pole_No","TotalBA_cm2","NumTrees")

#calculate species richness for each logger locatoin

sptab<-table(treedat$Hobo_SN,treedat$Species)
sptab[sptab!=0]<-1
rich<-rowSums(sptab != 0)
rich.df<-as.data.frame(cbind(names(rich),rich))
colnames(rich.df)[1]<-"Hobo_SN"
sumba2<-left_join(sumba,rich.df)

sumba<-sumba2

#do som cleaning- some plots with 0 trees say there is 1- correct this
sumba$NumTrees[sumba$TotalBA_cm2==0 & sumba$NumTrees>0]<-0


#nowmake a table of species and how many plots they are in, how much basal area there is
sumbabysp<-aggregate(treedat$BA, by=list(treedat$Species), sum, na.rm=TRUE)
colnames(sumbabysp)<-c("Species","TBA_cm2")

sptab2<-table(treedat$Species,treedat$Pole_No)
sptab2[sptab2!=0]<-1
freq<-rowSums(sptab2 != 0)
freq.df<-as.data.frame(cbind(names(freq),freq))
colnames(freq.df)[1]<-"Species"
freqba<-left_join(sumbabysp,freq.df)
freqba$TBA_cm2<-round(freqba$TBA_cm2, digits=0)
freqba<-freqba[order(freqba$TBA_cm2, decreasing=TRUE),]
write.csv(freqba,"output/sptab.csv", row.names = FALSE)
