#file to clean the hobo locs datafile 
#clean the Pole_No column
locs$Pole_No[grep(" - Jointly Owned",locs$Pole_No)]<-gsub(" - Jointly Owned","",locs$Pole_No[grep(" - Jointly Owned",locs$Pole_No)])
locs$Pole_No[grep(" - Tacoma Power Owned",locs$Pole_No)]<-gsub(" - Tacoma Power Owned","",locs$Pole_No[grep(" - Tacoma Power Owned",locs$Pole_No)])
locs$Pole_No[grep("- Tacoma Power",locs$Pole_No)]<-gsub("- Tacoma Power","",locs$Pole_No[grep("- Tacoma Power",locs$Pole_No)])
locs$Pole_No[grep("Tp",locs$Pole_No)]<-gsub("Tp","TP",locs$Pole_No[grep("Tp",locs$Pole_No)])

#clean Hobo_SN column
locs$Hobo_SN[grep("Bt",locs$Hobo_SN)]<-gsub("Bt","BT",locs$Hobo_SN[grep("Bt",locs$Hobo_SN)])
locs$Hobo_SN[grep("BT ",locs$Hobo_SN)]<-gsub("BT ","BT",locs$Hobo_SN[grep("BT ",locs$Hobo_SN)])
locs$Hobo_SN[locs$Hobo_SN=="B121302952"]<-"BT21302952"
locs$Hobo_SN[locs$Hobo_SN=="BT31302979"]<-"BT21302979"


locs$Hobo_SN[locs$Pole_No=="TP25887"]<-"NEEDTOFIX"

#remove abandoned logger:
locs<-locs[!locs$Pole_No=="TP12963",]

#correct pole that appears to have incorrect lat/long
locs$Latitude[locs$Pole_No=="TP9879"]<-47.209794
locs$Longitude[locs$Pole_No=="TP9879"]<--122.483606
