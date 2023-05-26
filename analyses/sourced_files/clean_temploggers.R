#file to "HoboLocations_TempLoggerInfo" file

#clean Hobo_SN column
#logs$Hobo_SN[grep("Bt",logs$Hobo_SN)]<-gsub("Bt","BT",logs$Hobo_SN[grep("Bt",logs$Hobo_SN)])
#logs$Hobo_SN[grep("BT ",logs$Hobo_SN)]<-gsub("BT ","BT",logs$Hobo_SN[grep("BT ",logs$Hobo_SN)])
logs$Hobo_SN[logs$Hobo_SN=="B121302952"]<-"BT21302952"
#logs$gHobo_SN[logs$Hobo_SN=="BT1302979"]<-"BT21302979"

