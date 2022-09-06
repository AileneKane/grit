#file to clean the tempblitz form data 
#rename serial number column name to match 
colnames(surflogs)[which(colnames(surflogs)=="Utility.Pole.Temperature.Logger..")]<- "Hobo_SN"
colnames(surflogs)[which(colnames(surflogs)=="Your.temperature.logger.is.located.in.")]<-"sunshade"
colnames(surflogs)[which(colnames(surflogs)=="Your.Temperature.Logger.Location")]<-"surftype"

#add columns to separate out year, month, date (time?)
surflogs$year<-substr(surflogs$Timestamp,1,4)
surflogs$month<-substr(surflogs$Timestamp,6,7)
surflogs$day<-substr(surflogs$Timestamp,9,10)
surflogs$date<-substr(surflogs$Timestamp,1,10)

#select out only temp blitz date
surflogs<-surflogs[surflogs$date=="2022/08/11",]

#clean the end.time column:
#unique(surflogs$End.time..hh.mm.AM.PM.)
#use time stamp to add end time to rows missing this
surflogs$End.time..hh.mm.AM.PM.[surflogs$End.time..hh.mm.AM.PM.==""]<-paste(as.numeric(substr(surflogs$Timestamp[surflogs$End.time..hh.mm.AM.PM.==""],12,13))-1,substr(surflogs$Timestamp[surflogs$End.time..hh.mm.AM.PM.==""],14,16), sep="")

#fix some inconsistencies to match temp logger time format
surflogs$End.time..hh.mm.AM.PM.[surflogs$End.time..hh.mm.AM.PM.=="857"]<-"8:57"
surflogs$End.time..hh.mm.AM.PM.[surflogs$End.time..hh.mm.AM.PM.=="905"]<-"9:05"
surflogs$End.time..hh.mm.AM.PM.[surflogs$End.time..hh.mm.AM.PM.=="911"]<-"9:11"
surflogs$End.time..hh.mm.AM.PM.[surflogs$End.time..hh.mm.AM.PM.=="918"]<-"9:18"
surflogs$End.time..hh.mm.AM.PM.[surflogs$End.time..hh.mm.AM.PM.=="925"]<-"9:25"
surflogs$End.time..hh.mm.AM.PM.[surflogs$End.time..hh.mm.AM.PM.=="932"]<-"9:32"
surflogs$End.time..hh.mm.AM.PM.[surflogs$End.time..hh.mm.AM.PM.=="12pm"]<-"12:00"
surflogs$End.time..hh.mm.AM.PM.[grep("pm",surflogs$End.time..hh.mm.AM.PM.,)]<-substr(surflogs$End.time..hh.mm.AM.PM.[grep("pm",surflogs$End.time..hh.mm.AM.PM.,)],1,nchar(surflogs$End.time..hh.mm.AM.PM.[grep("pm",surflogs$End.time..hh.mm.AM.PM.,)])-2)
surflogs$End.time..hh.mm.AM.PM.[grep("PM",surflogs$End.time..hh.mm.AM.PM.,)]<-substr(surflogs$End.time..hh.mm.AM.PM.[grep("PM",surflogs$End.time..hh.mm.AM.PM.,)],1,nchar(surflogs$End.time..hh.mm.AM.PM.[grep("PM",surflogs$End.time..hh.mm.AM.PM.,)])-2)
surflogs$End.time..hh.mm.AM.PM.[grep(" PM",surflogs$End.time..hh.mm.AM.PM.,)]<-substr(surflogs$End.time..hh.mm.AM.PM.[grep(" PM",surflogs$End.time..hh.mm.AM.PM.,)],1,nchar(surflogs$End.time..hh.mm.AM.PM.[grep(" PM",surflogs$End.time..hh.mm.AM.PM.,)])-3)
surflogs$End.time..hh.mm.AM.PM.[grep(" AM",surflogs$End.time..hh.mm.AM.PM.,)]<-substr(surflogs$End.time..hh.mm.AM.PM.[grep(" AM",surflogs$End.time..hh.mm.AM.PM.,)],1,nchar(surflogs$End.time..hh.mm.AM.PM.[grep(" AM",surflogs$End.time..hh.mm.AM.PM.,)])-3)
surflogs$End.time..hh.mm.AM.PM.[grep("am",surflogs$End.time..hh.mm.AM.PM.,)]<-substr(surflogs$End.time..hh.mm.AM.PM.[grep("am",surflogs$End.time..hh.mm.AM.PM.,)],1,nchar(surflogs$End.time..hh.mm.AM.PM.[grep("am",surflogs$End.time..hh.mm.AM.PM.,)])-2)

#convert to 24 hour time to match temp logger format
surflogs$End.time..hh.mm.AM.PM.[grep("3:",surflogs$End.time..hh.mm.AM.PM.,)]<-gsub("3:","15:",surflogs$End.time..hh.mm.AM.PM.[grep("3:",surflogs$End.time..hh.mm.AM.PM.,)])
surflogs$End.time..hh.mm.AM.PM.[substr(surflogs$End.time..hh.mm.AM.PM.,1,2)=="2:"]<-gsub("2:","14:",surflogs$End.time..hh.mm.AM.PM.[substr(surflogs$End.time..hh.mm.AM.PM.,1,2)=="2:"])
surflogs$End.time..hh.mm.AM.PM.[substr(surflogs$End.time..hh.mm.AM.PM.,1,2)=="1:"]<-gsub("1:","13:",surflogs$End.time..hh.mm.AM.PM.[substr(surflogs$End.time..hh.mm.AM.PM.,1,2)=="1:"])

#remove spaces
surflogs$End.time..hh.mm.AM.PM.[grep(" ",surflogs$End.time..hh.mm.AM.PM.,)]<-gsub(" ","",surflogs$End.time..hh.mm.AM.PM.[grep(" ",surflogs$End.time..hh.mm.AM.PM.,)])

#clean some of the serial numbers of air temp loggers
#sort(unique(surflogs$Hobo_SN))
surflogs$Hobo_SN[surflogs$Hobo_SN=="2116267 bt21302947"]<-"BT21302947"
surflogs$Hobo_SN[surflogs$Hobo_SN=="2401795 BT21302957"]<-"BT21302957"
surflogs$Hobo_SN[surflogs$Hobo_SN=="21162467"]<-"BT21302947"
surflogs$Hobo_SN[grep("Bt",surflogs$Hobo_SN)]<-gsub("Bt","BT",surflogs$Hobo_SN[grep("Bt",surflogs$Hobo_SN)])
surflogs$Hobo_SN[surflogs$Hobo_SN=="B721302950"]<-"BT21302950"
surflogs$Hobo_SN[surflogs$Hobo_SN=="BT21302967"]<-"BT21302967"
surflogs$Hobo_SN[surflogs$Hobo_SN=="211223121"]<-"2122312"
surflogs$Hobo_SN[surflogs$Hobo_SN=="21302967"]<-"BT21302967"
surflogs$Hobo_SN[surflogs$Hobo_SN=="2122312"]<-"21223121"
surflogs$Hobo_SN[surflogs$Hobo_SN=="BT21302942"]<-"BT21302947"
surflogs$Hobo_SN[surflogs$Hobo_SN=="B121302953"]<-"BT21302953"
surflogs$Hobo_SN[surflogs$Hobo_SN=="BT21302971"]<-"BT2130297"
surflogs$Hobo_SN[surflogs$Hobo_SN=="2122329"]<-"21223129"
surflogs$Hobo_SN[surflogs$Hobo_SN=="2122316"]<-"21223116"
surflogs$Hobo_SN[surflogs$Hobo_SN=="21302953"]<- "BT21302953"
surflogs$Hobo_SN[surflogs$Hobo_SN=="2122324"]<-"21223124"
surflogs$Hobo_SN[surflogs$Hobo_SN=="2122301"]<-"BT21302946"
surflogs$Hobo_SN[surflogs$Hobo_SN=="21302963"]<-"BT21302953"
surflogs$Hobo_SN[surflogs$Hobo_SN=="2401795 BT21302957"]<-"BT21302957"
surflogs$Hobo_SN[surflogs$Hobo_SN=="2116267 bt21302947"]<-"BT21302947"
surflogs$Hobo_SN[surflogs$Hobo_SN=="BT21302974 / 21223140"]<-"BT21302974"
surflogs$Hobo_SN[surflogs$Hobo_SN=="BT 21302967, 21223116"]<-"BT21302967"
surflogs$Hobo_SN[surflogs$Hobo_SN=="BT 21302977, 21223102"]<-"BT21302977"
surflogs$Hobo_SN[surflogs$Hobo_SN=="BT21302976 / 21223131"]<-"BT21302976"
surflogs$Hobo_SN[surflogs$Hobo_SN=="BT 21302946, 21223101"]<-"BT21302946"
surflogs$Hobo_SN[surflogs$Hobo_SN=="BT21302957 / 2401795"]<-"BT21302957"
surflogs$Hobo_SN[surflogs$Hobo_SN=="BT 21302953"]<-"BT21302953"
surflogs$Hobo_SN[surflogs$Hobo_SN=="Bt21302947"]<-"BT21302947"
surflogs$Hobo_SN[surflogs$Hobo_SN=="B721302950"]<-"BT21302950"
surflogs$Hobo_SN[surflogs$Hobo_SN=="By21302960"]<-"BT21302960"
surflogs$Hobo_SN[surflogs$Hobo_SN=="Bt21302962"]<-"BT21302962"
surflogs$Hobo_SN[surflogs$Hobo_SN=="21302967"]<-"BT21302967"
surflogs$Hobo_SN[surflogs$Hobo_SN=="Bt21302957"]<-"BT21302957"
surflogs$Hobo_SN[surflogs$Hobo_SN=="Bt21302944"]<-"BT21302944"
surflogs$Hobo_SN[surflogs$Hobo_SN=="212236136"]<-"21223136"
surflogs$Hobo_SN[surflogs$Hobo_SN=="BT21202973"]<-"BT21302974"
surflogs$Hobo_SN[surflogs$Hobo_SN=="21223140"]<-"BT21302974"
surflogs$Hobo_SN[surflogs$Hobo_SN=="BT2130297"]<-"BT21302971"

#clean serial numbers for surface loggers (the ones people carried with them)
surflogs$Your.Temperature.Logger..[surflogs$Your.Temperature.Logger..=="MX2201 21302978"]<-"21302978"
surflogs$Your.Temperature.Logger..[surflogs$Your.Temperature.Logger..=="Mx2201 21302954"]<-"21302954"
surflogs$Your.Temperature.Logger..[surflogs$Your.Temperature.Logger..=="MX2201 21302954"]<-"21302954"
surflogs$Your.Temperature.Logger..[surflogs$Your.Temperature.Logger..=="2130965"]<-"21302965"
surflogs$Your.Temperature.Logger..[surflogs$Your.Temperature.Logger..=="21402965"]<-"21302965"

#combine some categories in surftype
surflogs$surftype[surflogs$surftype=="Weeds!"]<-"veg"
surflogs$surftype[surflogs$surftype=="Grass"]<-"veg"
surflogs$surftype[surflogs$surftype=="Pebbles and some grass"]<-"veg"
surflogs$surftype[surflogs$surftype=="Sidewalk (Paved Surface)"]<-"pavement"
surflogs$surftype[surflogs$surftype=="Rocks and dirt"]<-"soil"
surflogs$surftype[surflogs$surftype=="Rocks and pebbles "]<-"soil"
surflogs$surftype[surflogs$surftype=="Dirt and dead grass"]<-"soil"
surflogs$surftype[surflogs$surftype=="Mulched soil"]<-"soil"
surflogs$surftype[surflogs$surftype=="Bare soil"]<-"soil"

#remove row with no data
surflogs<-surflogs[!surflogs$Hobo_SN=="",]

write.csv(surflogs,"output/TempBlitzFormData_cleaned.csv", row.names = FALSE)

