#file to clean the tempblitz form data 
#rename serial number column name to match 
colnames(surflogs)[which(colnames(surflogs)=="Utility.Pole.Temperature.Logger..")]<- "Hobo_SN"

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

#fix somet inconsistencies to match temp logger time format
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

#clean some of the serial numbers
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
surflogs$Hobo_SN[surflogs$Hobo_SN=="21223119"]<-"21123119"
surflogs$Hobo_SN[surflogs$Hobo_SN=="2122329"]<-"21223129"
surflogs$Hobo_SN[surflogs$Hobo_SN=="21223119"]<-"21123119"
surflogs$Hobo_SN[surflogs$Hobo_SN=="2122316"]<-"21223116"
surflogs$Hobo_SN[surflogs$Hobo_SN=="21302953"]<- "BT21302953"
surflogs$Hobo_SN[surflogs$Hobo_SN=="2122324"]<-"21223124"
surflogs$Hobo_SN[surflogs$Hobo_SN=="2122301"]<-"BT21302946"
surflogs$Hobo_SN[surflogs$Hobo_SN=="21302963"]<-"BT21302953"

#remove row with no data
surflogs<-surflogs[!surflogs$Hobo_SN=="",]

write.csv(surflogs,"output/TempBlitzFormData_cleaned.csv", row.names = FALSE)

