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
unique(surflogs$End.time..hh.mm.AM.PM.)
surflogs$End.time..hh.mm.AM.PM.[surflogs$End.time..hh.mm.AM.PM.=="857"]<-"8:57"
surflogs$End.time..hh.mm.AM.PM.[surflogs$End.time..hh.mm.AM.PM.=="905"]<-"9:05"
surflogs$End.time..hh.mm.AM.PM.[surflogs$End.time..hh.mm.AM.PM.=="911"]<-"9:11"
surflogs$End.time..hh.mm.AM.PM.[surflogs$End.time..hh.mm.AM.PM.=="918"]<-"9:18"
surflogs$End.time..hh.mm.AM.PM.[surflogs$End.time..hh.mm.AM.PM.=="925"]<-"9:25"
surflogs$End.time..hh.mm.AM.PM.[surflogs$End.time..hh.mm.AM.PM.=="932"]<-"9:32"
surflogs$End.time..hh.mm.AM.PM.[surflogs$End.time..hh.mm.AM.PM.=="12pm"]<-"12:00"
surflogs$End.time..hh.mm.AM.PM.[grep("pm",surflogs$End.time..hh.mm.AM.PM.,)]<-substr(surflogs$End.time..hh.mm.AM.PM.[grep("pm",surflogs$End.time..hh.mm.AM.PM.,)],1,nchar(surflogs$End.time..hh.mm.AM.PM.[grep("pm",surflogs$End.time..hh.mm.AM.PM.,)])-2)
surflogs$End.time..hh.mm.AM.PM.[grep("PM",surflogs$End.time..hh.mm.AM.PM.,)]<-substr(surflogs$End.time..hh.mm.AM.PM.[grep("PM",surflogs$End.time..hh.mm.AM.PM.,)],1,nchar(surflogs$End.time..hh.mm.AM.PM.[grep("PM",surflogs$End.time..hh.mm.AM.PM.,)])-2)
surflogs$End.time..hh.mm.AM.PM.[grep("AM",surflogs$End.time..hh.mm.AM.PM.,)]<-substr(surflogs$End.time..hh.mm.AM.PM.[grep("AM",surflogs$End.time..hh.mm.AM.PM.,)],1,nchar(surflogs$End.time..hh.mm.AM.PM.[grep("AM",surflogs$End.time..hh.mm.AM.PM.,)])-2)
surflogs$End.time..hh.mm.AM.PM.[grep("am",surflogs$End.time..hh.mm.AM.PM.,)]<-substr(surflogs$End.time..hh.mm.AM.PM.[grep("am",surflogs$End.time..hh.mm.AM.PM.,)],1,nchar(surflogs$End.time..hh.mm.AM.PM.[grep("am",surflogs$End.time..hh.mm.AM.PM.,)])-2)
