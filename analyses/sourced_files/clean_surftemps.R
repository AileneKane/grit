#file to clean the tempblitz surface temperature data 

#rename the columns
colnames(surftemps)[2:8]<- substr(colnames(surftemps)[2:8],nchar(colnames(surftemps)[2:8])-7,nchar(colnames(surftemps)[2:8]))

#add columns to separate out year, month, date (time?)
surftemps$time<-substr(surftemps$Date,nchar(surftemps$Date)-4,nchar(surftemps$Date))
