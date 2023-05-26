#file to clean the hobo locs datafile 
#remove abandoned logger:
locs<-locs[!locs$Pole_No=="TP12963",]

#below is no longer necessary
#correct poles that appear to have incorrect lat/long
#locs$Latitude[locs$Pole_No=="TP9879 - Tacoma Power Owned"]<-47.209794
#locs$Longitude[locs$Pole_No=="TP9879 - Tacoma Power Owned"]<--122.483606
#locs$Longitude[locs$Pole_No=="TP21898?/A1353781"]<--122.445105
#locs$Latitude[locs$Pole_No=="TP21898?/A1353781"]<-47.225066
#locs$Longitude[locs$Pole_No=="TP6325/A1341104"]<--122.4741819
#locs$Latitude[locs$Pole_No=="TP6325/A1341104"]<-47.2189137
#locs$Longitude[locs$Pole_No=="TP26058"]<--122.4740961
#locs$Latitude[locs$Pole_No=="TP26058"]<-47.2206607
