#compare our temp to the temps in Ziter
z1<-read.csv("../data/Ziter2018data/Ziter_UrbanTemp_Dataset_S1.csv", header=TRUE)
z2<-read.csv("../data/Ziter2018data/Ziter_UrbanTemp_Dataset_S2.csv", header=TRUE)
z3<-read.csv("../data/Ziter2018data/Ziter_UrbanTemp_Dataset_S3.csv", header=TRUE)

range(z2$TEMP_C); mean(z2$TEMP_C)
range(z3$TEMP_C); mean(z3$TEMP_C)
mean(c(z2$TEMP_C,z3$TEMP_C))
range(sumdat$airtemp_c)
mean(sumdat$airtemp_c)
