library(xlsx)
library(ggplot2)
#READ IN FILES
filepath<-"C:/Users/LY/Desktop/star on devices/vgs-id-05.xls"   #Your filepath
mydata<-read.xlsx(filepath,sheetName="Data",header=TRUE)
curve<-mydata[1:2]

qplot(GateV,DrainI,data=curve)
U<-curve[1]
I<-curve[2]   
newcurve<-curve
newcurve[1]<-U
newcurve[2]<-I
ggplot(newcurve,aes(x=GateV,y=DrainI))+geom_point()
# divide the curve into 2 seperate part
curve1<-newcurve
curve2<-newcurve
row<-nrow(U);
for(k in 1:row){
  curve1[k,2]=min(I[which(U==U[k,1]),1])
  curve1[k,1]=U[k,1]
  curve2[k,2]=max(I[which(U==U[k,1]),1])
  curve2[k,1]=U[k,1]}
ggplot(curve1,aes(x=GateV,y=DrainI))+geom_point()
ggplot(curve2,aes(x=GateV,y=DrainI))+geom_point()
