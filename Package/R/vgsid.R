####
#This will be a automatic processing program
library(MASS)
library(xlsx)
library(ggplot2)
#filepath<-choose.files()
#filepath<-"C:/Users/LY/Documents/Tencent Files/787094809/FileRecv/vgs-id-0.5.xls"
filepath<-"E:/A/vgs-id_A1.xls"   #Your filepath
#filepath<-"E:/2015.03.18/vgs-id#PVA5+DNTT@air-light-darktolight.xls"   #Your filepath
widthLengthRatio<-32.5  #W/L=33
Ci<-10^-8
mydata<-read.xlsx(filepath,sheetName="Data",header=TRUE)
curve<-mydata[1:2]
qplot(GateV,DrainI,data=curve)
U<-curve[1]
I<-curve[2]   # Lack of a function to judge whether I is negative
radicalI<-sqrt(abs(I))
newcurve<-curve
newcurve[1]<-U
newcurve[2]<-radicalI
ggplot(newcurve,aes(x=GateV,y=DrainI))+geom_point()
# divide the curve into 2 seperate part
curve1<-newcurve
curve2<-newcurve
row<-nrow(U);
for(k in 1:row){
  curve1[k,2]=min(radicalI[which(U==U[k,1]),1])
  curve1[k,1]=U[k,1]
  curve2[k,2]=max(radicalI[which(U==U[k,1]),1])
  curve2[k,1]=U[k,1]}
ggplot(curve1,aes(x=GateV,y=DrainI))+geom_point()


####ÄâºÏ
parameter<-lqs(DrainI~GateV,curve1)
plot(curve1)
abline(reg=parameter,lty=1)
var2<-coef(parameter)[2]
var1<-coef(parameter)[1]
mobility1<-var2^2/(widthLengthRatio*Ci/2)
#var1/sqrt(widthLengthRatio*Ci/2)
thresVoltage1<-var1/var2
thresVoltage1
mobility1
#The end