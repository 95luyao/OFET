library(xlsx)
library(ggplot2)

filepath<-"C:/Users/LY/Desktop/data/2015.03.19/vgs-id#1@0.xls"   #Your filepath
widthLengthRatio<-32.5  #W/L=33
Ci<-10^-9
mydata<-read.xlsx(filepath,sheetName="Data",header=TRUE)
curve<-data.frame(GateV=mydata$GateV,DrainI=mydata$DrainI)
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
start<-as.numeric(winDialogString(message="Input the start of linear area",default=""))
end<-as.numeric(winDialogString(message="Input the end of linear area",default=""))

linearDrainI<-curve1[which(U<start&U>end),2]
linearGateV<-curve1[which(U<start&U>end),1]
linearArea<-data.frame(DrainI=linearDrainI,GateV=linearGateV)

####fit
parameter<-lm(DrainI~GateV,linearArea)
plot(curve1)
abline(reg=parameter,lty=1)
var2<-coef(parameter)[2]
var1<-coef(parameter)[1]
mobility1<-var2^2/(widthLengthRatio*Ci/2)
#var1/sqrt(widthLengthRatio*Ci/2)
thresVoltage1<-var1/var2
thresVoltage1
mobility1
