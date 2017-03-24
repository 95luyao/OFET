####
#This will be a automatic processing program
library(MASS)
library(xlsx)
library(ggplot2)

path<-"~/R-Project/optical/PVA10"
file.names <-dir(path)
file.path <- sapply(file.names,function(names) paste(path,names,sep='/'))

#filepath<-"E:/A/vgs-id_A1.xls"   #Your filepath
numberArise = -20  ##to avoid wrong fit, select the start of the linear
numberStop = -60
widthLengthRatio<-32.5  #W/L=33
Ci<-10^-9
mobility<-1
length(mobility)<-length(file.names)
thresVoltage<-1
length(thresVoltage)<-length(file.names)

for(i in seq(from=1,to=length(file.path),by=1)){
mydata<-read.xlsx(file.path[i],sheetName="Data",header=TRUE)
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
ggplot(curve2,aes(x=GateV,y=DrainI))+geom_point()


####????
parameter<-lqs(DrainI~GateV,subset(curve2,GateV < numberArise& GateV > numberStop,select=DrainI:GateV))
plot(curve2)
abline(reg=parameter,lty=1)
var2<-coef(parameter)[2]
var1<-coef(parameter)[1]
mobility[i]<-var2^2/(widthLengthRatio*Ci/2)
#var1/sqrt(widthLengthRatio*Ci/2)
thresVoltage[i]<--var1/var2
#thresVoltage[i]
#mobility[i]

#The end
}
information<-data.frame(file.names,mobility,thresVoltage)
