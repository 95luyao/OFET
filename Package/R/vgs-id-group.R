library(xlsx)
library(ggplot2)
path<-"C:/Users/LY/Desktop/data/test"
file.names <-dir(path)
file.path <- sapply(file.names,function(names) paste(path,names,sep='/'))
for(i in seq(from=1,to=length(file.path),by=1)){
  data<-read.xlsx(file.path[i],sheetName="Data",header=TRUE)
  curve<-data.frame(DrainI=data$DrainI,GateV=data$GateV)
  U<-curve$GateV
  I<-curve$DrainI
  curve1<-curve
  curve2<-curve
  row<-nrow(curve);
  for(k in 1:row){
    curve1[k,1]=min(I[which(U==U[k])])
    curve1[k,2]=U[k]
    curve2[k,1]=max(I[which(U==U[k])])
    curve2[k,2]=U[k]} 
  #qplot(curve2$GateV,curve2$DrainI)
  points(curve2$GateV,curve2$DrainI)
  lines(curve2$GateV,curve2$DrainI)
}

{i=5
 data<-read.xlsx(file.path[i],sheetName="Data",header=TRUE)
 curve<-data.frame(DrainI=data$DrainI,GateV=data$GateV)
 U<-curve$GateV
 I<-curve$DrainI
 curve1<-curve
 curve2<-curve
 row<-nrow(curve);
 for(k in 1:row){
   curve1[k,1]=min(I[which(U==U[k])])
   curve1[k,2]=U[k]
   curve2[k,1]=max(I[which(U==U[k])])
   curve2[k,2]=U[k]}
 curve100A<-curve1
 curve100B<-curve2
}
{i=2
 data<-read.xlsx(file.path[i],sheetName="Data",header=TRUE)
 curve<-data.frame(DrainI=data$DrainI,GateV=data$GateV)
 U<-curve$GateV
 I<-curve$DrainI
 curve1<-curve
 curve2<-curve
 row<-nrow(curve);
 for(k in 1:row){
   curve1[k,1]=min(I[which(U==U[k])])
   curve1[k,2]=U[k]
   curve2[k,1]=max(I[which(U==U[k])])
   curve2[k,2]=U[k]}
 curve0A<-curve1
 curve0B<-curve2
}
for(i in seq(from=1,to=nrow(curve0A),by=1))
{
  c<-curve100A[i,1]/curve0A[i,1]
  if(c<0||c>100000)
  {c<-NA}
  a[i,1]<-c
}
ggplot(a,aes(x=GateV,y=DrainI))+geom_point(size=10)