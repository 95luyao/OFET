library(xlsx)
library(ggplot2)
path<-"~/R-Project/optical/PVA5"
file.names <-dir(path)
file.path <- sapply(file.names,function(names) paste(path,names,sep='/'))

{i=1
 data<-read.xlsx(file.path[i],sheetName="Data",header=TRUE)
 curve<-data.frame(DrainI=data$DrainI,GateV=data$GateV)
 U<-curve$GateV
 I<-curve$DrainI
 curve1<-curve
 curve2<-curve
 row<-nrow(curve);
 for(k in 1:row){
   curve1[k,1]=abs(min(I[which(U==U[k])]))
   curve1[k,2]=U[k]
   curve2[k,1]=abs(max(I[which(U==U[k])]))
   curve2[k,2]=U[k]}
 curve0A<-curve1
 curve0B<-curve2
}

{i=3
 data<-read.xlsx(file.path[i],sheetName="Data",header=TRUE)
 curve<-data.frame(DrainI=data$DrainI,GateV=data$GateV)
 U<-curve$GateV
 I<-curve$DrainI
 curve1<-curve
 curve2<-curve
 row<-nrow(curve);
 for(k in 1:row){
   curve1[k,1]=abs(min(I[which(U==U[k])]))
   curve1[k,2]=U[k]
   curve2[k,1]=abs(max(I[which(U==U[k])]))
   curve2[k,2]=U[k]}
 curve100A<-curve1
 curve100B<-curve2
# for(i in seq(from=1,to=nrow(curve0A),by=1))
#{
#   c<-curve100B[i,1]/curve0B[i,1]
  # if(c<0||c>1000)
 # {c<-NA}
#   curve100B[i,1]<-c
# }
 plot(curve100B$GateV,curve100B$DrainI,pch=20,type="b",xlab="GateV",ylab="Ratio",las=1,bg="blue")
 title("PVA(10%)+DNTT(60/0)",font.main=3)
}

for(i in seq(from=1,to=length(file.path),by=1)){
  data<-read.xlsx(file.path[i],sheetName="Data",header=TRUE)
  curve<-data.frame(DrainI=data$DrainI,GateV=data$GateV)
  ####more efficient way to 
  U<-curve$GateV
  I<-curve$DrainI
  curve1<-curve
  curve2<-curve
  row<-nrow(curve)
  for(k in 1:row){
    curve1[k,1]=abs(min(I[which(U==U[k])]))
    curve1[k,2]=U[k]
    curve2[k,1]=abs(max(I[which(U==U[k])]))
    curve2[k,2]=U[k]}
#  for(j in seq(from=1,to=nrow(curve0A),by=1))
# {
#    c<-curve2[j,1]/curve0A[j,1]
#    if(c<0||c>1000)
#   {c<-NA}
#    curve2[j,1]<-c
# }
  
  points(curve2$GateV,curve2$DrainI,pch=20)
  lines(curve2$GateV,curve2$DrainI)
}



#for(i in seq(from=1,to=nrow(curve0A),by=1))
#{
#  c<-curve100A[i,1]/curve0A[i,1]
#  if(c<0||c>100000)
#  {c<-NA}
#  a[i,1]<-c
#}
#ggplot(a,aes(x=GateV,y=DrainI))+geom_point(size=10)