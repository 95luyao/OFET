library(xlsx)
library(ggplot2)
#READ IN FILES
voltage=-50
path<-"C:/Users/LY/Desktop/star on devices/"
path_light<-"C:/Users/LY/Desktop/all devices/"
doc.names <- dir(path)
doc.names_light<-dir(path_light)

for(i in seq(from=2,to=length(doc.names),by=2)){doc.names[i/2]<-doc.names[i]}
xls.names<-doc.names[1:(length(doc.names)/2)]

for(i in seq(from=2,to=length(doc.names_light),by=2)){doc.names_light[i/2]<-doc.names_light[i]}
xls.names_light<-doc.names_light[1:(length(doc.names_light)/2)]

doc.path <- sapply(xls.names,function(names) paste(path,names,sep=''))
doc.path_light<-sapply(xls.names,function(names) paste(path_light,names,sep=''))



for(i in seq(from=1,to=length(xls.names_light),by=1)){
  assign(xls.names_light[i], 1)
}

for(i in seq(from=1,to=length(xls.names),by=1)){
  mydata<-read.xlsx(doc.path[i],sheetName="Data",header=TRUE)
  curve<-data.frame(DrainI=mydata$DrainI,GateV=mydata$GateV)
  final_dark<-curve[which(curve[,2]==voltage),1]
  
  
  mydata<-read.xlsx(doc.path_light[i],sheetName="Data",header=TRUE)
  curve<-data.frame(DrainI=mydata$DrainI,GateV=mydata$GateV)
  final_light<-curve[which(curve[,2]==voltage),1]
  final<-min(final_dark)/max(final_light)
  assign(xls.names[i], final)
}
'vgs-id-73.xls'<-1
'vgs-id-83.xls'<-1

matrix1<-paste("vgs-id-0",seq(from=0,to=9,by=1),sep="",".xls")
length(matrix1)<-100
for(i in 11:100){
  matrix1[i]<-paste("vgs-id-",i-1,sep="",".xls")
}

for(i in 1:100)
{
  data[i]<-get(matrix1[i])
}
out<-as.numeric(data)
output<-matrix(unlist(out), ncol = 10, byrow = TRUE)

write.xlsx(output, "d:/mydata50.xlsx")
