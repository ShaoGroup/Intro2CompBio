HC<-function(patient){
  #patient<-read.table("GeneMatrix.txt",sep ='\t',header=TRUE,as.is=TRUE) # read in the data
  patient<-t(patient)
  dist.e<-dist(patient,method='euclidean')  # hierarchical clustering using euclidean
  heatmap(as.matrix(dist.e),labRow = F, labCol = F,main = 'hierarchical clustering') #Cluster map 
  model1<-hclust(dist.e)
  result<-cutree(model1,k=2) # classification according to 'k'
  info<-read.table("clinical_data",sep ='\t',header=TRUE,as.is=TRUE) # read in somemore information to find the criteria to analysis the result
  m<-length(result)
  cr<-rep("positive",m) 
  for(i in 1:m){
    str<-attr(result,"name")[i]
    s<-unlist(strsplit(str, "\\."))
    cg<-paste(s[1],s[2],s[3],s[4],sep="-")
    #temp<-subset(info,sampleID==attr(result,"name")[i])
    temp<-subset(info,sampleID==cg)   
    if(dim(temp)[1]==0)
      cr[i]<-"Notrecognized" # if the patient do not have the corresponding information, a label of 'Notrecognized' will be defined
    else
      cr[i]<-temp$ER_Status_nature2012
  }
  x<-patient[,1]  # Here I use the first two variables to show the result
  y<-patient[,2]
  ER_Status_nature2012<-cr
  library(ggplot2)
  p<-ggplot(data.frame(x,y,ER_Status_nature2012,result),aes(x,y))
  p+geom_point(size=3,alpha=0.8,aes(colour=factor(result),shape = factor(ER_Status_nature2012)))
}