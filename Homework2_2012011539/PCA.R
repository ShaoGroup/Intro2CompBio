PCA<-function(data,rerr){
  sd=scale(data)  # normalization
  c=cor(sd)
  e=eigen(c)
  v=e$value    # eigenvalues
  su=sum(v)
  m=dim(data)[1]
  n=dim(data)[2]
  num=0
  for(i in 1:m){  # find suitable number of eigenvalues and eigenvectors according to 'rerr'
    num=num+v[i]
    if((num/su)>(1-rerr))
      break
  }
  pcs=e$vectors[,1:i]
  cprs_data=data%*%pcs  # compressed data
  return(cprs_data)
}