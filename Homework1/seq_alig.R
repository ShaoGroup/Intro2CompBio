# s and t represent the strings to compare
# a(b) represent the bonus(punishment) for single character matching
# c represent the punishment for matching the null
seq_alig<-function(s,t,a=1,b=-1,c=-1){
  n<-nchar(s)
  m<-nchar(t)
  mat<-array(0,c(n+1,m+1)) #this matrix stores cost of optimum alignment
  flag<-array(0,c(n+1,m+1)) #this matrix stores the optimum precursor for every point
  path<-array(0,c(n+1,m+1))  #this matrix records the information about the optimum path
  # (I use '1' to mark the point on the path)
  path[1,1]=1
  path[n+1,m+1]=1
  for(i in 2:(m+1)){  #initialization for the first row
    mat[1,i]<-(i-1)*c
    flag[1,i]<-3
  }
  for(i in 2:(n+1)){  #initialization for the first col
    mat[i,1]<-(i-1)*c
    flag[i,1]<-2
  }
  k=1;
  for(i in 2:(n+1)){  # dynamic programming
    for(j in 2:(m+1)){
      if(substr(s,i-1,i-1)==substr(t,j-1,j-1)){
        w<-a
        k<-1
      }
      else{
        w<-b
        k<--1
      }
      co<-c(mat[i-1,j-1]+w,mat[i-1,j]+c,mat[i,j-1]+c)
      mat[i,j]<-max(co)
      flag[i,j]<-k*which.max(co)
    }
  }
  i<-n+1
  j<-m+1
  str<-NULL
  k<-0
  while(i>1||j>1){  # maximal matching of strings
    q=flag[i,j]
    if(q==1||q==-1){
      i<-i-1
      j<-j-1
      if(q==1){
        str<-paste(substr(s,i,i),str) # Increments a character in each loop iteration
        k<-k+1
      }
    }
    if(q==2||q==-2){
      i<-i-1
    }
    if(q==3||q==-3){
      j<-j-1
    }
    path[i,j]<-1
  }
  write.table(str,file="result.txt",row.names = F,quote = F) #store the string
  print("cost of optimum alignment:")
  print(mat[n+1,m+1])
  #print("matching path:")  #for the first question
  #print(path)
  print("maximal matching of strings:") #for the second question
  print(str)
  print("the length of maximal matching of strings")
  print(k)
}
  