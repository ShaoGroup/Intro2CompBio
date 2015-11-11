#Following parameters are the alpha,beta.gamma and two sequences to be matched
alpha <- 10
beta <- -2
gamma <- -5
raw2 <- 'CTCGCAGC'
raw1 <- 'CATTCAC'
seq1<-0
seq2<-0

data <- as.data.frame(0)
path <- as.data.frame(0)
seq1 <- as.data.frame(seq1)
seq2 <- as.data.frame(seq2)

m <- function(a,b)
{
  if(a == b)
    alpha
  else
    beta
}

#Make raw1 the shorter one
if(nchar(raw1) > nchar(raw2))
{
  temp <- raw1
  raw1 <-raw2
  raw2<-temp
  rm(temp)
}

#put sequence into the data frame
for(i in 1:nchar(raw1))
{
  seq1[i,1] = substr(raw1,i,i) 
}
for(i in 1:nchar(raw2))
{
  seq2[i,1] = substr(raw2,i,i) 
}

h <- length(seq1$seq1) + 1
w <- length(seq2$seq2) + 1

for(i in 1:w)
{
  data[1,i] = gamma * (i - 1)
  path[1,i] = 5
}
for(i in 1:h)
{
  data[i,1] = gamma * (i - 1)
  path[i,1] = 2
}

path[1,1] = 0

#Filling the matrix
for(i in 2:h)
{
  for(j in 2:w)
  {
    data[i,j] =  max(data[i - 1,j - 1] + m(seq1[i - 1,1],seq2[j - 1,1]),data[i - 1,j] + gamma,data[i,j - 1] + gamma)
    path[i,j] = 0
    if(data[i,j] == data[i - 1,j - 1] + m(seq1[i - 1,1],seq2[j - 1,1]))
      path[i,j] = path[i,j] + 1
    if(data[i,j] == data[i - 1,j] + gamma)
      path[i,j] = path[i,j] + 2
    if(data[i,j] == data[i,j - 1] + gamma)
      path[i,j] = path[i,j] + 5
  }
}

show(max(data))
again <- 1
count <- 0
#Priting all the possibilities
while(again > 0)
{
  position_i <- h
  position_j <- which(data[h,] == max(data))
  seq1_final <- seq2
  seq2_final <- seq2
  seq1_final[,1] <- ""
  seq2_final[,1] <- ""
  point <- 1
  p2 <- nchar(raw2)
  p1 <- nchar(raw1)
  seq1_final[point,1] <- seq1[p1,1]
  seq2_final[point,1] <- seq2[p2,1]
  point <- point + 1
  p1 <- p1 - 1;
  p2 <- p2 - 1;
  again <- 1
  while(position_i > 2 || position_j > 2)
  {
    if(path[position_i,position_j] == 5)
    {
      seq1_final[point,1] = "_"
      seq2_final[point,1] = seq2[p2,1]
      p2 = p2 - 1
      position_j = position_j - 1
    }
    else if(path[position_i,position_j] == 2)
    {
      seq1_final[point,1] = seq1[p1,1]
      seq2_final[point,1] = "_"
      p1 = p1 - 1
      position_i = position_i - 1
    }
    else if(path[position_i,position_j] == 1)
    {
      seq1_final[point,1] = seq1[p1,1]
      seq2_final[point,1] = seq2[p2,1]
      p1 = p1 - 1
      p2 = p2 - 1
      position_i = position_i - 1
      position_j = position_j - 1
    }
    #if there are multiple ways first consider andscape way then the portrait way
    # and last the diagonal way
    else if(path[position_i,position_j] == 6)
    {
      seq1_final[point,1] = "_"
      seq2_final[point,1] = seq2[p2,1]
      p2 = p2 - 1
      path[position_i,position_j] = path[position_i,position_j] - 5
      position_j = position_j - 1
      again = again + 1
    }
    else if(path[position_i,position_j] == 3)
    {
      seq1_final[point,1] = seq1[p1,1]
      seq2_final[point,1] = "_"
      p1 = p1 - 1
      path[position_i,position_j] = path[position_i,position_j] - 2
      position_i = position_i - 1
      again = again + 1
    }
    else if(path[position_i,position_j] == 7)
    {
      seq1_final[point,1] = "_"
      seq2_final[point,1] = seq2[p2,1]
      p2 = p2 - 1
      path[position_i,position_j] = path[position_i,position_j] - 5
      position_j = position_j - 1
      again = again + 1
    }
    else if(path[position_i,position_j] == 8)
    {
      seq1_final[point,1] = "_"
      seq2_final[point,1] = seq2[p2,1]
      p2 = p2 - 1
      path[position_i,position_j] = path[position_i,position_j] - 5
      position_j = position_j - 1
      again = again + 1
    }
    point <- point + 1
  }
  
  #Print the final match sequence
  print1 <- ""
  print2 <- ""
  for(i in length(seq1_final$seq2):1)
  {
    print1 <- paste(print1,seq1_final[i,1])
    print2 <- paste(print2,seq2_final[i,1])
  }
  count <- count + 1
  show(c("Path",count))
  show(print1)
  show(print2)
  again <- again - 1
}