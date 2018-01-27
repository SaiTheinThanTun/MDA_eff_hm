#basis of output storage
#first create a matrix of output with 3 columns (time, MDAsuccess@Village1, MDAsuccess@Village2)
#put each matrix in a list with index i

a <- list()
b <- matrix(NA, 60,100)

for(i in 1:60){
  for(j in 1:100){
    b[i,j] <- paste(i,j)
    #print(i+j)
    #a[i+j][1] <- 0
  }
}

for(i in 1:3){
  a[[i]] <- b
}