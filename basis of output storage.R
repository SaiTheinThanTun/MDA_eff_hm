#basis of output storage
#first create a matrix of output with 3 columns (time, MDAsuccess@Village1, MDAsuccess@Village2)
#put each matrix in a list with index i

#x axis variable: homogen, [0 to 100] %
#y axis variable: cmda_2, [0 to 80] %, coverage of MDA in second village


a <- list()
b <- matrix(NA, 80,100)

for(i in 1:80){
  for(j in 1:100){
    b[i,j] <- paste(i,j)
    #print(i+j)
    #a[i+j][1] <- 0
    a[[((i-1)*100)+j]] <- b
  }
}

# for(i in 1:3){
#   a[[i]] <- b
# }
#inputing 'for' loop
#first option
testfor <- matrix(NA, 80*100,2)

for(i in 1:80){
  for(j in 1:100){
    testfor[((i-1)*100)+j,1] <- j
    testfor[((i-1)*100)+j,2] <- i
  }
}

#second option
testfor2j <- rep(1:100,80)
testfor2i <- rep(1:80,each=100)
testfor2 <- cbind(testfor2j,testfor2i)

#testing if they are equal
all(testfor==testfor2)
