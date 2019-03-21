#MDA_eff_homo_cov_forloop.R created only the RDS save files
#since one of the results doesn't have adequate rows
#each RDS file is read in and plotted individually

setwd("~/OneDrive/MORU/Projects/TCE_MDA effect/MDA_eff_hm/") #mac
library(deSolve)
library(shiny)
library(TSA)
library(Rcpp)
library(stringr)
library(lattice)
sourceCpp("functions/modGMS.cpp")
source("functions/no longer app.R")
tm_1 <- 9
timeVector <- read.csv('parameters/times.csv')
MDAstart <- which(timeVector==(2018+tm_1/12))

cmda_1Loop <- seq(0, by=10, to=90) # to=70)
successwithin <- 12 #6

#tmp2 <- NA
#for(loop in 1:10){
# for(loop in seq(1:10)[-2]){
#loop
loop <- 10
result <- readRDS(paste("results_homo_cov/results_loop_", loop,".rds", sep=""))
#result <- readRDS("results_homo_cov/results_2019-02-14 150440.rds")
cmda_1 <- cmda_1Loop[loop]#80 #90

#find where the invalid result is
tmp <- sapply(result, function(x){(nrow(x)<193)})
#tmp2[loop] <- sum(tmp)
#which(tmp==TRUE)

#result[[1501]]

village1 <- sapply(result, function(x){
  sum(x[MDAstart:(MDAstart+successwithin),1])>0
})
village2 <- sapply(result, function(x){
  sum(x[MDAstart:(MDAstart+successwithin),2])>0
})

#putting into matrix
v1m <- matrix(as.numeric(village1),nrow=80,ncol=100, byrow=TRUE)
#heatmap(v1m, Rowv=NA, Colv = NA)
#v1md <- as.data.frame(as.numeric(village1),nrow=80,ncol=100, byrow=TRUE)
levelplot(t(v1m))

v2m <- matrix(as.numeric(village2),nrow=80,ncol=100, byrow=TRUE)
#heatmap(v2m, Rowv=NA, Colv = NA)
levelplot(t(v2m))

v12m <- matrix(as.numeric(village1),nrow=80,ncol=100, byrow=TRUE)+matrix(as.numeric(village2),nrow=80,ncol=100, byrow=TRUE)
#heatmap(v12m, Rowv=NA, Colv = NA, col=heat.colors(3))
#write.csv(v12m,'results_homo_cov/v12m.csv')

new.palette=colorRampPalette(c("red","black"),space="rgb")
levelplot(t(v12m), col.regions=new.palette, xlab="% of homogeniety", ylab="% of MDA coverage in village 2", main="No. of villages reaching below elimination threshold")

png(paste('results_homo_cov/newPlot/homogeniety_MDAcoverage',gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300)
levelplot(t(v12m), col.regions=new.palette, xlab="% of homogeniety", ylab="% of MDA coverage in village 2", main=paste("No. of villages reaching below elimination threshold \n MDA coverage in village 1:", cmda_1))
dev.off()
#}
###

