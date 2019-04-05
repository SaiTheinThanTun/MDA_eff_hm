#MDA_eff_homo_cov_forloop.R created only the RDS save files
#since one of the results doesn't have adequate rows
#each RDS file is read in and plotted individually

#to change the coloring to better represent achievement in separate village
#to think about low & high relative prevalence for additional 6 plots

setwd("~/OneDrive/MORU/Projects/TCE_MDA effect/MDA_eff_hm/") #mac
library(deSolve)
library(shiny)
library(TSA)
library(Rcpp)
library(stringr)
#library(lattice)
library(ggplot2)
library(reshape)
sourceCpp("functions/modGMS.cpp")
source("functions/no longer app.R")
tm_1 <- 9
timeVector <- read.csv('parameters/times.csv')
MDAstart <- which(timeVector==(2018+tm_1/12))

cmda_1Loop <- seq(70, by=10, to=90) # to=70)
successwithin <- 12 #6

#tmp2 <- NA
for(loop in 1:3){
result <- readRDS(paste("results_homo_cov_start0_seas/results_loop_", loop,".rds", sep=""))

  #testing####
#loop <- 1
#result <- readRDS("results_homo_cov_start0/results_loop_1_2019-03-22 141427.rds")
#result <- readRDS("results_homo_cov/results_2019-02-14 150440.rds")
cmda_1 <- cmda_1Loop[loop]#80 #90


#how soon is the outcome?
#within the "successwithin" period####
village1 <- sapply(result, function(x){
  x[,2]<1
  #sum(x[MDAstart:(MDAstart+successwithin),1])>0
})
village2 <- sapply(result, function(x){
  x[,4]<1
  #sum(x[MDAstart:(MDAstart+successwithin),2])>0
})

#at exactly "successwithin" from MDA start####
# village1 <- sapply(result, function(x){
#   x[(MDAstart+successwithin),1]==TRUE
# })
# village2 <- sapply(result, function(x){
#   x[(MDAstart+successwithin),2]==TRUE
# })

#putting into matrix
v1m <- matrix(as.numeric(village1),nrow=100,ncol=101, byrow=TRUE)
v2m <- matrix(as.numeric(village2),nrow=100,ncol=101, byrow=TRUE)
v12m <- matrix(as.numeric(village1),nrow=100,ncol=101, byrow=TRUE)+matrix(as.numeric(village2),nrow=100,ncol=101, byrow=TRUE)

toPlot <- melt(t(v12m))

#within the "successwithin" period####
png(paste('results_homo_cov_start0_seas/newPlot_OneYrInc/homogeniety_MDAcoverage_',cmda_1Loop[loop],"_",gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300)
#at exactly "successwithin" from MDA start####
#png(paste('results_homo_cov_start0/newPlot_exactlyAt1Yr/homogeniety_MDAcoverage_',cmda_1Loop[loop],"_",gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300)

print(
ggplot(data=toPlot, aes(x=X1, y=X2))+
  geom_tile(aes(fill=factor(value)))+
  ggtitle(paste0("No. of village with less than 1 case/1000 within 1 year after MDA\nMDA coverage in village1: ",cmda_1))+
  xlab("% of homogeniety")+ylab("% of MDA coverage in village 2")+
  theme(legend.position = "bottom")+
  scale_fill_manual(name="# of village", labels=c("zero", "one village", "two villages"),values=c("#999999", "#E69F00", "#56B4E9"))
)
dev.off()
}
