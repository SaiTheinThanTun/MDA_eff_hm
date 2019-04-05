#MDA_eff_homo_cov_forloop.R created only the RDS save files
#since one of the results doesn't have adequate rows
#each RDS file is read in and plotted individually

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

timeVector <- read.csv('parameters/times.csv')

homoLoop <- seq(0, by=10, to=100) # to=70)


#tmp2 <- NA
for(loop in 1:11){
result <- readRDS(paste("results_covV12_bh_maxV2_homo/results_loop_", loop,".rds", sep=""))

  #testing####
#loop <- 1
#result <- readRDS("results_homo_cov_start0/results_loop_1_2019-03-22 141427.rds")
#result <- readRDS("results_homo_cov/results_2019-02-14 150440.rds")
#cmda_1 <- cmda_1Loop[loop]#80 #90
homogen <- homoLoop[loop]


#how soon is the outcome?

village1 <- sapply(result, function(x){
  x[,2]<1
  
})
village2 <- sapply(result, function(x){
  x[,4]<1
  
})


#putting into matrix
v1m <- matrix(as.numeric(village1),nrow=101,ncol=81, byrow=TRUE)
v2m <- matrix(as.numeric(village2),nrow=101,ncol=81, byrow=TRUE)
v12m <- matrix(as.numeric(village1),nrow=101,ncol=81, byrow=TRUE)+matrix(as.numeric(village2),nrow=101,ncol=81, byrow=TRUE)
v12m <- v12m[-101,] #MDA at 100% produces NA values, therefore it is removed

toPlot <- melt(t(v12m))

#within one year period####
png(paste('results_covV12_bh_maxV2_homo/newPlot_OneYrInc/HBR_MDAcoverage_atHomogen_',homoLoop[loop],"_",gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300)

print(
ggplot(data=toPlot, aes(x=X1, y=X2))+
  geom_tile(aes(fill=factor(value)))+
  ggtitle(paste0("No. of village with less than 1 case/1000 within 1 year after MDA\nHBR in village 1: 16; % of homogeniety: ",homogen))+
  xlab("HBR in village 2")+ylab("% of MDA coverage in both villages")+
  theme(legend.position = "bottom")+
  scale_fill_manual(name="# of village", labels=c("zero", "one village", "two villages"),values=c("#999999", "#E69F00", "#56B4E9"))
)
dev.off()
}
