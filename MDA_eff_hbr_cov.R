#MDA_eff_hm
#MDA success on hbr vs coverage
#20180213

#see #scenario tag for things to change in each scenario
#x axis variable: hb_max, [1 to 25] %
xval <- 25
#y axis variable: cmda_2, [1 to 80] %, coverage of MDA in second village
yval <- 80

setwd("~/OneDrive/MORU/Projects/TCE_MDA effect/MDA_eff_hm/") #mac
library(deSolve)
library(shiny)
library(TSA)
library(Rcpp)
library(stringr)
library(lattice)
sourceCpp("functions/modGMS.cpp")
source("functions/no longer app.R")

#scenario
##initialize input and output storage####
testfor2j <- rep(1:xval,yval)
testfor2i <- rep(1:yval,each=xval)
testfor2 <- cbind(testfor2j,testfor2i)
# colnames(testfor2) <- c('homogen','cmda_2')
colnames(testfor2) <- NULL
successwithin <- 6

result <- list()
#scenario
####for template####
# for(i in 1:yval){
#   for(j in 1:100){
#     homogen <- testfor2[((i-1)*100)+j,1]
#     cmda_2 <- testfor2[((i-1)*100)+j,2]
#     ###other codes for running the model
#     result[[((i-1)*100)+j]] <- successMDA
#   }
# }

####non-reactive parameters####
####interventions####
EDATon = TRUE
ITNon = TRUE
IRSon = FALSE
MDAon = TRUE
primon = FALSE
MSATon = TRUE
VACon = FALSE

####non-reactive functions####
#got from the "parameters" folder #scenario
API <- 2.5
eta <- 30
covEDAT0 <- 25
covITN0 <- 70
effITN <- 30
covIRS0 <- 0
effIRS <- 15
muC <- 1
muA <- 1
muU <- 1
percfail2018 <- 5
percfail2019 <- 15
percfail2020 <- 30
#bh_max0 <- 16 #scenario
#bh_max1 <- 16 #scenario
rhoa <- 55
rhou <- 17
EDATscale <- 1
covEDATi <- 70
ITNscale <- 1
covITNi <- 90
IRSscale <- 1
covIRSi <- 90
lossd <- 30
dm0 <- 3
dm1 <- 3
cmda_1 <- 90
#cmda_2 <- 50 #scenario
homogen <- 20 #scenario, 80, 50, 20
tm_1 <- 9
tm_2 <- 9
p1v <- 0.5
effv_1 <- 75
effv_2 <- 80
vh <- 90
MSATscale <- 1
covMSATi <- 90
MSATsensC <- 99
MSATsensA <- 87
MSATsensU <- 44




#non-reactive parameters
# define the number of weeks to run the model
dt<-1/12
startyear<-2007
stopyear<-2023
maxt<-stopyear-startyear
times <- seq(0, maxt, by = dt)
tsteps<-length(times)

# initial prevalence
initprevR <- (0.001*API)

#ParLabel <- read.table('functions/ParLabel.csv', sep=",", as.is=TRUE)

# scenario_0<-c(EDATon = 0,
#               ITNon = 0,
#               IRSon = 0,
#               MDAon = 0,
#               primon = 0,
#               MSATon = 0,
#               VACon = 0)


####for loop#####
for(i in 1:yval){
  for(j in 1:xval){
    #scenario
    bh_max0 <- bh_max1 <- testfor2[((i-1)*xval)+j,1]
    cmda_2 <- testfor2[((i-1)*xval)+j,2]
    ###other codes for running the model
    
    scenario_iR<-(c(EDATon = EDATon,
                    ITNon = ITNon,
                    IRSon = IRSon,
                    MDAon = MDAon,
                    primon = primon,
                    MSATon = MSATon,
                    VACon = as.numeric(VACon)))
    
    parametersR <- (c(
      bh_max0 = bh_max0,                 # bites per human per night
      bh_max1 = bh_max1,
      eta = eta,
      covEDAT0 = covEDAT0,
      covITN0 = covITN0,
      effITN = effITN,
      covIRS0 = covIRS0,
      effIRS = effIRS,
      muC = muC,
      muA = muA,
      muU = muU,
      percfail2018 = percfail2018,
      percfail2019 = percfail2019,
      percfail2020 = percfail2020,
      
      EDATscale = EDATscale,
      covEDATi = covEDATi,
      ITNscale = ITNscale,
      covITNi = covITNi,
      IRSscale = IRSscale,
      covIRSi = covIRSi,
      cmda_1 = cmda_1,
      cmda_2 = cmda_2,
      
      tm_1 = tm_1,          # timing of 1st round [2018 to 2021 - 1 month steps]
      tm_2 = tm_2,          # timing of 2nd round [2018+(1/12) to 2021 - 1 month steps]
      
      dm0 = dm0,
      dm1 = dm1,
      lossd = lossd,
      
      MSATscale = MSATscale,
      covMSATi = covMSATi,
      MSATsensC = MSATsensC,
      MSATsensA = MSATsensA,
      MSATsensU = MSATsensU,
      
      effv_1 = effv_1,
      effv_2 = effv_2,
      
      vh = vh,
      homogen = homogen,
      p1v = p1v,
      
      rhoa=rhoa,
      rhou=rhou
    ))
    
    
    
    #GMSout0R <- (runGMS(initprevR, scenario_0,parametersR))
    
    GMSoutiR <- (runGMS(initprevR, scenario_iR,parametersR))
    
    #labeling the columns
    outLab <- c("year","detectedIncidence1","totalIncidence1","prevalence1","detectedIncidence2","totalIncidence2","prevalence2")
    colnames(GMSoutiR) <- outLab
    
    #grabbing the time of MDA success
    #GMSoutiR[GMSoutiR[,3]<(1/12),1]
    MDAsuccessV1 <- GMSoutiR[,3]<(1/12)
    MDAsuccessV2 <- GMSoutiR[,6]<(1/12)
    
    successMDA <- cbind(MDAsuccessV1, MDAsuccessV2)
    
    result[[((i-1)*xval)+j]] <- successMDA #scenario
  }
}
#time component #scenario
#outside of 'for' loop
#write.table(GMSoutiR[,1],'parameters/times.csv', col.names = 'time', row.names = FALSE)
saveRDS(result, paste('results_hbr_cov/results_',gsub("\\:","",Sys.time()),'.rds',sep=''))
#scenario

#Analysing the data list 'results_.rds'####
#readRDS

timeVector <- read.csv('parameters/times.csv')
MDAstart <- which(timeVector==(2018+tm_1/12))

#draft
#result[[1]][MDAstart:(MDAstart+successwithin),]
#sum(result[[1]][MDAstart:(MDAstart+successwithin),1])>0

village1 <- sapply(result, function(x){
  sum(x[MDAstart:(MDAstart+successwithin),1])>0
})
village2 <- sapply(result, function(x){
  sum(x[MDAstart:(MDAstart+successwithin),2])>0
})

#putting into matrix
v1m <- matrix(as.numeric(village1),nrow=yval,ncol=xval, byrow=TRUE)
#heatmap(v1m, Rowv=NA, Colv = NA)
#v1md <- as.data.frame(as.numeric(village1),nrow=yval,ncol=100, byrow=TRUE)
levelplot(t(v1m))

v2m <- matrix(as.numeric(village2),nrow=yval,ncol=xval, byrow=TRUE)
#heatmap(v2m, Rowv=NA, Colv = NA)
levelplot(t(v2m))

v12m <- matrix(as.numeric(village1),nrow=yval,ncol=xval, byrow=TRUE)+matrix(as.numeric(village2),nrow=yval,ncol=xval, byrow=TRUE)
#heatmap(v12m, Rowv=NA, Colv = NA, col=heat.colors(3))
#write.csv(v12m,'results_homo_cov/v12m.csv')

new.palette=colorRampPalette(c("red","black"),space="rgb")
levelplot(t(v12m), col.regions=new.palette, xlab="HBR", ylab="% of MDA coverage in village 2", main="No. of villages reaching below elimination threshold")

png(paste('results_hbr_cov/hbr_MDAcoverage',gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300)
levelplot(t(v12m), col.regions=new.palette, xlab="HBR", ylab="% of MDA coverage in village 2", main=paste("No. of villages reaching below elimination threshold\n % homogeniety=",homogen, sep = ""))
dev.off()

