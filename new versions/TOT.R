#for loop for changing MDA coverage in the first village

#MDA_eff_hm
#MDA success on homogeniety vs coverage
#20190405
#outcome is the achievement of <1/1000/year in a year starting with the end of the 3rd MDA round

#see #scenario tag for things to change in each scenario
#x axis variable: homogen, [0 to 100] %
#y axis variable: cmda_2, [0 to 80] %, coverage of MDA in second village
#z: cmda_1, [70, 80, 90] %

setwd("~/OneDrive/MORU/Projects/TCE_MDA effect/MDA_eff_hm/") #mac
library(deSolve)
library(shiny)
library(TSA)
library(Rcpp)
library(stringr)
library(lattice)
#new formula with Ricardo 20190805
sourceCpp("functions_latest/modGMS_seas.cpp", rebuild = TRUE)
source("functions_latest/no longer app.R")
# sourceCpp("functions/modGMS_seas.cpp")
# source("functions/no longer app.R")

timeVector <- read.csv('parameters/times.csv') #to figure out when the MDA finishes

#scenario
##initialize input and output storage####
testfor2j <- rep(0:100,100) #homogen
testfor2i <- rep(0:99,each=101) #cmda_2
testfor2 <- cbind(testfor2j,testfor2i)
# colnames(testfor2) <- c('homogen','cmda_2')
colnames(testfor2) <- NULL


#scenario
####for template####
# for(i in 1:81){
#   for(j in 1:101){
#     homogen <- testfor2[((i-1)*101)+j,1]
#     cmda_2 <- testfor2[((i-1)*101)+j,2]
#     ###other codes for running the model
#     result[[((i-1)*101)+j]] <- successMDA
#   }
# }

####non-reactive parameters####
####interventions####
EDATon = TRUE
ITNon = TRUE
IRSon = FALSE
MDAon = TRUE
primon = TRUE #FALSE
MSATon = TRUE
VACon = FALSE

####non-reactive functions####
#got from the "parameters" folder #scenario
#cmda_1Loop <- seq(70, by=10, to=90) #1st batch 20190407
# cmda_1Loop <- seq(30, by=10, to=60) #2nd batch 20190409
#cmda_1Loop <- seq(10, by=10, to=20) #3rd batch 20190409
#cmda_1Loop <- seq(70, by=10, to=90) #20190522
cmda_1Loop <- 81.56 #TOT
for(loop in 1:length(cmda_1Loop)){
  
  result <- list()
  
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
  bh_max0 <- 16
  bh_max1 <- 16 #20 #12 #20 is next #8 #24 #16
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
  cmda_1 <- cmda_1Loop[loop]#80 #90
  #cmda_2 <- 50 #scenario
  #homogen <- 0 #scenario
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
  for(i in 1:100){
    for(j in 1:101){
      homogen <- testfor2[((i-1)*101)+j,1]
      cmda_2 <- testfor2[((i-1)*101)+j,2]
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
      # MDAsuccessV1 <- GMSoutiR[,3]<(1/12)
      # MDAsuccessV2 <- GMSoutiR[,6]<(1/12)
      # successMDA <- cbind(MDAsuccessV1, MDAsuccessV2)
      
      #calculating one year incidence per 1000 immediately after the end of MDA
      MDAendsV1 <- which(timeVector==(2018+(tm_1+dm0)/12))
      MDAendsV2 <- which(timeVector==(2018+(tm_2+dm1)/12))
      DecOneYrIncV1 <- sum(GMSoutiR[MDAendsV1:(MDAendsV1+12),2])
      TotOneYrIncV1 <- sum(GMSoutiR[MDAendsV1:(MDAendsV1+12),3])
      DecOneYrIncV2 <- sum(GMSoutiR[MDAendsV2:(MDAendsV2+12),5])
      TotOneYrIncV2 <- sum(GMSoutiR[MDAendsV2:(MDAendsV2+12),6])
      
      OneYrInc <- cbind(DecOneYrIncV1, TotOneYrIncV1, DecOneYrIncV2, TotOneYrIncV2)
      
      result[[((i-1)*101)+j]] <- OneYrInc  #successMDA
    }
  }
  #time component #scenario
  #outside of 'for' loop
  #write.table(GMSoutiR[,1],'parameters/times.csv', col.names = 'time', row.names = FALSE)
  #saveRDS(result, paste('results_homo_cov/results_loop_',loop,"_",gsub("\\:","",Sys.time()),'.rds',sep=''))
  #saveRDS(result, paste('results_homo_cov_start0_seas/results_loop_',loop,"_",gsub("\\:","",Sys.time()),'.rds',sep='')) #default HBR_MAX: 16
  #saveRDS(result, paste('results_homo_cov_start0_seas_village2highAPI/results_loop_',loop,"_",gsub("\\:","",Sys.time()),'.rds',sep='')) #hbr_max2: 24, highAPI
  #saveRDS(result, paste('results_homo_cov_start0_seas_village2lowAPI/results_loop_',loop,"_",gsub("\\:","",Sys.time()),'.rds',sep='')) #hbr_max2: 8, lowAPI
  #with seasonality on [switch is inside modGMS.cpp]
  #saveRDS(result, paste('results_homo_cov_start0_seas/results_loop_',loop,"_",gsub("\\:","",Sys.time()),'.rds',sep=''))
  saveRDS(result, paste('results_TOT/results_loop_',loop,"_",gsub("\\:","",Sys.time()),'.rds',sep='')) #hbr_max2: 24, highAPI
  #scenario
  
  #Analysing the data list 'results_.rds'####
  #in different plotting file
  
}