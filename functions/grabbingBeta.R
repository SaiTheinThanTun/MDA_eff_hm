library(deSolve)
library(shiny)
library(TSA)
library(Rcpp)
sourceCpp("/Users/Sai/OneDrive/MORU/Projects/TCE_MDA effect/v9_vaccineEffect_2villages_legacy/v8_booster_original_mod copy/modGMS.cpp")


scenario <-c(EDATon = 0,
              ITNon = 0,
              IRSon = 0,
              MDAon = 0,
              primon = 0,
              MSATon = 0,
              VACon = 0)

param = c(
  API = 10,
  bh_max = 20,
  eta = 30,
  covEDAT0 = 25,
  covITN0 = 70,
  effITN = 30,
  covIRS0 = 0,
  effIRS = 15,
  muC = 1,
  muA = 1,
  muU = 1,
  percfail2018 = 5,
  percfail2019 = 15,
  percfail2020 = 30,
  EDATscale = 1,
  covEDATi = 70,
  ITNscale = 1,
  covITNi = 90,
  IRSscale = 1,
  covIRSi = 90,
  lossd = 30,
  dm = 6,
  cmda_1 = 50,
  cmda_2 = 50,
  cmda_3 = 50,
  tm_1 = 9,
  tm_2 = 10,
  tm_3 = 11,
  effv_1 = 75,
  effv_2 = 80,
  effv_3 = 92,
  vh = 90,
  MSATscale = 1,
  covMSATi = 90,
  MSATsensC = 99,
  MSATsensA = 87,
  MSATsensU = 4
)


parameters <- c(scenario,
                timei = 2018,
                nuTr = 14,                   # days of infectiosness after treatment ACT [N]
                nuTrp = 7,                   # days of infectiosness after treatment ACT+primaquine [N]
                alpha = 0.7,                   # relative amplitude seasonality [N]
                phi = 0.0,                   # phase angle seasonality [N]
                epsilonh=0.23,                 # per bite probability of an infectious mosquito infecting a human
                epsilonm=0.5,                  # per bite probability of an infectious human infecting a mosquito
                b=365/3,                       # per mosquito rate of biting
                deltam=365/14,                 #
                gammam=365/10,#Rate of becoming infectious from the latent phase for mosquitos, Kai Matuschewski: Getting infectious
                cm_1=99,
                cm_2=99,
                cm_3=99,
                covMSAT0=0,
                omega = 2,                   # average duration of immunity (years) [N]
                nuC = 3,                     # days of symptoms in the absence of treatment [N], #change 9 -> 3
                nuA = 60,                    # days of asymptomatic microscopically detectable carriage [N]
                nuU = 100,                    # days of asymptomatic microscopically undetectable carriage [N], #change 60 -> 100, Mean duration of a malaria untreated infection: 160 days, 
                rhoa = 55,                   # relative infectivity of asymptomatic microscopically detectable carriers compared with clinical infections (%) [N]
                rhou = 17,                   # relative infectivity of asymptomatic microscopically undetectable carriers compared with clinical infections (%) [N]
                ps = 90,                     # % of all non-immune new infections that are clinical [N]
                pr = 20,                     # % of all immune new infections that are clinical [N]
                mu = 69,                      # life expectancy (years) [N]
                param)

initP<-10000 
initprev <- 0.001*10 #input$API)
initS_0<-0.5*(1-initprev)*initP
initIC_0<-0
initIA_0<-initprev*initP
initIU_0<-0
initR_0<-0.5*(1-initprev)*initP
initTr_0<-0

state <- c(Y = 0, Cinc_det = 0, Cinc_tot = 0, 
           S_0 = initS_0, IC_0 = initIC_0, IA_0 = initIA_0, IU_0 = initIU_0, R_0 = initR_0, Tr_0 = initTr_0, Sm_0 = 0, Rm_0 = 0,
           S_1 = 0, IC_1 = 0, IA_1 = 0, IU_1 = 0, R_1 = 0, Tr_1 = 0, Sm_1 = 0, Rm_1 = 0,
           S_2 = 0, IC_2 = 0, IA_2 = 0, IU_2 = 0, R_2 = 0, Tr_2 = 0, Sm_2 = 0, Rm_2 = 0,
           S_3 = 0, IC_3 = 0, IA_3 = 0, IU_3 = 0, R_3 = 0, Tr_3 = 0, Sm_3 = 0, Rm_3 = 0,
           S_4 = 0, IC_4 = 0, IA_4 = 0, IU_4 = 0, R_4 = 0, Tr_4 = 0, Sm_4 = 0, Rm_4 = 0, 
           beta = 0
)

dt<-1/12
startyear<-2007
stopyear<-2023
maxt<-stopyear-startyear
times <- seq(0, maxt, by = dt)

WmodGMSrcpp<-function(t,state,parameters){
  tmp<-modGMSrcpp(t,state,parameters)
  return(list(tmp))
}


out <- ode(y = state, times = times, func = WmodGMSrcpp, parms = parameters, method="vode")

plot(out[,ncol(out)])

#beta calculations
seas <- 1+parameters["alpha"]*cos(2*3.14159*(times-parameters["phi"]))
plot(seas, type='l')

b <- parameters["b"]
epsilonh <- parameters["epsilonh"]
epsilonm <- parameters["epsilonm"]
bh <- parameters["bh_max"]/(1+parameters["alpha"])
gammam <- parameters["gammam"]
deltam <- parameters["deltam"]

beta <- seas*b*epsilonh*epsilonm*bh/((bh*epsilonh+deltam)*(gammam/(gammam+deltam)))

plot(beta, type='l')


#lambda####
#lam = (1-(1-eta)*effIRS*covIRS)*(1-effITN*covITN)*beta
eta <- parameters["eta"]
effIRS <- parameters["effIRS"]/100
covIRS <- parameters["covIRS0"]/100
effITN <- parameters["effITN"]/100
covITN <- parameters["covITN0"]/100
lam <- (1-(1-eta)*effIRS*covIRS)*(1-effITN*covITN)*beta

plot(lam, type = "l")


plot(lam*(5/100), type = "l")
