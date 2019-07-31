#see #scenario tag for things to change in each scenario
#non-reactive function runGMS is now outside of the server function
runGMS<-function(initprev, scenario, param) 
{
  #MODEL PARAMETERS
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
                  # cm_1=80,
                  # cm_2=95,
                  # cm_3=95,
                  covMSAT0=0,
                  omega = 2,                   # average duration of immunity (years) [N]
                  nuC = 3,                     # days of symptoms in the absence of treatment [N], #change 9 -> 3
                  nuA = 60,                    # days of asymptomatic microscopically detectable carriage [N]
                  nuU = 100,                    # days of asymptomatic microscopically undetectable carriage [N], #change 60 -> 100, Mean duration of a malaria untreated infection: 160 days, 
                  ps = 90,                     # % of all non-immune new infections that are clinical [N]
                  pr = 20,                     # % of all immune new infections that are clinical [N]
                  mu = 50,                      # life expectancy (years) [N]
                  param)
  
  
  
  # MODEL INITIAL CONDITIONS
  # population size
  initP<-10000 
  
  initS_0<-0.5*(1-initprev)*initP
  initIC_0<-0
  initIA_0<-initprev*initP
  initIU_0<-0
  initR_0<-0.5*(1-initprev)*initP
  initTr_0<-0
  
  state <- c(Y = 0, Cinc_det0 = 0, Cinc_tot0 = 0, 
             S_0 = as.vector(param["p1v"])*initS_0, IC_0 = initIC_0, IA_0 = as.vector(param["p1v"])*initIA_0, IU_0 = initIU_0, R_0 = as.vector(param["p1v"])*initR_0, Tr_0 = initTr_0, Sm_0 = 0, Rm_0 = 0,
             S_1 = (1-as.vector(param["p1v"]))*initS_0, IC_1 = initIC_0, IA_1 = (1-as.vector(param["p1v"]))*initIA_0, IU_1 = initIU_0, R_1 = (1-as.vector(param["p1v"]))*initR_0, Tr_1 = initTr_0, Sm_1 = 0, Rm_1 = 0,
             Cinc_det1 = 0, Cinc_tot1 = 0
  )
  
  # state <- c(Y = 0, Cinc_det0 = 0, Cinc_tot0 = 0, 
  #            S_0 = initS_0, IC_0 = initIC_0, IA_0 = initIA_0, IU_0 = initIU_0, R_0 = initR_0, Tr_0 = initTr_0, Sm_0 = 0, Rm_0 = 0,
  #            S_1 = initS_0, IC_1 = initIC_0, IA_1 = initIA_0, IU_1 = initIU_0, R_1 = initR_0, Tr_1 = initTr_0, Sm_1 = 0, Rm_1 = 0,
  #            Cinc_det1 = 0, Cinc_tot1 = 0
  # )
  #S_1 = 0, IC_1 = initIC_0, IA_1 = initIA_0, IU_1 = initIU_0, R_1 = 0, Tr_1 = 0, Sm_1 = 0, Rm_1 = 0,
  
  
  #out <- ode(y = state, times = times, func = modGMS, parms = parameters)
  WmodGMSrcpp<-function(t,state,parameters){
    tmp<-modGMSrcpp(t,state,parameters)
    return(list(tmp))
  }
  out <- ode(y = state, times = times, func = WmodGMSrcpp, parms = parameters)
  #out <- ode(y = state, times = times, func = WmodGMSrcpp, parms = parameters, method="vode")
  
  # MODEL OUTPUTS
  ipop1 <- 5:12
  iinc_det1 <- 3
  iinc_tot1 <- 4
  iprev1 <- c(6,  7,  8, 10)
  
  ipop2 <- 13:20
  iinc_det2 <- 21 #in Rcpp
  iinc_tot2 <- 22 #in Rcpp
  iprev2 <- c(14, 15, 16, 18)
  
  # population
  times<-out[,1]+startyear
  
  pop1<-rowSums(out[,ipop1])
  pop2<-rowSums(out[,ipop2])
  
  
  # clinical incidence detected per 1000 per month
  tci_det1 <- out[,iinc_det1]
  clinmonth_det1 <- tci_det1
  clinmonth_det1[1] <- 0
  clinmonth_det1[2:length(times)] <- 1000*(tci_det1[2:length(times)] - tci_det1[1:(length(times)-1)])/pop1[2:length(times)]
  
  # clinical incidence total per 1000 per month
  tci_tot1 <- out[,iinc_tot1]
  clinmonth_tot1 <- tci_tot1
  clinmonth_tot1[1] <- 0
  clinmonth_tot1[2:length(times)] <- 1000*(tci_tot1[2:length(times)] - tci_tot1[1:(length(times)-1)])/pop1[2:length(times)]
  
  
  # % prevalence
  prevalence1 <- 100*rowSums(out[,iprev1])/pop1 # Additional file: Equation no.13
  
  # clinical incidence detected per 1000 per month
  tci_det2 <- out[,iinc_det2]
  clinmonth_det2 <- tci_det2
  clinmonth_det2[1] <- 0
  clinmonth_det2[2:length(times)] <- 1000*(tci_det2[2:length(times)] - tci_det2[1:(length(times)-1)])/pop2[2:length(times)]
  
  # clinical incidence total per 1000 per month
  tci_tot2 <- out[,iinc_tot2]
  clinmonth_tot2 <- tci_tot2
  clinmonth_tot2[1] <- 0
  clinmonth_tot2[2:length(times)] <- 1000*(tci_tot2[2:length(times)] - tci_tot2[1:(length(times)-1)])/pop2[2:length(times)]
  
  
  # % prevalence
  prevalence2 <- 100*rowSums(out[,iprev2])/pop2 # Additional file: Equation no.13
  
  
  GMSout<-matrix(NA,nrow=length(times),ncol=7)
  GMSout[,1]<-times
  GMSout[,2]<-clinmonth_det1
  GMSout[,3]<-clinmonth_tot1
  GMSout[,4]<-prevalence1
  GMSout[,5]<-clinmonth_det2
  GMSout[,6]<-clinmonth_tot2
  GMSout[,7]<-prevalence2
  
  return(GMSout)
}




################################################################
#below is just testing the app to see if it's still working#####
################################################################

# plotR <- function()
# {
#   GMSout0<-GMSout0R
#   
#   GMSouti<-GMSoutiR
#   
#   times<-GMSout0[,1]
#   #0
#   clinmonth_det0<-cbind(GMSout0[,2],GMSouti[,2])
#   clinmonth_tot0<-cbind(GMSout0[,3],GMSouti[,3])
#   prevalence0<-cbind(GMSout0[,4],GMSouti[,4])
#   
#   runin<-(2016-startyear)/dt
#   
#   finclin0<-max(clinmonth_tot0[(runin:length(clinmonth_det0[,1])),])
#   finprev0<-max(prevalence0[(runin:length(prevalence0[,1])),])
#   
#   #1
#   clinmonth_det1<-cbind(GMSout0[,5],GMSouti[,5])
#   clinmonth_tot1<-cbind(GMSout0[,6],GMSouti[,6])
#   prevalence1<-cbind(GMSout0[,7],GMSouti[,7])
#   
#   runin<-(2016-startyear)/dt
#   
#   finclin1<-max(clinmonth_tot1[(runin:length(clinmonth_det1[,1])),])
#   finprev1<-max(prevalence1[(runin:length(prevalence1[,1])),])
#   
#   # PLOTTING
#   par(mfrow=c(1,2), cex=1.5)
#   
#   #0
#   maxy<-max(finclin0,finclin1,API/12)
#   x<-times[(runin:length(clinmonth_det0[,1]))]
#   y1<-clinmonth_det0[runin:length(clinmonth_det0[,1]),2]
#   y2<-clinmonth_tot0[runin:length(clinmonth_tot0[,1]),2]
#   
#   plot(x,y1, type='l',lty=1,col=rgb(1,0,0,alpha=0.1),xlab = "Time",ylab="incidence per 1000 per month",main="Monthly cases per 1000 population",ylim=c(0,maxy),lwd=2)
#   lines(x,y2, type='l',lty=1,col=rgb(1,0,0,alpha=0.1),lwd=2)
#   
#   polygon(c(x,rev(x)),c(y2,rev(y1)),col=rgb(1,0,0,alpha=0.1),border=NA)
#   
#   y1<-clinmonth_det1[runin:length(clinmonth_det1[,1]),2]
#   y2<-clinmonth_tot1[runin:length(clinmonth_tot1[,1]),2]
#   lines(x,y1, type='l',lty=1,col=rgb(0,0,1,alpha=0.4),lwd=2)
#   lines(x,y2, type='l',lty=1,col=rgb(0,0,1,alpha=0.4),lwd=2)
#   
#   polygon(c(x,rev(x)),c(y2,rev(y1)),col=rgb(0,0,1,alpha=0.4),border=NA)
#   
#   lines(c(2018,2018),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
#   
#   abline(h=API/12,col="dark blue",lty=1,lwd=1)
#   abline(h=1/12,col="red",lty=3,lwd=3)
#   maxy<-max(finprev0,finprev1)
#   plot(times[(runin:length(prevalence0[,1]))],prevalence0[(runin:length(prevalence0[,1])),2], type='l',lty=1,col=rgb(1,0,0,alpha=0.25),xlab = "Time",ylab="% prevalence",main="Predicted true prevalence",ylim=c(0,maxy),lwd=6)
#   lines(times[(runin:length(prevalence1[,1]))],prevalence1[(runin:length(prevalence1[,1])),2], type='l',lty=1,col=rgb(0,0,1,alpha=0.6),xlab = "Time",ylab="% prevalence",main="Predicted true prevalence",ylim=c(0,maxy),lwd=6)
#   lines(c(2018,2018),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
#   
# }

#scenario
# png(filename=paste('results_homo_cov/plot_',gsub("\\:","",Sys.time()),'.png',sep=''), height= 1600, width=4800, units= "px", res=300) #if(...=="png"){png(file)} else if(...=="pdf"){pdf(file)}
# plotR()
# dev.off()

