library(deSolve)
library(shiny)
library(TSA)
library(Rcpp)
sourceCpp("modGMS.cpp")


ui <- fluidPage(
  tabsetPanel(
    id="panels",
    tabPanel(title = strong("Baseline"),
             column(3,
                    sliderInput(inputId="API", label = "baseline API", value = 2.5, min=1, max=100,step=0.5),
                    sliderInput(inputId="eta", label = "% of all infections that are caught outside the village (forest)", value = 30, min=0, max=100,step=10),
                    sliderInput(inputId="covEDAT0", label = "baseline % of all clinical cases treated", value = 25, min=0, max=100)
             ),
             column(3,
                    sliderInput(inputId="covITN0", label = "baseline coverage of ITN (%) ", value = 70, min=0, max=90,step=.5),
                    sliderInput(inputId="effITN", label = "% of infections averted due to ownership of ITN ", value = 30, min=0, max=50), 
                    sliderInput(inputId="covIRS0", label = "baseline coverage of IRS (%) ", value = 0, min=0, max=90,step=10),
                    sliderInput(inputId="effIRS", label = "% reduction in biting rate due to IRS ", value = 15, min=0, max=25,step=5)
             ),
             column(3,
                    sliderInput(inputId="muC", label = "imported clinical cases per 1000 population per year ", value = 1, min=0, max=10,step=1),
                    sliderInput(inputId="muA", label = "imported asymptomatic microscopically detectable carriers per 1000 population per year ", value = 1, min=0, max=100,step=1),
                    sliderInput(inputId="muU", label = "imported asymptomatic microscopically undetectable carriers per 1000 population per year ", value = 1, min=0, max=100,step=1)
             ),
             column(3,
                    sliderInput(inputId="percfail2018", label = "% of cases failing treatment in 2018 and before ", value = 5, min=0, max=100,step=5),
                    sliderInput(inputId="percfail2019", label = "% of cases failing treatment in 2019  ", value = 15, min=0, max=100,step=5),
                    sliderInput(inputId="percfail2020", label = "% of cases failing treatment in 2020 and after  ", value = 30, min=0, max=100,step=5)
             )
    ),
    tabPanel(title= strong("Transmissibility"),
             column(4,
                    wellPanel(
                      h3("Human Biting Rate"),
                      sliderInput(inputId="bh_max0", label = "number of mosquito bites per human per night (peak season) village 1", value = 16, min=0, max=80,step=1),
                      sliderInput(inputId="bh_max1", label = "number of mosquito bites per human per night (peak season) village 2", value = 16, min=0, max=80,step=1)
                    )),
             column(4,
                    wellPanel(
                      h3("Transmissibility compared to Clinical cases"),
                      sliderInput(inputId = "rhoa", label = "relative infectivity of asymptomatic microscopically detectable carriers compared with clinical infections (%)", value = 55, min = 17, max=80,step=1),
                      sliderInput(inputId = "rhou", label = "relative infectivity of asymptomatic microscopically undetectable carriers compared with clinical infections (%)", value = 17, min = 1, max=60,step=1)
                    ))),
    
    tabPanel(title = strong("Interventions currently available"),
             column(4,
                    wellPanel(
                      h3("Early Diagnosis and Treatment"),
                      checkboxInput(inputId="EDATon", label = "switch on scale up of EDAT ", value = TRUE),
                      checkboxInput(inputId="primon", label = "ACT+primaquine for EDAT and MDA ", value = FALSE), #under EDAT checkbox
                      sliderInput(inputId="EDATscale", label = "years to scale up EDAT ", value = 1, min=.25, max=3, step=.25),
                      sliderInput(inputId="covEDATi", label = "new % of all clinical cases treated", value = 70, min=0, max=100,step=5)
                    )), 
             column(4,wellPanel(
                      h3("Insecticide Treated Net (LLIN)"),
                      checkboxInput(inputId="ITNon", label = "switch on scale up of LLIN", value = TRUE),
                      sliderInput(inputId="ITNscale", label = "years to universal access to LLIN", value = 1, min=.25, max=3, step=.25),
                      sliderInput(inputId="covITNi", label = "new bed-net use of LLIN (%)", value = 90, min=0, max=90,step=5)
                    )),
             column(4,wellPanel(
               h3("Indoor Residual Spray"),
               checkboxInput(inputId="IRSon", label = "switch on scale up of IRS ", value = FALSE),
               sliderInput(inputId="IRSscale", label = "years to scale up IRS ", value = 1, min=.25, max=3, step=.25),
               sliderInput(inputId="covIRSi", label = "new coverage of IRS (%) ", value = 90, min=0, max=90,step=5)
             ))
  ),
  tabPanel(title = strong("Interventions under trial: Focal MVDA (hotspot)"),
                     column(3,
                            checkboxInput(inputId="MDAon", label = "switch on MDA", value = TRUE), #6
                            sliderInput(inputId="lossd", label = "days prophylaxis provided by the ACT", value = 30, min=15, max=30,step=1),
                            sliderInput(inputId="dm0", label = "months to complete MDA in village 1", value = 3, min=1, max=24,step=0.5),
                            sliderInput(inputId="dm1", label = "months to complete MDA in village 2", value = 3, min=1, max=24,step=0.5)
                            
                     ),
                     column(3,
                            sliderInput(inputId="cmda_1", label = "local population coverage of MDA in village 1", value = 90, min=0, max=99,step=1),
                            sliderInput(inputId="cmda_2", label = "local population coverage of MDA in village 2", value = 50, min=0, max=99,step=1),
                            sliderInput(inputId="homogen", label = "% homogeniety", value = 0, min=0, max=100,step=1)
                     ),
                     
                     column(3,
                            sliderInput(inputId="tm_1", label = "timing of 1st MDA round in 1st village [2018+ no. of month]", value = 9, min=1, max=36,step=1),
                            sliderInput(inputId="tm_2", label = "timing of 1st MDA round in 2nd village [2018+ no. of month]", value = 9, min=1, max=36,step=1),
                            sliderInput(inputId="p1v", label = "proportion of total in village 1 (remaining is in village 2)", value = .5, min=0.01, max=.99,step=.1)
                     ),
                     column(3,
                            radioButtons(inputId="VACon", label = "With vaccination: ", choices = c("No"=0, "Yes"=1), selected = 0, inline=TRUE),
                            sliderInput(inputId="effv_1", label = "% protective efficacy of RTS,S with 1st dose", value = 75, min=0, max=100),
                            sliderInput(inputId="effv_2", label = "% protective efficacy of RTS,S with 2nd dose", value = 80, min=0, max=100),
                            sliderInput(inputId="vh", label = "half-life of vaccine protection (days)", value = 90, min=10, max=500,step=10)
                     )
            ),
            tabPanel(title = strong("Interventions under trial: Focal MSAT (mobile)"),
                     column(3,
                            checkboxInput(inputId="MSATon", label = "switch on MSAT for imported cases", value = TRUE),
                            sliderInput(inputId="MSATscale", label = "years to scale up MSAT ", value = 1, min=.25, max=3, step=.25), 
                            sliderInput(inputId="covMSATi", label = "new coverage of MSAT (%)", value = 90, min=0, max=100,step=10)
                     ),
                     column(3,
                            sliderInput(inputId="MSATsensC", label = "sensitivity HS RDT (clinical) ", value = 99, min=0, max=100,step=5),
                            sliderInput(inputId="MSATsensA", label = "sensitivity HS RDT (micro detectable, asym)", value = 87, min=0, max=100,step=5),
                            sliderInput(inputId="MSATsensU", label = "sensitivity HS RDT (micro undetectable, asym)", value = 44, min=0, max=100,step=5)
                     )
            ),
            tabPanel(title= strong("Download"),
                     br(),
                     downloadButton("downloadTable", "Download current values of parameters"),
                     downloadButton("downloadplot","Download high resolution figure"))
  ),
  fluidRow(plotOutput(outputId = "MODEL"))
  
)

#non-reactive parameters
# define the number of weeks to run the model
dt<-1/12
startyear<-2007
stopyear<-2023
maxt<-stopyear-startyear
times <- seq(0, maxt, by = dt)
tsteps<-length(times)

ParLabel <- read.table('ParLabel.csv', sep=",", as.is=TRUE)

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
  #out <- ode(y = state, times = times, func = WmodGMSrcpp, parms = parameters)
  out <- ode(y = state, times = times, func = WmodGMSrcpp, parms = parameters, method="vode")
  
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


server <- function(input, output, session) {
  scenario_0<-c(EDATon = 0,
                ITNon = 0,
                IRSon = 0,
                MDAon = 0,
                primon = 0,
                MSATon = 0,
                VACon = 0)
  
  scenario_iR<-reactive(c(EDATon = input$EDATon,
                          ITNon = input$ITNon,
                          IRSon = input$IRSon,
                          MDAon = input$MDAon,
                          primon = input$primon,
                          MSATon = input$MSATon,
                          VACon = as.numeric(input$VACon)))
  
  parametersR <- reactive(c(
    bh_max0 = input$bh_max0,                 # bites per human per night
    bh_max1 = input$bh_max1,
    eta = input$eta,
    covEDAT0 = input$covEDAT0,
    covITN0 = input$covITN0,
    effITN = input$effITN,
    covIRS0 = input$covIRS0,
    effIRS = input$effIRS,
    muC = input$muC,
    muA = input$muA,
    muU = input$muU,
    percfail2018 = input$percfail2018,
    percfail2019 = input$percfail2019,
    percfail2020 = input$percfail2020,
    
    EDATscale = input$EDATscale,
    covEDATi = input$covEDATi,
    ITNscale = input$ITNscale,
    covITNi = input$covITNi,
    IRSscale = input$IRSscale,
    covIRSi = input$covIRSi,
    cmda_1 = input$cmda_1,
    cmda_2 = input$cmda_2,
    
    tm_1 = input$tm_1,          # timing of 1st round [2018 to 2021 - 1 month steps]
    tm_2 = input$tm_2,          # timing of 2nd round [2018+(1/12) to 2021 - 1 month steps]
    
    dm0 = input$dm0,
    dm1 = input$dm1,
    lossd = input$lossd,
    
    MSATscale = input$MSATscale,
    covMSATi = input$covMSATi,
    MSATsensC = input$MSATsensC,
    MSATsensA = input$MSATsensA,
    MSATsensU = input$MSATsensU,
    
    effv_1 = input$effv_1,
    effv_2 = input$effv_2,
    
    vh = input$vh,
    homogen = input$homogen,
    p1v = input$p1v,
    
    rhoa=input$rhoa,
    rhou=input$rhou
  ))
  
  # initial prevalence
  initprevR <- reactive(0.001*input$API)
  
  GMSout0R <- reactive(runGMS(initprevR(), scenario_0,parametersR()))
  
  GMSoutiR <- reactive(runGMS(initprevR(), scenario_iR(),parametersR()))
  
  plotR <- function()
  {
    GMSout0<-GMSout0R()
    
    GMSouti<-GMSoutiR()
    
    times<-GMSout0[,1]
    #0
    clinmonth_det0<-cbind(GMSout0[,2],GMSouti[,2])
    clinmonth_tot0<-cbind(GMSout0[,3],GMSouti[,3])
    prevalence0<-cbind(GMSout0[,4],GMSouti[,4])
    
    runin<-(2016-startyear)/dt
    
    finclin0<-max(clinmonth_tot0[(runin:length(clinmonth_det0[,1])),])
    finprev0<-max(prevalence0[(runin:length(prevalence0[,1])),])
    
    #1
    clinmonth_det1<-cbind(GMSout0[,5],GMSouti[,5])
    clinmonth_tot1<-cbind(GMSout0[,6],GMSouti[,6])
    prevalence1<-cbind(GMSout0[,7],GMSouti[,7])
    
    runin<-(2016-startyear)/dt
    
    finclin1<-max(clinmonth_tot1[(runin:length(clinmonth_det1[,1])),])
    finprev1<-max(prevalence1[(runin:length(prevalence1[,1])),])
    
    # PLOTTING
    par(mfrow=c(1,2), cex=1.5)
    
    #0
    maxy<-max(finclin0,finclin1,input$API/12)
    x<-times[(runin:length(clinmonth_det0[,1]))]
    y1<-clinmonth_det0[runin:length(clinmonth_det0[,1]),2]
    y2<-clinmonth_tot0[runin:length(clinmonth_tot0[,1]),2]
    
    plot(x,y1, type='l',lty=1,col=rgb(1,0,0,alpha=0.1),xlab = "Time",ylab="incidence per 1000 per month",main="Monthly cases per 1000 population",ylim=c(0,maxy),lwd=2)
    lines(x,y2, type='l',lty=1,col=rgb(1,0,0,alpha=0.1),lwd=2)
    
    polygon(c(x,rev(x)),c(y2,rev(y1)),col=rgb(1,0,0,alpha=0.1),border=NA)
    
    y1<-clinmonth_det1[runin:length(clinmonth_det1[,1]),2]
    y2<-clinmonth_tot1[runin:length(clinmonth_tot1[,1]),2]
    lines(x,y1, type='l',lty=1,col=rgb(0,0,1,alpha=0.4),lwd=2)
    lines(x,y2, type='l',lty=1,col=rgb(0,0,1,alpha=0.4),lwd=2)
    
    polygon(c(x,rev(x)),c(y2,rev(y1)),col=rgb(0,0,1,alpha=0.4),border=NA)
    
    lines(c(2018,2018),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
    
    abline(h=input$API/12,col="dark blue",lty=1,lwd=1)
    abline(h=1/12,col="red",lty=3,lwd=3)
    maxy<-max(finprev0,finprev1)
    plot(times[(runin:length(prevalence0[,1]))],prevalence0[(runin:length(prevalence0[,1])),2], type='l',lty=1,col=rgb(1,0,0,alpha=0.25),xlab = "Time",ylab="% prevalence",main="Predicted true prevalence",ylim=c(0,maxy),lwd=6)
    lines(times[(runin:length(prevalence1[,1]))],prevalence1[(runin:length(prevalence1[,1])),2], type='l',lty=1,col=rgb(0,0,1,alpha=0.6),xlab = "Time",ylab="% prevalence",main="Predicted true prevalence",ylim=c(0,maxy),lwd=6)
    lines(c(2018,2018),c(-maxy,2*maxy),col="dark grey",lty=3,lwd=2)
    
  }
  
  output$MODEL <- renderPlot({
    plotR()
  })
  
  # output$MODEL2 <- renderPlot({
  #   plotR2()
  # })
  
  output$downloadplot <- downloadHandler(
    filename = function(){paste('MalMod_',gsub("\\:","",Sys.time()),'.png',sep='')},
    content = function(file) {
      png(filename=file, height= 1600, width=4800, units= "px", res=300) #if(...=="png"){png(file)} else if(...=="pdf"){pdf(file)}
      plotR()
      dev.off()
    })
  
  tableContentR <- reactive({
    tmp <- c(scenario_iR(), input$API, parametersR())
    tmp2 <- cbind(ParLabel[,1], tmp, ParLabel[,2], names(tmp))
    colnames(tmp2) <- c("Name","Value","Unit","VarName")
    tmp2
  })
  
  output$downloadTable <- downloadHandler(
    filename = function(){paste('MalMod_',gsub("\\:","",Sys.time()),'.csv',sep='')},
    content = function(file) {
      write.csv(tableContentR(), file, row.names = FALSE)
    })
}

shinyApp(ui = ui, server = server)