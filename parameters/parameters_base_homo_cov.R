#remember to remove intervention parameters and 'in' from the texts
setwd("~/OneDrive/MORU/Projects/TCE_MDA effect/MDA_eff_hm/") #mac

source("functions/modified copy of shiny2ode.R")

z <- 'sliderInput(inputId="API", label = "baseline API", value = 2.5, min=1, max=100,step=0.5),
sliderInput(inputId="eta", label = "% of all infections that are caught outside the village (forest)", value = 30, min=0, max=100,step=10),
sliderInput(inputId="covEDAT0", label = "baseline % of all clinical cases treated", value = 25, min=0, max=100)

sliderInput(inputId="covITN0", label = "baseline coverage of ITN (%) ", value = 70, min=0, max=90,step=.5),
sliderInput(inputId="effITN", label = "% of infections averted due to ownership of ITN ", value = 30, min=0, max=50), 
sliderInput(inputId="covIRS0", label = "baseline coverage of IRS (%) ", value = 0, min=0, max=90,step=10),
sliderInput(inputId="effIRS", label = "% reduction biting rate due to IRS ", value = 15, min=0, max=25,step=5)

sliderInput(inputId="muC", label = "imported clinical cases per 1000 population per year ", value = 1, min=0, max=10,step=1),
sliderInput(inputId="muA", label = "imported asymptomatic microscopically detectable carriers per 1000 population per year ", value = 1, min=0, max=100,step=1),
sliderInput(inputId="muU", label = "imported asymptomatic microscopically undetectable carriers per 1000 population per year ", value = 1, min=0, max=100,step=1)

sliderInput(inputId="percfail2018", label = "% of cases failing treatment 2018 and before ", value = 5, min=0, max=100,step=5),
sliderInput(inputId="percfail2019", label = "% of cases failing treatment 2019  ", value = 15, min=0, max=100,step=5),
sliderInput(inputId="percfail2020", label = "% of cases failing treatment 2020 and after  ", value = 30, min=0, max=100,step=5)

sliderInput(inputId="bh_max0", label = "number of mosquito bites per human per night (peak season) village 1", value = 16, min=0, max=80,step=1),
sliderInput(inputId="bh_max1", label = "number of mosquito bites per human per night (peak season) village 2", value = 16, min=0, max=80,step=1)

sliderInput(inputId = "rhoa", label = "relative infectivity of asymptomatic microscopically detectable carriers compared with clinical infections (%)", value = 55, min = 17, max=80,step=1),
sliderInput(inputId = "rhou", label = "relative infectivity of asymptomatic microscopically undetectable carriers compared with clinical infections (%)", value = 17, min = 1, max=60,step=1)

sliderInput(inputId="EDATscale", label = "years to scale up EDAT ", value = 1, min=.25, max=3, step=.25),
sliderInput(inputId="covEDATi", label = "new % of all clinical cases treated", value = 70, min=0, max=100,step=5)

sliderInput(inputId="ITNscale", label = "years to universal access to LLIN", value = 1, min=.25, max=3, step=.25),
sliderInput(inputId="covITNi", label = "new bed-net use of LLIN (%)", value = 90, min=0, max=90,step=5)

sliderInput(inputId="IRSscale", label = "years to scale up IRS ", value = 1, min=.25, max=3, step=.25),
sliderInput(inputId="covIRSi", label = "new coverage of IRS (%) ", value = 90, min=0, max=90,step=5)


sliderInput(inputId="lossd", label = "days prophylaxis provided by the ACT", value = 30, min=15, max=30,step=1),
sliderInput(inputId="dm0", label = "months to complete MDA village 1", value = 3, min=1, max=24,step=0.5),
sliderInput(inputId="dm1", label = "months to complete MDA village 2", value = 3, min=1, max=24,step=0.5)

sliderInput(inputId="cmda_1", label = "local population coverage of MDA village 1", value = 90, min=0, max=99,step=1),
sliderInput(inputId="cmda_2", label = "local population coverage of MDA village 2", value = 50, min=0, max=99,step=1),
sliderInput(inputId="homogen", label = "% homogeniety", value = 0, min=0, max=100,step=1)

sliderInput(inputId="tm_1", label = "timing of 1st MDA round 1st village [2018+ no. of month]", value = 9, min=1, max=36,step=1),
sliderInput(inputId="tm_2", label = "timing of 1st MDA round 2nd village [2018+ no. of month]", value = 9, min=1, max=36,step=1),
sliderInput(inputId="p1v", label = "proportion of total village 1 (remaining is village 2)", value = .5, min=0.01, max=.99,step=.1)


sliderInput(inputId="effv_1", label = "% protective efficacy of RTS,S with 1st dose", value = 75, min=0, max=100),
sliderInput(inputId="effv_2", label = "% protective efficacy of RTS,S with 2nd dose", value = 80, min=0, max=100),
sliderInput(inputId="vh", label = "half-life of vaccine protection (days)", value = 90, min=10, max=500,step=10)

sliderInput(inputId="MSATscale", label = "years to scale up MSAT ", value = 1, min=.25, max=3, step=.25), 
sliderInput(inputId="covMSATi", label = "new coverage of MSAT (%)", value = 90, min=0, max=100,step=10)

sliderInput(inputId="MSATsensC", label = "sensitivity HS RDT (clinical) ", value = 99, min=0, max=100,step=5),
sliderInput(inputId="MSATsensA", label = "sensitivity HS RDT (micro detectable, asym)", value = 87, min=0, max=100,step=5),
sliderInput(inputId="MSATsensU", label = "sensitivity HS RDT (micro undetectable, asym)", value = 44, min=0, max=100,step=5)'

valueTable <- shiny2ode(z)

valueRange <- matrix(as.numeric(valueTable[,c(2,3,4)]),nrow(valueTable),3)


#transform the valueTable into 
#1. ODE code
cat(paste(valueTable[,1],"<-",valueTable[,2]), sep='\n')


#2. to use in for loop
cat(paste(valueTable[,1]," <- simValueTable[i,",1:nrow(valueTable),"]", sep=""), sep='\n')

