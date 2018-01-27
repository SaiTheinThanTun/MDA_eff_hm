#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
List modGMSrcpp(double t, NumericVector state, NumericVector parameters) 
{
  // switches for interventions
  double EDATon = parameters["EDATon"];
  double ITNon = parameters["ITNon"];
  double IRSon = parameters["IRSon"];
  double MDAon = parameters["MDAon"];
  double primon = parameters["primon"];
  double MSATon = parameters["MSATon"];
  double VACon = parameters["VACon"];
  
  // convert %s to proportions
  double covEDATi=parameters["covEDATi"];
    covEDATi = 0.9*covEDATi/100;
  double covEDAT0=parameters["covEDAT0"];
    covEDAT0 = 0.9*covEDAT0/100;
  double covITNi=parameters["covITNi"];
    covITNi = covITNi/100;
  double covITN0=parameters["covITN0"];
    covITN0 = covITN0/100;
  double effITN = parameters["effITN"];
    effITN =effITN/100;
  double covIRSi=parameters["covIRSi"];
    covIRSi=covIRSi/100;
  double covIRS0=parameters["covIRS0"];
    covIRS0=covIRS0/100;
  double effIRS = parameters["effIRS"];
    effIRS=effIRS/100;
  double covMSATi=parameters["covMSATi"];
    covMSATi=covMSATi/100;
  double covMSAT0=parameters["covMSAT0"];
    covMSAT0=covMSAT0/100;
  double MSATsensC=parameters["MSATsensC"];
    MSATsensC=MSATsensC/100;
  double MSATsensA=parameters["MSATsensA"];
    MSATsensA=MSATsensA/100;
  double MSATsensU=parameters["MSATsensU"];
    MSATsensU=MSATsensU/100;
  
  
  double cmda_1=parameters["cmda_1"];
    cmda_1=cmda_1/100;
  double cmda_2=parameters["cmda_2"];
    cmda_2=cmda_2/100;
  
  double effv_1=parameters["effv_1"];
    effv_1=effv_1/100;
  double effv_2=parameters["effv_2"];
    effv_2=effv_2/100;
  double rhoa=parameters["rhoa"];
    rhoa=rhoa/100;
  double rhou=parameters["rhou"];
    rhou=rhou/100;
  double ps=parameters["ps"];
    ps=ps/100;
  double pr=parameters["pr"];
    pr=pr/100;
  double eta=parameters["eta"];
    eta=eta/100;
  
  // convert time scales
  double dm0=parameters["dm0"];
    dm0=dm0/12;
  double dm1=parameters["dm1"];
    dm1=dm1/12;
  double tm_1 = parameters["tm_1"];
    tm_1=2018+(tm_1/12);
  double tm_2 = parameters["tm_2"];
    tm_2=2018+(tm_2/12);
  
  
  // convert durations to rates
  double lossd=parameters["lossd"];
    lossd=365/lossd;
  double omega=parameters["omega"];
    omega=1/omega;
  double nuC=parameters["nuC"];
    nuC=365/nuC;
  double nuA=parameters["nuA"];
    nuA=365/nuA;
  double nuU=parameters["nuU"];
    nuU=365/nuU;
  double mu=parameters["mu"];
    mu=1/mu;
  double nTr=parameters["nuTr"];
    nTr=365/nTr;
  double nTrp=parameters["nuTrp"];
    nTrp=365/nTrp;
  double muC=parameters["muC"];
    muC=muC/1000;
  double muA=parameters["muA"];
    muA=muA/1000;
  double muU=parameters["muU"];
    muU=muU/1000;
  
  //remaining variables
  double vh = parameters["vh"];
    vh=vh/365;
  double timei = parameters["timei"];
  double alpha = parameters["alpha"];
  double phi =  parameters["phi"];
  double epsilonh = parameters["epsilonh"];
  double epsilonm = parameters["epsilonm"];
  double b = parameters["b"];
  double deltam=parameters["deltam"];                 
  double gammam=parameters["gammam"];
  double percfail2018 = parameters["percfail2018"];
  double percfail2019 = parameters["percfail2019"];
  double percfail2020 = parameters["percfail2020"];
  double EDATscale =parameters["EDATscale"];
  double ITNscale=parameters["ITNscale"] ;
  double IRSscale =parameters["IRSscale"];
  double MSATscale=parameters["MSATscale"];
  double bh_max0 =parameters["bh_max0"];
  double bh_max1 =parameters["bh_max1"];
  //double commute0 = parameters["commute0"];
    //commute0 = commute0/100;
  //double commute1 = parameters["commute1"];
  //commute1 = commute1/100;
  double homogen = parameters["homogen"];
    homogen = homogen/100;
  double startyear=2007;
  
  // states
  double Y = state["Y"];
  double S_0 = state["S_0"];
  double IC_0 = state["IC_0"];
  double IA_0 = state["IA_0"];
  double IU_0 = state["IU_0"];
  double R_0 = state["R_0"];
  double Tr_0 = state["Tr_0"];
  double Sm_0 = state["Sm_0"];
  double Rm_0 = state["Rm_0"];
  double S_1 = state["S_1"];
  double IC_1 = state["IC_1"];
  double IA_1 = state["IA_1"];
  double IU_1 = state["IU_1"];
  double R_1 = state["R_1"];
  double Tr_1 = state["Tr_1"];
  double Sm_1 = state["Sm_1"];
  double Rm_1 = state["Rm_1"];
  
  // swtich on double interventions
  covEDATi = EDATon*covEDATi+(1-EDATon)*covEDAT0;
  covITNi = ITNon*covITNi+(1-ITNon)*covITN0;
  covIRSi = IRSon*covIRSi+(1-IRSon)*covIRS0;
  
  double sS = S_0+S_1;
  double sR = R_0+R_1;
  double sIC = IC_0+IC_1;
  double sIA = IA_0+IA_1;
  double sIU = IU_0+IU_1;
  double sTr = Tr_0+Tr_1;
  double sSm = Sm_0+Sm_1;
  double sRm = Rm_0+Rm_1;
  
  // define variables
  //double P = (sS+sR+sIC+sIA+sIU+sTr+sSm+sRm);
  double P0 = (S_0+R_0+IC_0+IA_0+IU_0+Tr_0+Sm_0+Rm_0);
  double P1 = (S_1+R_1+IC_1+IA_1+IU_1+Tr_1+Sm_1+Rm_1);
  
  double seas=1+alpha*cos(2*3.14159*(Y-phi));
  double bh0=bh_max0/(1+alpha);
  double bh1=bh_max1/(1+alpha);
  
  // Additional file: Equation no.10
  double beta0=b*epsilonh*epsilonm*bh0/((bh0*epsilonh+deltam)*(gammam/(gammam+deltam)));
  double beta1=b*epsilonh*epsilonm*bh1/((bh1*epsilonh+deltam)*(gammam/(gammam+deltam)));
  //double beta0=seas*b*epsilonh*epsilonm*bh0/((bh0*epsilonh+deltam)*(gammam/(gammam+deltam)));
  //double beta1=seas*b*epsilonh*epsilonm*bh1/((bh1*epsilonh+deltam)*(gammam/(gammam+deltam)));
  
  double mu_out = mu+muC+muA+muU;
  
  timei=timei-startyear;
  
  // Additional file: Equation no.14
  double wsiEDAT=(1-(Y<=timei))*(Y<=(timei+EDATscale))*((Y-timei)/EDATscale)+1*(Y>(timei+EDATscale));
  double wsiITN=(1-(Y<=timei))*(Y<=(timei+ITNscale))*((Y-timei)/ITNscale)+1*(Y>(timei+ITNscale));
  double wsiIRS=(1-(Y<=timei))*(Y<=(timei+IRSscale))*((Y-timei)/IRSscale)+1*(Y>(timei+IRSscale));
  double wsiMSAT=(1-(Y<=timei))*(Y<=(timei+MSATscale))*((Y-timei)/MSATscale)+1*(Y>(timei+MSATscale));
  double covEDAT=(1-wsiEDAT)*covEDAT0+wsiEDAT*covEDATi;
  double covITN=(1-wsiITN)*covITN0+wsiITN*covITNi;
  double covIRS=(1-wsiIRS)*covIRS0+wsiIRS*covIRSi;
  double covMSAT=(1-wsiMSAT)*covMSAT0+wsiMSAT*covMSATi;
  
  double nuTr= primon*((Y<timei)*nTr+(Y>timei)*nTrp)+(1-primon)*nTr;
  lossd=1/((1/lossd)-(1/nuTr));
  
  // Additional file: Equation no.9
  //double lam = (1-(1-eta)*effIRS*covIRS)*(1-effITN*covITN)*beta*(sIC+sTr+rhoa*sIA+rhou*sIU)/P;
  //double lam0 = (1-(1-eta)*effIRS*covIRS)*(1-effITN*covITN)*beta*(((IC_0+Tr_0+rhoa*IA_0+rhou*IU_0))+(commute0*(IC_1+Tr_1+rhoa*IA_1+rhou*IU_1)))/(P0+commute0*P1);
  //double lam1 = (1-(1-eta)*effIRS*covIRS)*(1-effITN*covITN)*beta*((commute1*(IC_0+Tr_0+rhoa*IA_0+rhou*IU_0))+((IC_1+Tr_1+rhoa*IA_1+rhou*IU_1)))/(P1+commute1*P0);
  double lam0 = (1-(1-eta)*effIRS*covIRS)*(1-effITN*covITN)*beta0*(((IC_0+Tr_0+rhoa*IA_0+rhou*IU_0))+(homogen*(IC_1+Tr_1+rhoa*IA_1+rhou*IU_1)))/(P0+homogen*P1);
  double lam1 = (1-(1-eta)*effIRS*covIRS)*(1-effITN*covITN)*beta1*((homogen*(IC_0+Tr_0+rhoa*IA_0+rhou*IU_0))+((IC_1+Tr_1+rhoa*IA_1+rhou*IU_1)))/(P1+homogen*P0);
  
  // vaccine effects
  // Additional file: Equation no.15
  double v_0 = VACon*((Y>((tm_1+dm0+(2/52))-startyear))*effv_1*exp(-log(2)*std::pow((std::max((Y+startyear-(tm_1+dm0+(2/52))),0.0)/vh),0.8)));
  double v_1 = VACon*((Y>((tm_2+dm1+(2/52))-startyear))*effv_2*exp(-log(2)*std::pow((std::max((Y+startyear-(tm_2+dm1+(2/52))),0.0)/vh),0.8)));
    
  // Additional file: Equation no.16
  double lam_0 = (1-(cmda_1*v_0))*lam0;
  //double lam_0 = (1-(cmda_1*v_0))*lam;
  //double lam_0 = lam0;
  double lam_1 = (1-(cmda_2*v_1))*lam1;
  //double lam_1 = lam1;
  
  double tau = covEDAT;
  
  //double fail = ((Y+startyear)<2019)*(percfail2018/100)+((Y+startyear)>=2019)*((Y+startyear)<2020)*(percfail2019/100)+((Y+startyear)>=2020)*(percfail2020/100);
  double fail = ((Y+startyear)<2018)*(percfail2018/100)+((Y+startyear)>=2018)*((Y+startyear)<2019)*(((percfail2019-percfail2018)/100)*(Y+startyear-2019)+(percfail2018/100))+((Y+startyear)>=2019)*((Y+startyear)<2020)*(((percfail2020-percfail2019)/100)*(Y+startyear-2020)+(percfail2019/100))+((Y+startyear)>=2020)*(percfail2020/100);

  // MDA and RTS,S rounds
  // Additional file: Equation no.15
  double m_0= MDAon*(Y>(tm_1-startyear))*(Y<=(tm_1+dm0-startyear))*(-log((1-cmda_1))/dm0);
  double m_1= MDAon*(Y>(tm_2-startyear))*(Y<=(tm_2+dm1-startyear))*(-log((1-cmda_2))/dm1);
  
  
  // Additional file: Equation no.18 
  muC = (1-MSATon*MSATsensC*covMSAT)*muC;
  muA = (1-MSATon*MSATsensA*covMSAT)*muA;
  muU = (1-MSATon*MSATsensU*covMSAT)*muU;
  
  
  // rate of change
  // Additional file: Equation no. 1 - 8
  double dY = 1;
  
  double dCinc_det0 = ps*tau*lam_0*S_0+pr*tau*lam_0*R_0+pr*tau*lam_0*IU_0+pr*tau*lam_0*IA_0;                           //3 // Additional file: Equation no.12
  double dCinc_tot0 = ps*lam_0*S_0+pr*lam_0*R_0+pr*lam_0*IU_0+pr*lam_0*IA_0;                                                                                                 //4 // Additional file: Equation no.11
  double dS_0 = mu*P0-mu_out*S_0+omega*R_0-lam_0*S_0+lossd*Sm_0-m_0*S_0;                                                                                         //5
  double dIC_0 = muC*P0-mu_out*IC_0+ps*(1-tau)*lam_0*S_0+pr*(1-tau)*lam_0*R_0+pr*(1-tau)*lam_0*IU_0+pr*(1-tau)*lam_0*IA_0-nuC*IC_0-m_0*IC_0;        //6 
  double dIA_0 = muA*P0-mu_out*IA_0+(1-ps)*lam_0*S_0+(1-pr)*lam_0*R_0+(1-pr)*lam_0*IU_0-pr*lam_0*IA_0+nuC*IC_0-nuA*IA_0+fail*nuTr*Tr_0-m_0*IA_0;    //7
  double dIU_0 = muU*P0-mu_out*IU_0-lam_0*IU_0-nuU*IU_0+nuA*IA_0-m_0*IU_0;                                                                    //8
  double dR_0 = -mu_out*R_0-omega*R_0-lam_0*R_0+nuU*IU_0+lossd*Rm_0-m_0*R_0;                                                                                      //9
  double dTr_0 = -mu_out*Tr_0+ps*tau*lam_0*S_0+pr*tau*lam_0*R_0+pr*tau*lam_0*IU_0+pr*tau*lam_0*IA_0-nuTr*Tr_0+m_0*(IC_0+IA_0+IU_0); //10
  double dSm_0 = -mu_out*Sm_0+omega*Rm_0-lossd*Sm_0+m_0*S_0;                                                                                                   //11
  double dRm_0 = -mu_out*Rm_0-omega*Rm_0+(1-fail)*nuTr*Tr_0-lossd*Rm_0+m_0*R_0;                                                                                //12
  
  
  double dS_1 = mu*P1-mu_out*S_1+omega*R_1-lam_1*S_1+lossd*Sm_1-m_1*S_1;                                                                          //13
  double dIC_1 = muC*P1-mu_out*IC_1+ps*(1-tau)*lam_1*S_1+pr*(1-tau)*lam_1*R_1+pr*(1-tau)*lam_1*IU_1+pr*(1-tau)*lam_1*IA_1-nuC*IC_1-m_1*IC_1;      //14
  double dIA_1 = muA*P1-mu_out*IA_1+(1-ps)*lam_1*S_1+(1-pr)*lam_1*R_1+(1-pr)*lam_1*IU_1-pr*lam_1*IA_1+nuC*IC_1-nuA*IA_1+fail*nuTr*Tr_1-m_1*IA_1;  //15
  double dIU_1 = muU*P1-mu_out*IU_1-lam_1*IU_1-nuU*IU_1+nuA*IA_1-m_1*IU_1;                                                                        //16
  double dR_1 = -mu_out*R_1-omega*R_1-lam_1*R_1+nuU*IU_1 +lossd*Rm_1-m_1*R_1;                                                                //17
  double dTr_1 = -mu_out*Tr_1+ps*tau*lam_1*S_1+pr*tau*lam_1*R_1+pr*tau*lam_1*IU_1+pr*tau*lam_1*IA_1-nuTr*Tr_1+m_1*(IC_1+IA_1+IU_1);      //18
  double dSm_1 = -mu_out*Sm_1+omega*Rm_1-lossd*Sm_1+m_1*S_1;                                                                             //19
  double dRm_1 = -mu_out*Rm_1-omega*Rm_1+(1-fail)*nuTr*Tr_1-lossd*Rm_1+m_1*R_1;                                                          //20
  double dCinc_det1 = ps*tau*lam_1*S_1+pr*tau*lam_1*R_1+pr*tau*lam_1*IU_1+pr*tau*lam_1*IA_1;                           //3 // Additional file: Equation no.12
  double dCinc_tot1 = ps*lam_1*S_1+pr*lam_1*R_1+pr*lam_1*IU_1+pr*lam_1*IA_1;
                                                            //44
  
  // return the rate of change
  List output;
  output["dY"]=dY;
  output["dCinc_det0"]=dCinc_det0;
  output["dCinc_tot0"]=dCinc_tot0;
  output["dS_0"]=dS_0;
  output["dIC_0"]=dIC_0;
  output["dIA_0"]=dIA_0;
  output["dIU_0"]=dIU_0;
  output["dR_0"]=dR_0;
  output["dTr_0"]=dTr_0;
  output["dSm_0"]=dSm_0;
  output["dRm_0"]=dRm_0;
  output["dS_1"]=dS_1;
  output["dIC_1"]=dIC_1;
  output["dIA_1"]=dIA_1;
  output["dIU_1"]=dIU_1;
  output["dR_1"]=dR_1;
  output["dTr_1"]=dTr_1;
  output["dSm_1"]=dSm_1;
  output["dRm_1"]=dRm_1;
  output["dCinc_det1"]=dCinc_det1;
  output["dCinc_tot1"]=dCinc_tot1;

  return output;
    
}

