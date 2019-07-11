double beta0_new = (1-(1-eta)*effIRS*covIRS)*(1-effITN*covITN)*beta0;
double beta1_new = (1-(1-eta)*effIRS*covIRS)*(1-effITN*covITN)*beta1;

double lams = ((homogen/2)*(beta0_new*(IC_0+Tr_0+rhoa*IA_0+rhou*IU_0)+beta1_new*(IC_1+Tr_1+rhoa*IA_1+rhou*IU_1))/(P0+P1));

double lam0 = ((1-homogen)*beta0_new*(IC_0+Tr_0+rhoa*IA_0+rhou*IU_0)/P0)+lams;
double lam1 = ((1-homogen)*beta1_new*(IC_1+Tr_1+rhoa*IA_1+rhou*IU_1)/P1)+lams;

