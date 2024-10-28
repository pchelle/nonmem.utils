$PROB 

$PLUGIN Rcpp

$PARAM @annotated @covariates

CL : 0.1 : Clearance (L/h)
V1 : 3.0 : Volume (L)
Q2 : 0.1 : Intercompartment Clearance 2 (L/h)
V2 : 0.3 : Peripheral Volume 2 (L)
Q3 : 0.1 : Intercompartment Clearance 3 (L/h)
V3 : 0.3 : Peripheral Volume 3 (L)
PREDOSE : 0.0 : Predose value (IU/L)
BASELINE : 5.0 : Endogenous factor level (IU/L)
LLOQ : 5.0 : Lower limit of quantification (IU/L)

$CMT @annotated
CENTRAL : Central () [OBS, ADM]
PERIPH2 : Peripheral2 () []
PERIPH3 : Peripheral3 () []

$MAIN
double K = CL/V1;
double K12 = Q2/V1;
double K21 = Q2/V2;
double K13 = Q3/V1;
double K31 = Q3/V3;

double XA0=K*K21*K31;
double XA1=K*K31+K21*K31+K21*K13+K*K21+K31*K12;
double XA2=K+K12+K21+K13+K31;
double XP = XA1-XA2*XA2/3.0;
double XQ = 2.0*XA2*XA2*XA2/27.0-XA1*XA2/3.0+XA0;
double XR1 = sqrt(-XP*XP*XP/27.0);
double XR2 = 2.0*exp(log(XR1)/3.0);
 
double XPSI = acos(-XQ/(XR1*2.0))/3.0;
double GAMMA = -(cos(XPSI)*XR2-XA2/3.0);
//double PI=3.141592653
//double ALPHA=-(COS(XPSI+2.0D+00*PI/3.0D+00)*XR2-XA2/3.0D+00)
//double BETA=-(COS(XPSI+4.0D+00*PI/3.0D+00)*XR2-XA2/3.0D+00)
double CORRECTED_BASELINE = R::fmax2(LLOQ/2.0, BASELINE);
double CORRECTED_PREDOSE = R::fmax2(0.0, PREDOSE - CORRECTED_BASELINE);

$ODE
dxdt_CENTRAL = -(K+K12+K13) * CENTRAL + K21 * PERIPH2 + K31 * PERIPH3 ;
dxdt_PERIPH2 = K12 * CENTRAL - K21 * PERIPH2 ;
dxdt_PERIPH3 = K13 * CENTRAL - K31 * PERIPH3 ;

$TABLE
double DV = (CENTRAL / V1) + CORRECTED_BASELINE + CORRECTED_PREDOSE*exp(-GAMMA*TIME); 

$CAPTURE DV
