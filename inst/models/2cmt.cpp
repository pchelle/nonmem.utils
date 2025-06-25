$PROB 

$PLUGIN Rcpp

$PARAM @annotated @covariates

CL : 0.1 : Clearance (L/h)
V1 : 3.0 : Volume (L)
Q : 0.1 : Intercompartment Clearance (L/h)
V2 : 0.3 : Peripheral Volume (L)
PREDOSE : 0.0 : Predose value (IU/L)
BASELINE : 0.0 : Endogenous factor level (IU/L)

$CMT @annotated
CENTRAL : Central () [OBS, ADM]
PERIPHERAL : Peripheral () []

$MAIN
double K = CL/V1;
double K12 = Q/V1;
double K21 = Q/V2;
double SUMK = K+K12+K21;
double SQ = sqrt(SUMK*SUMK-4.0*K*K21);
//double ALPHA = (SUMK+SQ)/2.0;
double BETA=(SUMK-SQ)/2.0;
double CORRECTED_PREDOSE = R::fmax2(0.0, PREDOSE - BASELINE);

$ODE
dxdt_CENTRAL = -(K+K12) * CENTRAL + K21 * PERIPHERAL ;
dxdt_PERIPHERAL = K12 * CENTRAL - K21 * PERIPHERAL ;

$TABLE
double DV = (CENTRAL / V1) + BASELINE + CORRECTED_PREDOSE*exp(-BETA*TIME); 

$CAPTURE DV
