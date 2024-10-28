$PROB 

$PLUGIN Rcpp

$PARAM @annotated @covariates

CL : 0.1 : Clearance (L/h)
V : 3.0 : Volume (L)
PREDOSE : 0.0 : Predose value (IU/L)
BASELINE : 5.0 : Endogenous factor level (IU/L)
LLOQ : 5.0 : Lower limit of quantification (IU/L)

$CMT @annotated
CENTRAL : Central () [OBS, ADM]

$MAIN
double K = CL/V;
double CORRECTED_BASELINE = R::fmax2(LLOQ/2.0, BASELINE);
double CORRECTED_PREDOSE = R::fmax2(0.0, PREDOSE - CORRECTED_BASELINE);

$ODE
dxdt_CENTRAL = - K * CENTRAL ;

$TABLE
double DV = (CENTRAL / V) + CORRECTED_BASELINE + CORRECTED_PREDOSE*exp(-K*TIME); 

$CAPTURE DV