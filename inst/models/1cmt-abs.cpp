$PROB 

$PLUGIN Rcpp autodec nm-vars

$PARAM @annotated

CL : 0.1 : Clearance (L/h)
V : 0.2 : Central Volume (L)
KA : 0.1 : First Absorption Rate (1/h)
PREDOSE : 0.0 : Predose value (IU/L)
BASELINE : 0.0 : Endogenous factor level (IU/L)

$CMT @annotated
DEPOT : Depot () [ADM]
CENTRAL : Central () [OBS]

$MAIN
double K = CL/V;
double CORRECTED_PREDOSE = R::fmax2(0.0, PREDOSE - BASELINE);

$ODE
dxdt_DEPOT = -KA*DEPOT;
dxdt_CENTRAL = KA*DEPOT - K*CENTRAL;

$TABLE
double DV = (CENTRAL / V) + BASELINE + CORRECTED_PREDOSE*exp(-K*TIME); 

$CAPTURE DV