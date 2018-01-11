//@@@@@@@@@@@@@@@@@@@
//modified changepoint model/simplifed interaction
//this is really a multigroup hierarchical, b/c we know the changepoint
//but it is like the Carlin 1992 model on p. 401, with k known
//dev Stan 2.17.0
//Bryce Bartlett
//@@@@@@@@@@@@@@@@@@@

data { 
  int<lower=0> N; //n observations
  int<lower=0> IDS; //n cells
  int<lower=0> TDS; //n groups (2) in this case
  int<lower=0> YRS; //n year-specific effects
  int yrctr; //integer to center year for index 1: end-year
  int id[N]; // unique groups (for random effect)
  real y[N]; // outcomes
  real t[N]; //time variable (years)
  int td[N]; //index number for group of time
  int<lower=0> P; //dimensions of predictors
  matrix[N,P] z; // all time invariant
  } 
  
parameters{
  //individual level
  vector[TDS] beta; // grand mean coefficients for intercept and slope
  vector[P] gamma; //
  real<lower=0> sig;//l1 error; BDA3 388 - uniform gelman 2006; stan manual 66
  vector<lower=0>[TDS] zi; // scale for correlation matrix
  real<lower=0> delta; //scale for year-specific errors
  cholesky_factor_corr[TDS] L_Omega; //faster for programming; correlation matrix
  vector[TDS] omega_t; //container for random normal draw to distribute cross-cell error
  vector[IDS] mu_i; //random error for years - robust on student t
}

transformed parameters {
    vector[TDS] mu_t; // time-specific random effect
    vector[N] yhat;
    //vector[N] sigma; //container for 2 level 1 variances
    mu_t = diag_matrix(zi)*L_Omega*omega_t;

  for(n in 1:N){
    yhat[n] = mu_i[id[n]] + t[n]*mu_t[td[n]] + t[n]*beta[td[n]] + z[n]*gamma;
    
  }

}

model{

  to_vector(omega_t) ~ normal(0,1);

  y ~ normal(yhat,sig);
  
  //prior
  to_vector(beta) ~ normal(0,5);
  to_vector(gamma) ~ normal(0,5);
  sig ~ normal(0,5);
  zi ~ cauchy(0,10);
  delta ~ cauchy(0,5);
  //mu_t ~ student_t(1,0,delta);
  mu_i ~ student_t(1,0,delta);
  L_Omega ~ lkj_corr_cholesky(1); //1 is equiv to uniform prior; >1 diagonal <1 high

}
