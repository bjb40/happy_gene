//dev Stan 2.17.0
//Bryce Bartlett
//@@@@@@@@@@@@@@@@@@@

data { 
  int<lower=0> N; //n observations
  int<lower=0> IDS; //n individuals
  real y[N]; // outcomes
  int id[N]; // unique individual group
  int<lower=0> P; //dimensions of predictors
  matrix[N,P] z; // all time invariant
  } 

parameters{
  //individual level
  vector[P] beta; //mean for slope
  real<lower=0> sig; //l1 error; BDA3 388 - uniform gelman 2006; stan manual 66
  real<lower=0> zi; //(scale for intercept)
  vector[IDS] omega_i; //container for random normal draw to distribute cross-cell error
}

transformed parameters {
    vector[IDS] mu_i; // individual conditonal effects
    vector[N] yhat;
    mu_i = omega_i*zi;

  for(n in 1:N){
    yhat[n] = mu_i[id[n]] + z[n]*beta;
  }

}

model{
//parameter expansion for student-t on pp. 294-295 BDA3
  to_vector(omega_i) ~ normal(0,1);

    y ~ normal(yhat,sig);
  
  //prior
  beta ~ normal(0,5);
  sig ~ normal(0,5);

  zi ~ cauchy(0,5);

  
}

// see DIC in stan's google mailing list for discussion, BDA3, pp.172-179

generated quantities {
  //for WAIC
  vector[N] loglik; // log pointwise predictive density
  //for DIC
  real dev;
  //FOR PPD
  vector[N] ppd;

  dev = 0;  
  for(i in 1:N){
    loglik[i] = (normal_lpdf(y[i]|yhat[i],sig));
    dev = dev-(2*normal_lpdf(y[i]|yhat[i],sig));
    ppd[i] = normal_rng(yhat[i],sig);
  }

}
