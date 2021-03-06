##sim from 
#https://ecosang.github.io/blog/study/dirichlet-process-with-stan/

rm(list = ls())
library(mixtools)
library(ggplot2)
library(tidyverse)
library(magrittr)
# Data generation code retrieved from
# http://www.jarad.me/615/2013/11/13/fitting-a-dirichlet-process-mixture

dat_generator <- function(truth) {
  set.seed(1)
  n = 500
  
  f = function(x) {
    out = numeric(length(x))
    for (i in 1:length(truth$pi)) out = out + truth$pi[i] * dnorm(x, truth$mu[i], 
                                                                  truth$sigma[i])
    out
  }
  y = rnormmix(n, truth$pi, truth$mu, truth$sigma)
  for (i in 1:length(truth$pi)) {
    assign(paste0("y", i), rnorm(n, truth$mu[i], truth$sigma[i]))
  }
  dat <- data_frame(y = y, y1 = y1, y2 = y2, y3 = y3)
}
truth = data.frame(pi = c(0.1, 0.5, 0.4), mu = c(-3, 0, 3), sigma = sqrt(c(0.5, 
                                                                           0.75, 1)))
dat <- dat_generator(truth)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_model <- "
data{
int<lower=0> C;//num of cludter
int<lower=0> N;//data num
real y[N];
}

parameters {
real mu_cl[C]; //cluster mean
real <lower=0,upper=1> v[C];
real<lower=0> sigma_cl[C]; // error scale
//real<lower=0> alpha; // hyper prior DP(alpha,base)
}

transformed parameters{
simplex [C] pi;
pi[1] = v[1];
// stick-break process based on The BUGS book Chapter 11 (p.294)
for(j in 2:(C-1)){
pi[j]= v[j]*(1-v[j-1])*pi[j-1]/v[j-1]; 
}
pi[C]=1-sum(pi[1:(C-1)]); // to make a simplex.
}

model {
real alpha = 1;
real a=0.001;
real b=0.001;
real ps[C];
sigma_cl ~ inv_gamma(a,b);
mu_cl ~ normal(0,5);
//alpha~gamma(6,1);
v ~ beta(1,alpha);

for(i in 1:N){
for(c in 1:C){
ps[c]=log(pi[c])+normal_lpdf(y[i]|mu_cl[c],sigma_cl[c]);
}
target += log_sum_exp(ps);
}

}
"
y <- dat$y
C <- 10  # to ensure large enough
N <- length(y)
input_dat <- list(y = y, N = N, C = C)
# model_object<-stan_model(model_code=stan_model)
fit <- stan(model_code = stan_model, data = input_dat, iter = 1000, chains = 1)
results <- rstan::extract(fit)