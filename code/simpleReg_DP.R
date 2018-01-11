setwd("/Users/sepehrakhavan/Desktop/DP_in_Stan/Simple_DP_in_Stan")
library(rstan)
library(mvtnorm)

set.seed(1234)
N <- 300
LDA_K <- 10
beta0 <- 0
beta1 <- 0.5
sigma2 <- 0.5
X <- runif(N, 0.2, 1.0)
Y <- NULL
meanVal <- beta0 + beta1*X
for (i in 1:N){
  Y <- c(Y, rnorm(1, meanVal[i], sqrt(sigma2)))
}

data <- list(N = N, LDA_K = LDA_K, Y = Y, X = X)

Start <- Sys.time()
# I set the "fit" argument equal to TRUE!
fit <- stan(file = "simpleReg_DP.stan", data = data, iter = 1000, warmup = 500, chains = 1) # first fit

# fit <- stan(file = "code.stan", data = data, iter = 10000, warmup = 3000, chains = 1, fit = fit) # after the 1st compilation!
runTime <- Sys.time() - Start


# Let's try frequentist Weibull model:
myData <- data.frame(time = c(t_obs, t_cens), nu = c(rep(1, length(t_obs)), rep(0, length(t_cens))),
                     beta0L = c(beta0L_obs, beta0L_cens), kappa2 = c(kappa2_obs, kappa2_cens))

library(survival)
myfit <- survreg(Surv(time, nu) ~ beta0L + kappa2, data = myData)

survreg_WeibullWrapper <- function(FreqWeibSummary, nDecimal = 3){
  rslt.nrow <- nrow(FreqWeibSummary$table)
  rslt <- matrix(rep(NA, rslt.nrow*2), ncol = 2)
  colnames(rslt) <- c("estimate", "95% CI")
  rownames(rslt) <- c("tau", rownames(FreqWeibSummary$table)[1:(rslt.nrow - 1)])

  # tau:
  rslt[1,1] <- round(1/(FreqWeibSummary$scale), nDecimal)
  rslt[1,2] <- paste("(",toString(round((1/exp(c(FreqWeibSummary$table[rslt.nrow,1] - 1.96*FreqWeibSummary$table[rslt.nrow,2], FreqWeibSummary$table[rslt.nrow,1] + 1.96*FreqWeibSummary$table[rslt.nrow,2])))[c(2,1)],nDecimal)),")", sep = "")

  # lambda: lambda = log(1/(exp(intercept) ^ (scale^-1)))
  for (j in 2:(rslt.nrow)){
    rslt[j,1] <- round(log(  1/(exp(FreqWeibSummary$table[(j - 1),1])^(1/exp(FreqWeibSummary$table[rslt.nrow,1]))) ), nDecimal)
    Intercept.95CI <- c(FreqWeibSummary$table[(j - 1),1] - 1.96*FreqWeibSummary$table[(j - 1),2], FreqWeibSummary$table[(j - 1),1] + 1.96*FreqWeibSummary$table[(j - 1),2])
    rslt[j,2] <- paste("(",toString(round(log(1/(exp(Intercept.95CI)^(1/exp(FreqWeibSummary$table[rslt.nrow,1]))))[c(2,1)],nDecimal)),")", sep = "")
  }
  return(rslt)
}

survreg_WeibullWrapper(summary(myfit))
# permuted = TRUE: Permuting chains or not !
listOfArrays <- rstan::extract(fit, permuted = TRUE) # return a list of arrays 

# Let's summarize things:
tau_PM <- mean(listOfArrays$tau)
# beta0_PM <- mean(listOfArrays$beta0)
beta1_PM <- mean(listOfArrays$beta1)
beta2_PM <- mean(listOfArrays$beta2)

# c(tau_PM = tau_PM, beta0_PM = beta0_PM, beta1_PM = beta1_PM, beta2_PM = beta2_PM)
c(tau_PM = tau_PM, beta1_PM = beta1_PM, beta2_PM = beta2_PM)