##
#Conducts bayesian analysis

###
rm(list=ls())

source('code/config~.R',echo=TRUE)

library(rstan)
library(bayesplot)

#detect cores to activate parallel procesing
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
options(mc.cores = 3) #leave one core free for work

##load data
load('analyze_dat~.RData') #data only

#select variables and listwize delete
znames = c('md_uer','iuer','iret','iunemp','md_ret','md_unemp','male')

analyze = analyze %>%
  dplyr::select(hhidpn,cesd,pgs.swb,wave,one_of(znames)) %>%
  arrange(hhidpn)

analyze = analyze[complete.cases(analyze),]

#random subset of 200 individuals for model checking
ss = sample(unique(analyze$hhidpn),250,replace=FALSE)
analyze = analyze %>%
  filter(hhidpn %in% ss)

#assign an integer index number
analyze$clust = as.numeric(factor(analyze$hhidpn))
analyze$intercept = 1
analyze$td = as.numeric(cut(analyze$wave,11))
analyze$time = as.numeric(factor(analyze$wave))

#set list objects for sending to stan
y=analyze$cesd
id=analyze$clust
z=analyze[,c('intercept','time',znames)] #time should be completely controlled in error str. (mu_i)
t=analyze$pgs.swb #this essentially becomes an interactoin
td=analyze$td #include all factors of time

N=length(y)
IDS=length(unique(id))
YRS=length(unique(analyze$time))
P = ncol(z)
TDS = length(unique(td))
yrctr=0

fit = stan("H:/projects/happy_gene/code/bhm-changepoint_rev1.stan", 
          data=c("y","id","z","t","td","P","N","IDS","TDS","YRS","yrctr"),
          chains=3,iter=500,verbose=T)

z = analyze[,c('intercept', znames)] #take out time
P=ncol(z)

fit_i = stan("H:/projects/happy_gene/code/bhm-changepoint_rev.stan", 
            data=c("y","id","z","t","td","P","N","IDS","TDS","YRS","yrctr"),
            chains=3,iter=500,verbose=T)

samp = summary(fit,pars=c('beta','gamma','zi','delta','sig'))
print(samp$summary)

ss = as.data.frame(extract(fit,pars='beta'))
colnames(ss) = seq(1994,2014,by=2)
print(mcmc_intervals(ss, prob=0.84,
                     prob_outer=0.95))

gg = as.data.frame(extract(fit,pars='gamma')) 
colnames(gg) = colnames(z)
print(mcmc_intervals(gg, prob=0.84,
                     prob_outer=0.95))

  
  
save.image('bayes~.RData')

#fit <- stan("H:/projects/happy_gene/code/random_effects_simp.stan", 
#            data=c("y","id","z","P","N","IDS"),
            #algorithm='HMC',
#            chains=3,iter=500,verbose=T)
#60 seconds -- simp (warmpup + sampling)

#fit1 <- stan("H:/projects/happy_gene/code/random_effects_simp1.stan", 
#            data=c("y","id","z","P","N","IDS"),
            #algorithm='HMC',
#            chains=3,iter=500,verbose=T)
#202 seconds

#fit2 <- stan("H:/projects/happy_gene/code/random_effects_simp2.stan", 
#             data=c("y","id","z","P","N","IDS"),
             #algorithm='HMC',
#             chains=3,iter=500,verbose=T)

#print summary
#ss = summary(fit,pars='beta')$summary
#rownames(ss) = colnames(z)
#print(ss)

#ss1 = summary(fit1,pars='beta')$summary
#rownames(ss1) = colnames(z)
#print(ss1)

#ss2 = summary(fit2,pars='beta')$summary
#rownames(ss2) = colnames(z)
#print(ss2)

print(get_elapsed_time(fit))
print(get_elapsed_time(fit_i))
#print(get_elapsed_time(fit2))
#confirm with lme4
#print(summary(lmer(cesd~pm_uer +
#       (1|hhidpn),data=analyze)))

