## Name: Elizabeth Lee
## Date: 10/4/17
## Function: JAGS model 9a VERSION 1: county-level seasonal intensity ~ single season model
## Filenames: 
## Data Source: 
## Notes: full model, cross-check INLA results
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### model #################################
model{
  
  # priors for random effects
  # https://people.ucsc.edu/~abrsvn/general_correlated_ranefs_bayes_jags.r
  mu_st ~ dnorm(0, 1/100) # mean hyperparamater for state random effect
  sigma_st ~ dunif(0, 100) # variance hyperparameter for state random effect
  tau_st <- 1/sigma_st
  mu_reg ~ dnorm(0, 1/100) # mean hyperparameter for region random effect
  sigma_reg ~ dunif(0, 100) # variance hyperparameter for region random effect
  tau_reg <- 1/sigma_reg

  for (i in 1:n.st) {
	alpha_st[i] ~ dnorm(mu_st, tau_st)
  } # end of st for loop
  for (i in 1:n.reg) {
	alpha_reg[i] ~ dnorm(mu_reg, tau_reg)
  } # end of reg for loop
  
  b0 ~ dnorm(0, 1/100) # deterministic intercept
  b1 ~ dnorm(0, 1/100) # imscoverage
  b2 ~ dnorm(0, 1/100) # careseek
  b3 ~ dnorm(0, 1/100) # insured
  b4 ~ dnorm(0, 1/100) # percent of population in poverty
  b5 ~ dnorm(0, 1/100) # child pop
  b6 ~ dnorm(0, 1/100) # adult pop
  b7 ~ dnorm(0, 1/100) # hospaccess
  b8 ~ dnorm(0, 1/100) # popdensity
  b9 ~ dnorm(0, 1/100) # housdensity
  b10 ~ dnorm(0, 1/100) # vaxcovI
  b11 ~ dnorm(0, 1/100) # vaxcovE
  b12 ~ dnorm(0, 1/100) # H3A
  b13 ~ dnorm(0, 1/100) # B
  b14 ~ dnorm(0, 1/100) # priorImmunity
  b15 ~ dnorm(0, 1/100) # humidity
  b16 ~ dnorm(0, 1/100) # pollution
  b17 ~ dnorm(0, 1/100) # singlePersonHH
  b18 ~ dnorm(0, 1/100) # H3A * adult
  b19 ~ dnorm(0, 1/100) # B * child
  

  # model true seasIntens state
  for (i in 1:n.loc){
    log(mu_y[i]) <- (b0 + b1*O_imscoverage[i] + b2*O_careseek[i] + b3*O_insured[i] + b4*X_poverty[i] + b5*X_child[i] + b6*X_adult[i] + b7*X_hospaccess[i] + b8*X_popdensity[i] + b9*X_housdensity[i] + b10*X_vaxcovI[i] + b11*X_vaxcovE[i] + b12*X_H3A[i] + b13*X_B[i] + b14*X_priorImmunity[i] + b15*X_humidity[i] + b16*X_pollution[i] + b17*X_singlePersonHH[i] + b18*X_H3A[i]*X_adult[i] + b19*X_B[i]*X_child[i] + alpha_st[fips_st[i]] + alpha_reg[regionID[i]]) 
    
    y[i] ~ dpois(mu_y[i]) 

  } # end of n.loc for loop
} # end of model
