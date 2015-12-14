
## Name: Elizabeth Lee
## Date: 12/12/15
## Function: JAGS model 2a VERSION 4: state-level seasonal intensity ~ intercept + uninsured pop 
## Filenames: 
## Data Source: Overleaf Models document
## Notes:
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### model #################################
model{
  # priors
  sigma_m ~ dunif(0, 100) # measurement error
  tau_m <- 1/sigma_m
  sigma_p ~ dunif(0, 100) # process variance
  tau_p <- 1/sigma_p
  
  b0 ~ dnorm(0, 1/100) # deterministic intercept
  b2 ~ dnorm(0, 1/100) # percent of population uninsured

  # model true seasIntens state
  for (i in 1:n.loc){
    mu_z[i] <- b0 + b2*x2unins[i]
    
    z[i] ~ dnorm(mu_z[i], tau_p)
    y[i] ~ dnorm(z[i], tau_m)
  } # end of i for loop
  
} # end of model

