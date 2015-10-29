
## Name: Elizabeth Lee
## Date: 9/28/15
## Function: JAGS model 1a: IMS Health provider coverage, total ILI reports, VERSION 3 (remove theta parameter)
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
  lambda ~ dunif(0, 0.1)
  phi ~ dunif(0.35, 0.75) 

  # model true iliSum state
  for (i in 1:n.loc){
    p[i] <- exp(-1/(upsilon[i]*phi))
    z[i] ~ dpois(lambda*eta[i])
    y[i] ~ dbin(p[i], z[i])
  } # end of i for loop
  
} # end of model

