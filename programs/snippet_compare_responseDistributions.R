
## Name: Elizabeth Lee
## Date: 10/14/16
## Function: examine appropriateness of gamma distribution for response variable
## Filenames: source_clean_response_functions_cty.R
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL) # clean_data_functions dependencies
require(ggplot2)

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
seasons <- 2:9

#### SOURCE: import data cleaning functions #################################
setwd("/home/elee/Dropbox/code")
source("return_gammaDistParams.R")

setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean original data sources

#### check iliSum response variable ###################
#### FILEPATHS ####
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
path_adjMxExport_cty <- paste0(getwd(), "/US_county_adjacency_fips.dat")

setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_adjMxExport_cty = path_adjMxExport_cty,
                  path_response_cty = path_response_cty)

#### custom functions ####
gamma_llf <- function(params, t1, t2, Nobs){
  print(match.call())
  
  shape <- params[1]
  rate <- params[2]
  # log-likelihood function
  return( - ((shape - 1)*t1 - rate*t2 - Nobs*shape*log(1/rate) - Nobs*log(gamma(shape))))
}

compare_gammaDistribution <- function(empiricalDat, plotname){
  print(match.call())
  
  Nobs <- length(empiricalDat)
  # moments for empirical data
  mu <- mean(empiricalDat); sigma <- sd(empiricalDat)
  # plug in moments to get params for associated gamma distribution
  params_mom <- return_gammaDistParams(mu, sigma)
  print(params_mom)
  # use method of moments to get simulated gamma distribution
  distr_mom <- rgamma(Nobs, shape=params_mom$shape, rate=params_mom$rate)
  # use maximum likelihood to get best simulated gamma distribution
  t1 <- sum(log(empiricalDat)); t2 <- sum(empiricalDat)
  mloptim <- optim(par=c(params_mom$shape, params_mom$rate), fn=gamma_llf, t1=t1, t2=t2, Nobs=Nobs)
  params_ml <- mloptim$par
  print(params_ml)
  distr_ml <- rgamma(Nobs, shape=params_ml[1], rate=params_ml[2])
  
  # plot simulated and empirical data
  png(plotname, width=w, height=h, units=un, res=res)
  par(oma=c(0,0,0,0), mfrow=c(2,2))
  mom <- hist(distr_mom, breaks=25, xlab="simulated method of moments", ylab="count", main="")
  ml <- hist(distr_ml, breaks=25, xlab="simulated maximum likelihood", ylab="count", main="")
  hist(empiricalDat, breaks=25, xlab="empirical non-zero data", ylab="count", main="")
  lines(ml$mids, ml$counts)
  qqplot(distr_ml, empiricalDat, xlab="simulated ML gamma", ylab="empirical data")
  abline(0,1)
  
  dev.off()
}

#### import data ####
rDat <- cleanR_iliSum_cty(path_list)

#### plot actual distribution and hypothetical gamma distribution for reponse variable ####
setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/compare_responseDistributions", showWarnings=FALSE)
setwd("../graph_outputs/compare_responseDistributions")

## seasonal plotting parameters ##
res <- 250
un <- "in"
w <- 6; h <- 6

## by season ##
for (s in seasons){
  # subset seasonal data
  y1Dat <- rDat %>% 
    filter(season == s & !is.na(y1)) %>%
    select(y1) %>% 
    unlist
  exportname <- paste0("gammaDistr_iliSum_S", s, ".png")
  compare_gammaDistribution(y1Dat, exportname)
}

## for all data ##
# subset seasonal data
y1totDat <- rDat %>% 
  filter(!is.na(y1)) %>%
  select(y1) %>% 
  unlist
totexportname <- "gammaDistr_iliSum_tot.png"
compare_gammaDistribution(y1totDat, totexportname)

# 10/14/16
