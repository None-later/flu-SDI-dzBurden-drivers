
## Name: Elizabeth Lee
## Date: 10/26/15
## Function: string together analyses for detrended ilic metric
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

rm(list = ls())

setwd(dirname(sys.frame(1)$ofile))
source("write_loess_fits_ILIc.R")
source("explore_loess_fits_ILIc.R")
source("write_periodicReg_fits_ilicDt_Octfit.R")
source("write_fullIndic_periodicReg_ilicDt.R")
source("explore_periodicReg_fits_ilicDt.R")
source("write_relativeDiseaseBurden_ilicDt.R")
source("explore_dbMetricsDistribution_ilicDt.R")


span.list <- c(0.6)

for (span in span.list){
  params <- list(span.var = span, degree.var = 2)
  
#   do.call(write_loess_fits_ILIc, c(params))
#   do.call(explore_loess_fits_ILIc, c(params))
#   do.call(write_periodicReg_fits_ilicDt_Octfit, c(params))
#   do.call(write_fullIndic_periodicReg_ilicDt, c(params))
#   do.call(explore_periodicReg_fits_ilicDt, c(params))
#   do.call(write_relativeDiseaseBurden_ilicDt, c(params))
  do.call(explore_dbMetricsDistribution_ilicDt, c(params))
}

