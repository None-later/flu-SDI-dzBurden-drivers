
## Name: Elizabeth Lee
## Date: 10/26/15
## Function: string together analyses for detrended ilic metric divided by population size (ilicn)
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

rm(list = ls())


setwd(dirname(sys.frame(1)$ofile))
source("write_loess_fits_ILIcn.R")
source("explore_loess_fits_ILIcn.R")
source("write_periodicReg_fits_ilicnDt_Octfit.R")
source("write_fullIndic_periodicReg_ilicnDt.R")
source("explore_periodicReg_fits_ilicnDt.R")
source("write_relativeDiseaseBurden_ilicnDt.R")
source("explore_dbMetricsDistribution_ilicnDt.R")

#### set these! ####################################
spatial.scale <- "state"
span.list <- seq(0.4, 0.4, by=0.1)

#### control flow for spatial scale ####################################
spatial.params <- list()
if (spatial.scale == "state"){
  spatial.params <- list(scale = spatial.scale, stringcode = "State", stringabbr = "_st")
} else if (spatial.scale == "zip3"){
  spatial.params <- list(scale = spatial.scale, stringcode = "Zip3", stringabbr = "")
}

for (span in span.list){
  params <- list(span.var = span, degree.var = 2, spatial = spatial.params)

  # do.call(write_loess_fits_ILIcn, c(params))
  # do.call(explore_loess_fits_ILIcn, c(params))
  # do.call(write_periodicReg_fits_ilicnDt_Octfit, c(params))
  # do.call(write_fullIndic_periodicReg_ilicnDt, c(params))
  # do.call(explore_periodicReg_fits_ilicnDt, c(params))
  # do.call(write_relativeDiseaseBurden_ilicnDt, c(params))
  do.call(explore_dbMetricsDistribution_ilicnDt, c(params))
}

