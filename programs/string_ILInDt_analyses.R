
## Name: Elizabeth Lee
## Date: 12/15/15
## Function: string together analyses for detrended ili metric divided by population size (ilin.dt)
## Filenames: 
## Data Source: 
## Notes: 12/12/15 - add switch between state-level and zip3-level
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

rm(list = ls())


setwd(dirname(sys.frame(1)$ofile))
source("write_loess_fits_ILIn.R")
source("explore_loess_fits_ILIn.R")
source("write_periodicReg_fits_ilinDt_Octfit.R")
source("write_periodicReg_fits_ilinDt_Octfit_emergency.R")
source("write_fullIndic_periodicReg_ilinDt.R")
source("explore_periodicReg_fits_ilinDt.R")
source("write_relativeDiseaseBurden_ilinDt.R")
source("explore_dbMetricsDistribution_ilinDt.R")
source("explore_periodicReg_inSeasonFits_ilinDt.R")

# zip to county conversion functions
source("source_clean_response_functions_cty.R")

# run outside of string, after "write_periodicReg_fits_ilinDt_Octfit.R"
# determines the lower threshold for consecutive weeks above the epidemic threshold
# source("explore_fluSeasonDefinition_ilinDt.R") 

#### set these! ####################################
spatial.scale <- "county"
span.list <- seq(0.4, 0.42, by=0.05)
deg <- 2

#### control flow for spatial scale ####################################
spatial.params <- list()
if (spatial.scale == "state"){
  spatial.params <- list(scale = spatial.scale, stringcode = "State", stringabbr = "_st")
} else if (spatial.scale == "zip3"){
  spatial.params <- list(scale = spatial.scale, stringcode = "Zip3", stringabbr = "", serv = "_totServ", servToggle = "") 
} else if (spatial.scale == "county"){
  spatial.params <- list(scale = spatial.scale, stringcode = "County", stringabbr = "_cty", serv = "_totServ", servToggle = "") 
}

# serv = "_totServ", servToggle = ""
# serv = "_emergency", servToggle = "_emergency"

for (span in span.list){
  params <- list(span.var = span, degree.var = deg, spatial = spatial.params)

  do.call(write_loess_fits_ILIn, c(params))
  do.call(explore_loess_fits_ILIn, c(params))
  do.call(write_periodicReg_fits_ilinDt_Octfit, c(params))
  do.call(write_fullIndic_periodicReg_ilinDt, c(params))
  do.call(explore_periodicReg_fits_ilinDt, c(params))
  do.call(write_relativeDiseaseBurden_ilinDt, c(params))
  do.call(explore_dbMetricsDistribution_ilinDt, c(params))
  do.call(explore_periodicReg_inSeasonFits_ilinDt, c(params))
}

