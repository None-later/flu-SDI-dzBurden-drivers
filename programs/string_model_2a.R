
## Name: Elizabeth Lee
## Date: 10/26/15
## Function: string together analyses for detrended ilic metric divided by population size (ilicn.dt)
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

rm(list = ls())

set.seed(4262316)
setwd(dirname(sys.frame(1)$ofile))
source("main_model_2a.R")

#### params to loop through ####################################
metric.ls <- c("ilicnDt.sum", "ilicnDt.excess.BL", "ilicnDt.excess.thresh", "ilicnDt.peak", "epi.dur", "wks.to.epi", "wks.to.peak")
metcode.ls <- c("totIntens", "totExcessBL", "totExcessThresh", "pkIntens", "epiDur", "wksToEpi", "wksToPk")
metric.df <- data.frame(met = metric.ls, metcode = metcode.ls)
n.metrics <- 1:(length(metric.ls))
seasons <- 2:9

#### control flow for spatial scale ####################################
for (s in seasons){
  for (i in n.metrics){
    params <- list(metricname = metric.df[i,1], metriccode = metric.df[i,2], season = s)
    do.call(main_model_2a, c(params)) 
  }
  
}

