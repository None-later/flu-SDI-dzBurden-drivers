
## Name: Elizabeth Lee
## Date: 5/9/17
## Function: perform anova analysis for fixed and iid random effects
## Filenames:
## Data Source: IMS Health
## Notes:
##
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr) 
require(RColorBrewer); require(ggplot2) # export_inlaData_st dependencies
setwd(getSrcDirectory(function(x){}))

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr <- "8f_wksToEpi_v2-2"

#### fxn: calculate finite-population standard deviation ####
calculate_fp_sd <- function(batch_coef){
  print(match.call())
  
  df <- length(batch_coef)-1
  grandmean <- mean(batch_coef)
  fp_sd <- sqrt((1/df)*sum((batch_coef-grandmean)^2))
  return(fp_sd)
}

#### MAIN #################################
#### import summary statistics ####
summStatsPath <- sprintf("../R_export/inlaModelData_export/%s/summaryStats_%s.csv", modCodeStr, modCodeStr)
summStatsDat <- read_csv(summStatsPath, col_types = cols_only(RV = "c", effectType = "c", mean = "d", sd = "d")) 

#### calculate anova table ####
effectTypeLs <- c("fixed", "spatial", "stID", "regID", "season", "error")
fpSDLs <- rep(NA, length(effectTypeLs))

for (i in 1:length(effectTypeLs)){
  # subset coefficient batches into different dataframes
  dummyDf <- summStatsDat %>% filter(effectType == effectTypeLs[i])
  fpSDLs[i] <- calculate_fp_sd(dummyDf$mean)
}
print(modCodeStr)
anovaDf <- data.frame(effectType = effectTypeLs, fp_sd = fpSDLs)




