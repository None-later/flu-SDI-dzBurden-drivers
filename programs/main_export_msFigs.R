
## Name: Elizabeth Lee
## Date: 1/17/17
## Function: main code to generate MS figures
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(RColorBrewer); require(ggplot2); require(dplyr); require(tidyr); require(readr)
require(data.table)
require(lazyeval)
require(ggthemes)
setwd(dirname(sys.frame(1)$ofile))
source("source_export_msFigs.R")

################################
## MAIN ##
setwd(dirname(sys.frame(1)$ofile))

multiSeas_modCodeLs <- c("8a_iliSum_v2-6", "8e_epiDur_v2-3")
for (code in multiSeas_modCodeLs){
  choro_stateEffects(code)
  forest_coefDistr_seasEffects(code)
}

allCombs_modCodeLs <- c("8a_iliSum_v2-6", "8e_epiDur_v2-3", "8a_iliSum_v3-6", "8a_iliSum_v4-6")
for (code in allCombs_modCodeLs){
  forest_coefDistr_fixedEffects(code) # multi-season fixed effects
}

singleSeas_modCodeLs <- c("9a_iliSum_v2-4", "9e_epiDur_v2-2")
for (code in singleSeas_modCodeLs){
  forest_coefDistr_fixedEffects_singleSeason(code)
}

## DOT PLOTS ###
ctySeq_modCodeLs <- c("8a_iliSum_v2-6", "8a_iliSum_v2-6_c80", "8a_iliSum_v2-6_c60", "8a_iliSum_v2-6_c40", "8a_iliSum_v2-6_c20")
ctySeq_plotFormats <- list(w = 6, h = 3, lvls = ctySeq_modCodeLs, labs = c("complete", "80% of counties", "60% of counties","40% of counties", "20% of counties"), descrip = "ctySeq")
dot_coefCompare(ctySeq_modCodeLs, ctySeq_plotFormats)

seasSeq_modCodeLs <- c("8a_iliSum_v2-6", "8a_iliSum_v2-6_s6", "8a_iliSum_v2-6_s4", "8a_iliSum_v2-6_s2")
seasSeq_plotFormats <- list(w = 6, h = 3, lvls = seasSeq_modCodeLs, labs = c("complete", "missing 1", "missing 3","missing 5"), descrip = "seasSeq")
dot_coefCompare(seasSeq_modCodeLs, seasSeq_plotFormats)

missSeq_modCodeLs <- c("8a_iliSum_v2-6", "8a_iliSum_v2-6_m20", "8a_iliSum_v2-6_m40", "8a_iliSum_v2-6_m60", "8a_iliSum_v2-6_m80")
missSeq_plotFormats <- list(w = 6, h = 3, lvls = missSeq_modCodeLs, labs = c("complete", "missing 20%", "missing 40%", "missing 60%", "missing 80%"), descrip = "missSeq")
dot_coefCompare(missSeq_modCodeLs, missSeq_plotFormats)

ageSeq_modCodeLs <- c("8a_iliSum_v2-6", "8a_iliSum_v3-6", "8a_iliSum_v4-6")
ageSeq_plotFormats <- list(w = 6, h = 3, lvls = ageSeq_modCodeLs, labs = c("total", "children", "adults"), descrip = "ageSeq")
dot_coefCompare(ageSeq_modCodeLs, ageSeq_plotFormats)

## FIT CHOROS ###
pairCodeLs <- c("c80", "c60", "c40", "c20", "s6", "s4", "s2", "m20", "m40", "m60", "m80")
for (pairCode in pairCodeLs){
  pairLs <- c("8a_iliSum_v2-6", paste0("8a_iliSum_v2-6_", pairCode))
  pair_plotFormats <- list(w = 10, h = 5, descrip = paste0("8aV2-6_", pairCode), lvls = pairLs, labs = c("complete", pairCode))
  choro_fitCompare(pairLs, pair_plotFormats)
}
