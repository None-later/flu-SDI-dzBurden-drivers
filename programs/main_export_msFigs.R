
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
source("source_clean_response_functions_cty.R")

################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"

## PATHS ##
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_response_cty = path_response_cty)

################################
## MAIN ##
setwd(dirname(sys.frame(1)$ofile))

## PLOTS ##
###############################################################################
multiSeas_modCodeLs <- c("8a_iliSum_v2-6")
obsFit_plotFormats <- list(w = 7, h = 3.5)
for (code in multiSeas_modCodeLs){
  choro_obsFit_seasIntensityRR(code, obsFit_plotFormats, path_list)
}

# multiSeas_modCodeLs <- c("8a_iliSum_v2-6", "8e_epiDur_v2-3")
# for (code in multiSeas_modCodeLs){
#   choro_stateEffects(code)
#   forest_coefDistr_seasEffects(code)
# }

# allCombs_modCodeLs <- c("8a_iliSum_v2-6", "8e_epiDur_v2-3", "8a_iliSum_v3-6", "8a_iliSum_v4-6")
# for (code in allCombs_modCodeLs){
#   forest_coefDistr_fixedEffects(code) # multi-season fixed effects
# }

# singleSeas_modCodeLs <- c("9a_iliSum_v2-4", "9e_epiDur_v2-2")
# for (code in singleSeas_modCodeLs){
#   forest_coefDistr_fixedEffects_singleSeason(code)
# }


###############################################################################
### DOT PLOTS - Replicate comparison ###################
# ## missing county sequence
# baseCtySeq <- c("8a_iliSum_v2-6", "8a_iliSum_v2-6_c80", "8a_iliSum_v2-6_c60", "8a_iliSum_v2-6_c40", "8a_iliSum_v2-6_c20")
# ctyPlotFormats <- list(w = 6, h = 3, lvls = baseCtySeq, labs = c("complete", "80% of counties", "60% of counties","40% of counties", "20% of counties"), replvls = c(.25, .5, .75, 1), replabs = c("25", "50", "75", "100"), descrip = "ctySeq", numReplicates = 10)
# dot_coefCompareReplicates(baseCtySeq, ctyPlotFormats)
# 
# ## missing stratified county sequence
# baseMissSeq <- c("8a_iliSum_v2-6", "8a_iliSum_v2-6_m20", "8a_iliSum_v2-6_m40", "8a_iliSum_v2-6_m60", "8a_iliSum_v2-6_m80")
# missPlotFormats <- list(w = 6, h = 3, lvls = baseMissSeq, labs = c("complete", "missing 20%", "missing 40%", "missing 60%", "missing 80%"), replvls = c(.25, .5, .75, 1), replabs = c("25", "50", "75", "100"), descrip = "missSeq", numReplicates = 10)
# dot_coefCompareReplicates(baseMissSeq, missPlotFormats)
# 
# ## missing season sequence
# baseSeasSeq <- c("8a_iliSum_v2-6", "8a_iliSum_v2-6_s6", "8a_iliSum_v2-6_s4", "8a_iliSum_v2-6_s2")
# seasPlotFormats <- list(w = 6, h = 3, lvls = baseSeasSeq, labs = c("complete", "missing 1", "missing 3","missing 5"), replvls = c(.25, .5, .75, 1), replabs = c("25", "50", "75", "100"), descrip = "seasSeq", numReplicates = 10)
# dot_coefCompareReplicates(baseSeasSeq, seasPlotFormats)

### FIT CHOROS - Replicate comparison ###################
# ## missing county sequence
# baseCtySeq <- c("8a_iliSum_v2-6", "8a_iliSum_v2-6_c40", "8a_iliSum_v2-6_c20")
# ctyPlotFormats <- list(w = 6, h = 2.5, lvls = baseCtySeq[2:length(baseCtySeq)], labs = c("40% of counties", "20% of counties"), replvls = c(.25, .5, .75, 1), replabs = c("25", "50", "75", "100"), descrip = "ctySeq", repcodelength = 5, numReplicates = 10, nomatchThresh = 0.5)
# choro_fitCompareReplicates(baseCtySeq, ctyPlotFormats)
# 
# ## missing stratified county sequence
# baseMissSeq <- c("8a_iliSum_v2-6", "8a_iliSum_v2-6_m60", "8a_iliSum_v2-6_m80")
# missPlotFormats <- list(w = 6, h = 2.5, lvls = baseMissSeq[2:length(baseMissSeq)], labs = c("missing 60%", "missing 80%"), replvls = c(.25, .5, .75, 1), replabs = c("25", "50", "75", "100"), descrip = "missSeq", repcodelength = 5, numReplicates = 10, nomatchThresh = 0.5)
# choro_fitCompareReplicates(baseMissSeq, missPlotFormats)
# 
# ## missing season sequence
# baseSeasSeq <- c("8a_iliSum_v2-6", "8a_iliSum_v2-6_s6", "8a_iliSum_v2-6_s4", "8a_iliSum_v2-6_s2")
# seasPlotFormats <- list(w = 6, h = 2.5, lvls = baseSeasSeq[2:length(baseSeasSeq)], labs = c("missing 1", "missing 3","missing 5"), replvls = c(.25, .5, .75, 1), replabs = c("25", "50", "75", "100"), descrip = "seasSeq", repcodelength = 4, numReplicates = 10, nomatchThresh = 0.5)
# choro_fitCompareReplicates(baseSeasSeq, seasPlotFormats)


###############################################################################
# ### DOT PLOTS - Direct comparison ###################
# repLs <- c("", paste0("-", 4))
# # compare the sequence from a single replicate against the complete model 
# for(rep in repLs){
#   ctySeq <- paste0(c("8a_iliSum_v2-6_c80", "8a_iliSum_v2-6_c60", "8a_iliSum_v2-6_c40", "8a_iliSum_v2-6_c20"), rep)
#   seasSeq <- paste0(c("8a_iliSum_v2-6_s6", "8a_iliSum_v2-6_s4", "8a_iliSum_v2-6_s2"), rep)
#   missSeq <- paste0(c("8a_iliSum_v2-6_m20", "8a_iliSum_v2-6_m40", "8a_iliSum_v2-6_m60", "8a_iliSum_v2-6_m80"), rep)

#   ctySeq_modCodeLs <- c("8a_iliSum_v2-6", ctySeq)
#   ctySeq_plotFormats <- list(w = 6, h = 3, lvls = ctySeq_modCodeLs, labs = c("complete", "80% of counties", "60% of counties","40% of counties", "20% of counties"), descrip = paste0("ctySeq", rep))
#   dot_coefCompare(ctySeq_modCodeLs, ctySeq_plotFormats)

#   seasSeq_modCodeLs <- c("8a_iliSum_v2-6", seasSeq)
#   seasSeq_plotFormats <- list(w = 6, h = 3, lvls = seasSeq_modCodeLs, labs = c("complete", "missing 1", "missing 3", "missing 5"), descrip = paste0("seasSeq", rep))
#   dot_coefCompare(seasSeq_modCodeLs, seasSeq_plotFormats)

#   missSeq_modCodeLs <- c("8a_iliSum_v2-6", missSeq)
#   missSeq_plotFormats <- list(w = 6, h = 3, lvls = missSeq_modCodeLs, labs = c("complete", "missing 20%", "missing 40%", "missing 60%", "missing 80%"), descrip = paste0("missSeq", rep))
#   dot_coefCompare(missSeq_modCodeLs, missSeq_plotFormats)
# }

# ageSeq_modCodeLs <- c("8a_iliSum_v2-6", "8a_iliSum_v3-6", "8a_iliSum_v4-6")
# ageSeq_plotFormats <- list(w = 6, h = 3, lvls = ageSeq_modCodeLs, labs = c("total", "children", "adults"), descrip = "ageSeq")
# dot_coefCompare(ageSeq_modCodeLs, ageSeq_plotFormats)


# ### FIT CHOROS - Direct comparison ###################
# repLs <- c("", paste0("-", 4))
# pairCodeLs <- c("c80", "c60", "c40", "c20", "s6", "s4", "s2", "m20", "m40", "m60", "m80")
# for (code in pairCodeLs){
#   for (rep in repLs){
#     pairCode <- paste0(code, rep)
#     pairLs <- c("8a_iliSum_v2-6", paste0("8a_iliSum_v2-6_", pairCode))
#     pair_plotFormats <- list(w = 10, h = 5, descrip = paste0("8aV2-6_", pairCode), lvls = pairLs, labs = c("complete", pairCode))
#     choro_fitCompare(pairLs, pair_plotFormats)
#   }
# }
