
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
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")
setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_st = path_latlon_st,
                  path_latlon_cty = path_latlon_cty,
                  path_response_st = path_response_st,
                  path_response_cty = path_response_cty)

################################
## MAIN ##
setwd(dirname(sys.frame(1)$ofile))


## PLOTS ##
###############################################################################
### SEASONAL INTENSITY - total population ###################
# obsFit_plotFormats_one <- list(w = 7, h = 3.5)
# choro_obsFit_seasIntensityRR_oneSeason("8a_iliSum_v2-6", obsFit_plotFormats_one, path_list)
# 
# fit_plotFormats_one <- list(w = 3.1, h = 2.5, popCode = "")
# choro_fit_seasIntensityRR_oneSeason("8a_iliSum_v2-6", fit_plotFormats_one, path_list)
# 
# obsFit_plotFormats_multi <- list(w = 4.25, h = 9, popCode = "") # add (rmSeas = "2006-07") arg if needed and (h = 8)
# choro_obsFit_seasIntensityRR_multiSeason("8a_iliSum_v2-6", obsFit_plotFormats_multi, path_list)

# obsFit_plotFormats_scatter <- list(w = 6, h = 4)
# scatter_obsFit_seasIntensityRR_multiSeason("8a_iliSum_v2-6", obsFit_plotFormats_scatter, path_list)
# scatter_obsFit_excessSeasIntensityRR_multiSeason("8a_iliSum_v2-6", obsFit_plotFormats_scatter, path_list)
# scatter_residFit_logSeasIntensity_multiSeason("8a_iliSum_v2-6", obsFit_plotFormats_scatter, path_list)

# forest_coefDistr_stateStructuredEffects("8a_iliSum_v6-3")
# forest_coefDistr_precisionTerms("8a_iliSum_v2-6")

# plotFormats_scatter_regionValidationViral <- list(w = 4, h = 4, xmax = 30)
# scatter_regionValidationViral("8a_iliSum_v2-6", plotFormats_scatter_regionValidationViral)


###############################################################################
### EPIDEMIC DURATION - total population ###################
# fit_plotFormats_one <- list(w = 3, h = 2.3, popCode = "")
# choro_fit_epiDuration_oneSeason("8e_epiDur_v2-3", fit_plotFormats_one, path_list)

# obsFit_plotFormats_one <- list(w = 7, h = 3.5)
# choro_obsFit_epiDuration_oneSeason("8e_epiDur_v2-3", obsFit_plotFormats_one, path_list)

# obsFit_plotFormats_multi_epiDur <- list(w = 4.25, h = 9, popCode = "")
# choro_obsFit_epiDuration_multiSeason("8e_epiDur_v2-3", obsFit_plotFormats_multi_epiDur, path_list)

# obsFit_plotFormats_scatter <- list(w = 6, h = 4)
# scatter_obsFit_epiDuration_multiSeason("8e_epiDur_v2-3", obsFit_plotFormats_scatter, path_list)
# scatter_residFit_epiDuration_multiSeason("8e_epiDur_v2-3", obsFit_plotFormats_scatter, path_list)


###############################################################################
# ### EPIDEMIC DURATION vs. SEASONAL INTENSITY ###################
# pltFormats_epiDur_seasInt <- list(w = 5, h = 8, modLabs = c("seasIntensity", "epiDuration"))
# scatter_obsFit_seasInt_epiDur_multiSeason(c("8a_iliSum_v2-6", "8e_epiDur_v2-3"), pltFormats_epiDur_seasInt, path_list)


###############################################################################
### AGGREGATION BIAS: DIFFERENCE BETWEEN COUNTY-STATE ###################

# test <- import_fit_aggBias_seasIntensityRR("8a_iliSum_v2-6", "10a_iliSum_v2-3", path_list)
# 
# fit_aggBias_plotFormats_one <- list(w = 3, h = 2.5)
# choro_fit_aggBias_seasIntensityRR_oneSeason("8a_iliSum_v2-6", "10a_iliSum_v2-3", fit_aggBias_plotFormats_one, path_list)
# 
# fit_aggBias_plotFormats_mult <- list(w = 7, h = 4.25)
# choro_fit_aggBias_seasIntensityRR_multiSeason("8a_iliSum_v2-6", "10a_iliSum_v2-3", fit_aggBias_plotFormats_mult, path_list)

# stCty_fit_plotFormats_one <- list(w = 3, h = 1.9, stName = "south carolina")
# choro_stCty_fit_seasIntensityRR_oneSeason("8a_iliSum_v2-6", "10a_iliSum_v2-3", stCty_fit_plotFormats_one, path_list)
# 
# stCty_fit_plotFormats_one <- list(w = 3.1, h = 1.9, stName = "montana", legendStep = 0.5)
# choro_stCty_fit_seasIntensityRR_oneSeason("8a_iliSum_v2-6", "10a_iliSum_v2-3", stCty_fit_plotFormats_one, path_list)
# 
# stCty_fit_plotFormats_one <- list(w = 3.1, h = 1.9, stName = "kansas", legendStep = 0.5)
# choro_stCty_fit_seasIntensityRR_oneSeason("8a_iliSum_v2-6", "10a_iliSum_v2-3", stCty_fit_plotFormats_one, path_list)


# # Is there more heterogeneity within states that have greater under or overestimation?
# scatter_fitVariance_aggBias_plotFormats <- list(w = 6, h = 4)
# test <- scatter_corr_fitVariance_aggBias("8a_iliSum_v2-6", "10a_iliSum_v2-3", scatter_fitVariance_aggBias_plotFormats, path_list)

###############################################################################
### STATE SEASONAL INTENSITY MODELS ###################

# fit_plotFormats_choroSt_one <- list(w = 3.15, h = 2.5, popCode = "", legendStep = 1)
# choroSt_fit_seasIntensityRR_oneSeason("10a_iliSum_v2-3", fit_plotFormats_choroSt_one, path_list)
# 
# obsFit_plotFormats_choroSt_multi <- list(w = 4.25, h = 9, popCode = "", manualBreaks = seq(-4,3,by=1)) 
# choroSt_obsFit_seasIntensityRR_multiSeason("10a_iliSum_v2-3", obsFit_plotFormats_choroSt_multi, path_list)

# choro_stateEffects("10a_iliSum_v2-3") # no states were significant

###############################################################################
### MULTI-SEASON MODELS ###################
# allCombs_modCodeLs <- c("8a_iliSum_v2-6", "8e_epiDur_v2-3", "8a_iliSum_v3-6", "8a_iliSum_v4-6")
# allCombs_modCodeLs <- c("8a_iliSum_v2-6", "8e_epiDur_v2-3")
# allCombs_modCodeLs <- c("9a_iliSum_2009p_v1-2") # pandemic model
# for (code in allCombs_modCodeLs){
#   choro_stateEffects(code)
#   # 5/17/17 account for variability within different random effects
#   forest_coefDistr_seasEffects(code)
#   forest_coefDistr_regionEffects(code)
#   forest_coefDistr_stateEffects(code) #8a V3-6 and 8a V4-6 don't have graphIdx_st in ids file
#   forest_coefDistr_ctyEffects_sample(code)
#   forest_coefDistr_errorEffects_sample(code)
#   forest_coefDistr_fixedEffects(code, TRUE) # multi-season fixed effects, T/F is2009p
# }


###############################################################################
# ### FOREST PLOTS - Single season ###################
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
# 

###############################################################################
### FIT CHOROS - Replicate comparison ###################
## missing county sequence
baseCtySeq <- c("8a_iliSum_v2-6", "8a_iliSum_v2-6_c40", "8a_iliSum_v2-6_c20")
ctyPlotFormats <- list(w = 6, h = 2, lvls = baseCtySeq[2:length(baseCtySeq)], labs = c("40% of counties", "20% of counties"), replvls = c(.25, .5, .75, 1), replabs = c("25", "50", "75", "100"), descrip = "ctySeq", repcodelength = 5, numReplicates = 10, nomatchThresh = 0.1)
choro_fitCompareReplicates(baseCtySeq, ctyPlotFormats)

baseCtySeq <- c("8a_iliSum_v2-6", "8a_iliSum_v2-6_c10", "8a_iliSum_v2-6_c05")
ctyPlotFormats <- list(w = 6, h = 2, lvls = baseCtySeq[2:length(baseCtySeq)], labs = c("10% of counties", "5% of counties"), replvls = c(.25, .5, .75, 1), replabs = c("25", "50", "75", "100"), descrip = "ctySeq2", repcodelength = 5, numReplicates = 3, nomatchThresh = 0.1)
choro_fitCompareReplicates(baseCtySeq, ctyPlotFormats)
# 
# ## missing stratified county sequence
# baseMissSeq <- c("8a_iliSum_v2-6", "8a_iliSum_v2-6_m60", "8a_iliSum_v2-6_m80")
# missPlotFormats <- list(w = 6, h = 2, lvls = baseMissSeq[2:length(baseMissSeq)], labs = c("missing 60%", "missing 80%"), replvls = c(.25, .5, .75, 1), replabs = c("25", "50", "75", "100"), descrip = "missSeq", repcodelength = 5, numReplicates = 10, nomatchThresh = 0.5)
# choro_fitCompareReplicates(baseMissSeq, missPlotFormats)
# 
# ## missing season sequence
# baseSeasSeq <- c("8a_iliSum_v2-6", "8a_iliSum_v2-6_s6", "8a_iliSum_v2-6_s4", "8a_iliSum_v2-6_s2")
# seasPlotFormats <- list(w = 6, h = 2.25, lvls = baseSeasSeq[2:length(baseSeasSeq)], labs = c("missing 1", "missing 3","missing 5"), replvls = c(.25, .5, .75, 1), replabs = c("25", "50", "75", "100"), descrip = "seasSeq", repcodelength = 4, numReplicates = 10, nomatchThresh = 0.5)
# choro_fitCompareReplicates(baseSeasSeq, seasPlotFormats)


# ###############################################################################
# ### SCATTER - obsFit out of sample replicates ###################
# # missing county sequence
# baseCtySeq <- c("8a_iliSum_v2-6_c80", "8a_iliSum_v2-6_c60", "8a_iliSum_v2-6_c40", "8a_iliSum_v2-6_c20")
# ctyPlotFormats <- list(w = 6, h = 4, lvls = baseCtySeq[1:length(baseCtySeq)], labs = c("80% of counties", "60% of counties", "40% of counties", "20% of counties"), descrip = "ctySeq", repcodelength = 5, numReplicates = 10)
# cty_outOfSample <- scatter_obsFit_outOfSampleReplicates(baseCtySeq, ctyPlotFormats)
# cor.test(cty_outOfSample$y1_orig, cty_outOfSample$mean, method = "pearson") # cor = .7509, p-value < 2.2e-16

# # missing stratified county sequence
# baseMissSeq <- c("8a_iliSum_v2-6_m20", "8a_iliSum_v2-6_m40", "8a_iliSum_v2-6_m60", "8a_iliSum_v2-6_m80")
# missPlotFormats <- list(w = 6, h = 4, lvls = baseMissSeq[1:length(baseMissSeq)], labs = c("missing 20%", "missing 40%", "missing 60%", "missing 80%"), descrip = "missSeq", repcodelength = 5, numReplicates = 10)
# miss_outOfSample <- scatter_obsFit_outOfSampleReplicates(baseMissSeq, missPlotFormats)
# cor.test(miss_outOfSample$y1_orig, miss_outOfSample$mean, method = "pearson") # cor = .8088, p-value < 2.2e-16

# # missing season sequence
# baseSeasSeq <- c("8a_iliSum_v2-6_s6", "8a_iliSum_v2-6_s4", "8a_iliSum_v2-6_s2")
# seasPlotFormats <- list(w = 6, h = 4, lvls = baseSeasSeq[1:length(baseSeasSeq)], labs = c("missing 1", "missing 3","missing 5"), replvls = c(.25, .5, .75, 1), replabs = c("25", "50", "75", "100"), descrip = "seasSeq", repcodelength = 4, numReplicates = 10)
# seas_outOfSample <- scatter_obsFit_outOfSampleReplicates(baseSeasSeq, seasPlotFormats)
# cor.test(seas_outOfSample$y1_orig, seas_outOfSample$mean, method = "pearson") # cor = .6163, p-value < 2.2e-16

###############################################################################
# ### DOT PLOTS - Region comparison ###################
# regSeq_modCodeLs <- paste0("8a_iliSum_v2-6_R", c("1&2&3", "4&6", "5&7", "8&9&10"))
# regSeq_plotFormats <- list(w = 6, h = 5, lvls = regSeq_modCodeLs, labs = c("R1-3 BOS-NY-PHL", "R4&6 ATL-DAL", "R5&7 CHI-KS", "R8-10 DENV-SF-SEATT"), descrip = "regSeq")
# dot_coefCompare(regSeq_modCodeLs, regSeq_plotFormats)

###############################################################################
# ### DOT PLOTS - Direct comparison ###################
# repLs <- c("", paste0("-", 4))
# # compare the sequence from a single replicate against the complete model
# for(rep in repLs){
#   ctySeq <- paste0(c("8a_iliSum_v2-6_c80", "8a_iliSum_v2-6_c60", "8a_iliSum_v2-6_c40", "8a_iliSum_v2-6_c20"), rep)
#   seasSeq <- paste0(c("8a_iliSum_v2-6_s6", "8a_iliSum_v2-6_s4", "8a_iliSum_v2-6_s2"), rep)
#   missSeq <- paste0(c("8a_iliSum_v2-6_m20", "8a_iliSum_v2-6_m40", "8a_iliSum_v2-6_m60", "8a_iliSum_v2-6_m80"), rep)
# 
#   ctySeq_modCodeLs <- c("8a_iliSum_v2-6", ctySeq)
#   ctySeq_plotFormats <- list(w = 6, h = 3, lvls = ctySeq_modCodeLs, labs = c("complete", "80% of counties", "60% of counties","40% of counties", "20% of counties"), descrip = paste0("ctySeq", rep))
#   dot_coefCompare(ctySeq_modCodeLs, ctySeq_plotFormats)
# 
#   seasSeq_modCodeLs <- c("8a_iliSum_v2-6", seasSeq)
#   seasSeq_plotFormats <- list(w = 6, h = 3, lvls = seasSeq_modCodeLs, labs = c("complete", "missing 1", "missing 3", "missing 5"), descrip = paste0("seasSeq", rep))
#   dot_coefCompare(seasSeq_modCodeLs, seasSeq_plotFormats)
# 
#   missSeq_modCodeLs <- c("8a_iliSum_v2-6", missSeq)
#   missSeq_plotFormats <- list(w = 6, h = 3, lvls = missSeq_modCodeLs, labs = c("complete", "missing 20%", "missing 40%", "missing 60%", "missing 80%"), descrip = paste0("missSeq", rep))
#   dot_coefCompare(missSeq_modCodeLs, missSeq_plotFormats)
# }as.c
# 
# ageSeq_modCodeLs <- c("8a_iliSum_v2-6", "8a_iliSum_v3-6", "8a_iliSum_v4-6")
# ageSeq_plotFormats <- list(w = 6, h = 3, lvls = ageSeq_modCodeLs, labs = c("total", "children", "adults"), descrip = "ageSeq")
# dot_coefCompare(ageSeq_modCodeLs, ageSeq_plotFormats)


###############################################################################
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

###############################################################################
### BOXPLOTS - Raw predictor by region ###################
# bxp_plotFormats <- list(w = 6, h = 2)
# bxp_rawPredictors_region(path_list, bxp_plotFormats)

