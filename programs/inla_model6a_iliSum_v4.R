
## Name: Elizabeth Lee
## Date: 7/8/16
## Function: Model 6a, v4-1 covariate & sampling effort hurdle model -- after variable selection round 2 (from 6a_iliSum_v2 & 6a_iliSum_v3)
## v1-1: One model per season, see variables selected in 'Drivers' spreadsheet
## Filenames: physicianCoverage_IMSHealth_state.csv, dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: IMS Health
## Notes: need to SSH into snow server
## v4-1 (7/22/16): rm variables without at least 4 posteriors with 95% CI that deviated from 0 in the simple models (6a v2 - binomial & v3 - gamma)
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL) # clean_data_functions dependencies
require(maptools); require(spdep) # prepare_inlaData_st.R dependencies
require(INLA) # main dependencies
require(RColorBrewer); require(ggplot2) # export_inlaData_st dependencies


#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr <- "6a_iliSum_v4-2"; testDataOn <- FALSE
seasons <- 2:9
rdmFx_RV <- "nu"
dig <- 4 # number of digits in the number of elements at this spatial scale (~3000 counties -> 4 digits)

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
source("source_clean_data_functions.R") # functions to clean covariate data
source("source_prepare_inlaData_cty.R") # functions to aggregate all data sources for model
source("source_export_inlaData_cty.R") # functions to plot county-specific model diagnostics
source("source_export_inlaData.R") # functions to plot general model diagnostics
source("source_export_inlaData_hurdle.R") # data export functions for hurdle model

#### FILEPATHS #################################
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
path_adjMxExport_cty <- paste0(getwd(), "/US_county_adjacency_fips.dat")

setwd('./UScounty_shapefiles')
path_shape_cty <- paste0(getwd(), "/gz_2010_us_050_00_500k") # for dbf metadata only

setwd("../../R_export")
path_response_zip3 <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB.csv", dbCodeStr))

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_shape_cty = path_shape_cty,
                  path_adjMxExport_cty = path_adjMxExport_cty,
                  path_response_zip3 = path_response_zip3)


#### MAIN #################################
#### test data module ####
if (testDataOn){
  modData <- testing_module(path_list) # with driver & sampling effort variables
  # testing module formula
  formula <- Y ~ -1 + f(fips_bin, model = "iid") + f(fips_st_bin, model = "iid") + f(regionID_bin, model = "iid") + intercept_bin +  O_imscoverage_bin + O_careseek_bin + X_poverty_bin + X_H3_bin + f(fips_nonzero, model = "iid") + f(fips_st_nonzero, model = "iid") + f(regionID_nonzero, model = "iid") + intercept_nonzero + O_imscoverage_nonzero + O_careseek_nonzero + X_poverty_nonzero + X_H3_nonzero + offset(logE_nonzero)
} else{
#### Import and process data ####
  modData <- model5a_iliSum_v1(path_list) # with driver & sampling effort variables
  #### Model 6a: County-level, after variable selection, one model per season, separate predictors for the 2 likelihoods ####
  formula <- Y ~ -1 + f(fips_bin, model = "iid") + f(fips_st_bin, model = "iid") + f(regionID_bin, model = "iid") + intercept_bin + O_imscoverage_bin + O_careseek_bin + X_commute_bin + X_humidity_bin + f(fips_nonzero, model = "iid") + f(fips_st_nonzero, model = "iid") + f(regionID_nonzero, model = "iid") + intercept_nonzero + O_imscoverage_nonzero + offset(logE_nonzero)
}

#### export formatting ####
# diagnostic plot export directories
setwd(dirname(sys.frame(1)$ofile))
dir.create(sprintf("../graph_outputs/inlaModelDiagnostics/%s", modCodeStr), showWarnings = FALSE)
setwd(sprintf("../graph_outputs/inlaModelDiagnostics/%s", modCodeStr))
path_plotExport <- getwd()

# diagnostic plot formatting
labVec <- paste("Tier", 1:5)
colVec <- brewer.pal(length(labVec), 'RdYlGn')

# csv file export directories
setwd(dirname(sys.frame(1)$ofile))
dir.create(sprintf("../R_export/inlaModelData_export/%s", modCodeStr), showWarnings = FALSE)
setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
path_csvExport <- getwd()

#### run models by season ################################
for (s in seasons){
  modData_full <- modData %>% filter(season == s) %>% mutate(ID = seq_along(fips))
  modData_hurdle <- convert_hurdleModel_separatePredictors(modData_full)
  
  mod <- inla(formula, 
              family = list("binomial", "gamma"), 
              data = modData_hurdle, 
              control.family = list(list(link="logit"),
                                    list(link="log",
                                         hyper=list(theta=list(fixed=TRUE)))), 
              Ntrials = 1, # binomial likelihood params
              control.fixed = list(mean = 0, prec = 1/100), # set prior parameters for regression coefficients
              control.predictor = list(compute = TRUE, link = c(rep(1, nrow(modData_full)), rep(2, nrow(modData_full)))), # compute summary statistics on fitted values, link designates that NA responses are calculated according to the first likelihood for the first (nrow(modData_full)) rows & the second likelihood for the second (nrow(modData_full)) rows
              control.compute = list(dic = TRUE, cpo = TRUE),
              verbose = TRUE) 
  
  #### model summary outputs ################################
  # 7/20/16 reorganized
  
  #### write DIC and CPO values in separate tables by season ####
  # file path 
  path_csvExport_dic <- paste0(path_csvExport, sprintf("/modFit_%s_S%s.csv", modCodeStr, s))
  # DIC & CPO file formatting
  dicData <- tbl_df(data.frame(modCodeStr = c(), season = c(), exportDate = c(), DIC = c(), CPO = c(), cpoFail = c()))
  dicData <- bind_rows(dicData, list(modCodeStr = modCodeStr, season = s, exportDate = as.character(Sys.Date()), DIC = mod$dic$dic, CPO = sum(log(mod$cpo$cpo), na.rm=TRUE), cpoFail = sum(mod$cpo$failure, na.rm=TRUE)))
  # write DIC & CPO to file
  export_DIC(path_csvExport_dic, dicData) 
  
  #### write random and group effect identities ####
  # file path
  path_csvExport_ids <- paste0(path_csvExport, sprintf("/ids_%s_S%s.csv", modCodeStr, s))
  # write identity codes to file
  export_ids(path_csvExport_ids, modData_full)
  
  #### write fixed and random effects summary statistics ####
  # file path
  path_csvExport_summaryStats <- paste0(path_csvExport, sprintf("/summaryStats_%s_S%s.csv", modCodeStr, s))
  # write all summary statistics to file
  export_summaryStats_hurdle(path_csvExport_summaryStats, mod, rdmFx_RV, modCodeStr, dbCodeStr, s) # assuming fixed, spatial, state ID, and region ID exist
  
  #### hyperparameter outputs? ####
  
  
  #### process fitted values for each model ################################
  # binomial model processing
  path_csvExport_fittedBinomial <- paste0(path_csvExport, sprintf("/summaryStatsFitted_binomial_%s_S%s.csv", modCodeStr, s))
  dummy_bin <- mod$summary.fitted.values[1:nrow(modData_full),]
  mod_bin_fitted <- export_summaryStats_fitted_hurdle(path_csvExport_fittedBinomial, dummy_bin, modData_full, modCodeStr, dbCodeStr, s) 
  
  # gamma model processing
  path_csvExport_fittedGamma <- paste0(path_csvExport, sprintf("/summaryStatsFitted_gamma_%s_S%s.csv", modCodeStr, s))
  dummy_gam <- mod$summary.fitted.values[(1+nrow(modData_full)):(2*nrow(modData_full)),]
  mod_gam_fitted <- export_summaryStats_fitted_hurdle(path_csvExport_fittedGamma, dummy_gam, modData_full, modCodeStr, dbCodeStr, s)


  #### Diagnostic plots ################################
  
  #### binomial likelihood figures ####
  # marginal posteriors: first 6 random effects (nu or phi)
  path_plotExport_rdmFxSample_bin <- paste0(path_plotExport, sprintf("/inla_%s_%s1-6_marg_binomial_S%s.png", modCodeStr, rdmFx_RV, s))
  plot_rdmFx_marginalsSample(path_plotExport_rdmFxSample_bin, mod$marginals.random$fips_bin, rdmFx_RV)
  
  #### gamma likelihood figures ####
  # marginal posteriors: first 6 random effects (nu or phi)
  path_plotExport_rdmFxSample_nonzero <- paste0(path_plotExport, sprintf("/inla_%s_%s1-6_marg_gamma_S%s.png", modCodeStr, rdmFx_RV, s))
  plot_rdmFx_marginalsSample(path_plotExport_rdmFxSample_nonzero, mod$marginals.random$fips_nonzero, rdmFx_RV)
  
  #### figures (agnostic to likelihood) ####
  # marginal posteriors: fixed effects
  path_plotExport_fixedFxMarginals <- paste0(path_plotExport)
  plot_fixedFx_marginals(path_plotExport_fixedFxMarginals, mod$marginals.fixed, modCodeStr, s)
  
  # choropleth: observations (y_i)  
  path_plotExport_obsY <- paste0(path_plotExport, sprintf("/choro_obsY_%s_S%s.png", modCodeStr, s))
  plot_countyChoro(path_plotExport_obsY, modData_full, "y", "tier")
  

}

# #### export model data ###
# setwd(dirname(sys.frame(1)$ofile))
# write_csv(modData_full, "testmethod_inlaData_model3b_v1.csv")
