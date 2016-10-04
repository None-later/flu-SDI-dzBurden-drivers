
## Name: Elizabeth Lee
## Date: 7/22/16
## Function: Model 6a, v2 binomial component of hurdle model
## v2-1: all iid random effects
## v2-2: county CAR, others iid random effects
## v2-3: all CAR random effects 
## see variables selected in 'Drivers' spreadsheet
## Filenames: physicianCoverage_IMSHealth_state.csv, dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: IMS Health
## Notes: need to SSH into snow server
## forked from model 6a v1-4

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
modCodeStr <- "6a_iliSum_v2-6"; testDataOn <- FALSE
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
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_shape_cty = path_shape_cty,
                  path_adjMxExport_cty = path_adjMxExport_cty,
                  path_response_cty = path_response_cty)

#### MAIN #################################
#### test data module ####
if (testDataOn){
  modData <- testing_module(path_list) # with driver & sampling effort variables
  # testing module formula
  formula <- Y ~ -1 + 
    f(fips_bin, model = "iid") + 
    f(fips_st_bin, model = "iid") + 
    f(regionID_bin, model = "iid") + 
    intercept_bin +  O_imscoverage_bin + O_careseek_bin + X_poverty_bin + X_H3_bin
} else{
#### Import and process data ####
  dummy <- model6a_iliSum_v2(path_list) # with driver & sampling effort variables
  modData <- remove_case_exceptions(dummy)
  formula <- Y ~ -1 + 
    f(fips_bin, model = "iid") + 
    f(fips_st_bin, model = "iid") + 
    f(regionID_bin, model = "iid") + 
    intercept_bin + O_imscoverage_bin + O_careseek_bin + O_insured_bin + X_poverty_bin + X_child_bin + X_adult_bin + X_hospaccess_bin + X_popdensity_bin + X_housdensity_bin + X_flight_bin + X_vaxcovI_bin + X_vaxcovE_bin + X_H3_bin + X_humidity_bin 
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

# dic dataframe
dicData <- rep(NA, length(seasons)*6)

#### run models by season ################################
for (i in 1:length(seasons)){
  s <- seasons[i]
  modData_full <- modData %>% filter(season == s) %>% mutate(ID = seq_along(fips))
  modData_hurdle <- convert_hurdleModel_binomial(modData_full)
  
  starting1 <- inla(formula, 
                    family = "binomial", 
                    data = modData_hurdle, 
                    control.family = list(link="logit"), 
                    Ntrials = 1, # binomial likelihood params
                    control.fixed = list(mean = 0, prec = 1), # set prior parameters for regression coefficients
                    control.predictor = list(compute = TRUE, link = rep(1, nrow(modData_full))), 
                    control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 1000, strategy = "gaussian", int.strategy = "eb"), # http://www.r-inla.org/events/newfeaturesinr-inlaapril2015; http://www.r-inla.org/?place=msg%2Fr-inla-discussion-group%2Fuf2ZGh4jmWc%2FA0rdPE5W7uMJ
                    verbose = TRUE)
  
  starting2 <- inla(formula, 
                    family = "binomial", 
                    data = modData_hurdle, 
                    control.family = list(link="logit"), 
                    Ntrials = 1, # binomial likelihood params
                    control.fixed = list(mean = 0, prec = 1), # set prior parameters for regression coefficients
                    control.predictor = list(compute = TRUE, link = rep(1, nrow(modData_full))), 
                    control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 100, strategy = "gaussian", int.strategy = "eb"), # http://www.r-inla.org/events/newfeaturesinr-inlaapril2015; http://www.r-inla.org/?place=msg%2Fr-inla-discussion-group%2Fuf2ZGh4jmWc%2FA0rdPE5W7uMJ
                    control.mode = list(result = starting1, restart = TRUE),
                    verbose = TRUE)
  
  starting3 <- inla(formula, 
                    family = "binomial", 
                    data = modData_hurdle, 
                    control.family = list(link="logit"),
                    Ntrials = 1, # binomial likelihood params
                    control.fixed = list(mean = 0, prec = 1), # set prior parameters for regression coefficients
                    control.predictor = list(compute = TRUE, link = rep(1, nrow(modData_full))), 
                    control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 10, strategy = "gaussian", int.strategy = "eb"), # http://www.r-inla.org/events/newfeaturesinr-inlaapril2015; http://www.r-inla.org/?place=msg%2Fr-inla-discussion-group%2Fuf2ZGh4jmWc%2FA0rdPE5W7uMJ
                    control.mode = list(result = starting2, restart = TRUE),
                    verbose = TRUE)
  
  starting4 <- inla(formula, 
                    family = "binomial", 
                    data = modData_hurdle, 
                    control.family = list(link="logit"),  
                    Ntrials = 1, # binomial likelihood params
                    control.fixed = list(mean = 0, prec = 1), # set prior parameters for regression coefficients
                    control.predictor = list(compute = TRUE, link = rep(1, nrow(modData_full))), 
                    control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 1, strategy = "gaussian", int.strategy = "eb"), # http://www.r-inla.org/events/newfeaturesinr-inlaapril2015; http://www.r-inla.org/?place=msg%2Fr-inla-discussion-group%2Fuf2ZGh4jmWc%2FA0rdPE5W7uMJ
                    control.mode = list(result = starting3, restart = TRUE),
                    verbose = TRUE)
  
  mod <- inla(formula, 
              family = "binomial", 
              data = modData_hurdle, 
              control.family = list(link="logit"), 
              Ntrials = 1, # binomial likelihood params
              control.fixed = list(mean = 0, prec = 1), # set prior parameters for regression coefficients
              control.predictor = list(compute = TRUE, link = rep(1, nrow(modData_full))), 
              control.compute = list(dic = TRUE, cpo = TRUE),
              control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 0, tolerance = 1e-6),
              control.mode = list(result = starting4, restart = TRUE),
              verbose = TRUE,
              keep = TRUE, debug = TRUE) 
  
  #### model summary outputs ################################
  # 7/20/16 reorganized
  
  #### clean DIC and CPO values ####
  dicData[((i*6)-5):(i*6)] <- unlist(c(modCodeStr, s, as.character(Sys.Date()), mod$dic$dic, sum(log(mod$cpo$cpo), na.rm=TRUE), sum(mod$cpo$failure, na.rm=TRUE), use.names=FALSE))
  
  
  #### write random and group effect identities ####
  # file path
  path_csvExport_ids <- paste0(path_csvExport, sprintf("/ids_%s_S%s.csv", modCodeStr, s))
  # write identity codes to file
  export_ids(path_csvExport_ids, modData_full)
  
  
  #### write fixed and random effects and hyperpar summary statistics ####
  # file path
  path_csvExport_summaryStats <- paste0(path_csvExport, sprintf("/summaryStats_%s_S%s.csv", modCodeStr, s))
  # write all summary statistics to file
  export_summaryStats_hurdle_binomial(path_csvExport_summaryStats, mod, rdmFx_RV, modCodeStr, dbCodeStr, s) # assuming hyperpar, fixed always exist

  
  #### process fitted values for each model ################################
  # binomial model processing
  path_csvExport_fittedBinomial <- paste0(path_csvExport, sprintf("/summaryStatsFitted_binomial_%s_S%s.csv", modCodeStr, s))
  dummy_bin <- mod$summary.fitted.values[1:nrow(modData_full),]
  mod_bin_fitted <- export_summaryStats_fitted_hurdle(path_csvExport_fittedBinomial, dummy_bin, modData_full, modCodeStr, dbCodeStr, s) 

  
  #### Diagnostic plots ################################
  #### binomial likelihood figures ####
  if (!is.null(mod$marginals.random$fips_bin)){
    # marginal posteriors: first 6 random effects (nu or phi)
    path_plotExport_rdmFxSample_bin <- paste0(path_plotExport, sprintf("/inla_%s_%s1-6_marg_binomial_S%s.png", modCodeStr, rdmFx_RV, s))
    plot_rdmFx_marginalsSample(path_plotExport_rdmFxSample_bin, mod$marginals.random$fips_bin, rdmFx_RV)
  }
  
#   #### figures (agnostic to likelihood) ####
#   # marginal posteriors: fixed effects
#   path_plotExport_fixedFxMarginals <- paste0(path_plotExport)
#   plot_fixedFx_marginals(path_plotExport_fixedFxMarginals, mod$marginals.fixed, modCodeStr, s)
  
  # choropleth: observations (y_i)  
  path_plotExport_obsY <- paste0(path_plotExport, sprintf("/choro_obsY_%s_S%s.png", modCodeStr, s))
  plot_countyChoro(path_plotExport_obsY, modData_full, "y", "tier", TRUE)
  
}

#### write DIC for all years to file #### 
path_csvExport_dic <- paste0(path_csvExport, sprintf("/modFit_%s.csv", modCodeStr))
dicData2 <- as.data.frame(matrix(dicData, nrow = length(seasons), byrow = TRUE))
names(dicData2) <- c("modCodeStr", "season", "exportDate", "DIC", "CPO", "cpoFail")

# write DIC & CPO to file
export_DIC(path_csvExport_dic, dicData2) 

# # #### export model data ###
# # setwd(dirname(sys.frame(1)$ofile))
# # write_csv(modData_full, "testmethod_inlaData_model3b_v1.csv")
