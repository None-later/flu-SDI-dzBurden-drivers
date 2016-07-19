
## Name: Elizabeth Lee
## Date: 7/8/16
## Function: Model 6a, v1-1 covariate & sampling effort hurdle model -- after variable selection
## v1-1: One model per season, see variables selected in 'Drivers' spreadsheet
## Filenames: physicianCoverage_IMSHealth_state.csv, dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: IMS Health
## Notes: need to SSH into snow server
## v1testing1 hurdle with sharedPredictors, no intercept
## v1testing2 hurdle with separatePredictors, no intercept
## v1testing3 hurdle w/separatePredictors, intercept
## v1-1 sharedPredictors
## v1-2 separatePredictors
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
modCodeStr <- "6a_iliSum_v1-3"; testDataOn <- FALSE
seasons <- 2:9
rdmFx_RV <- "nu"
inverseLink_nonzero <- function(x){exp(x)}
inverseLink_zero <- function(x){exp(x) / (1 + exp(x))}
dig <- 4 # number of digits in the number of elements at this spatial scale (~3000 counties -> 4 digits)

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
source("source_clean_data_functions.R") # functions to clean covariate data
source("source_prepare_inlaData_cty.R") # functions to aggregate all data sources for model
source("source_export_inlaData_cty.R") # functions to plot county-specific model diagnostics
source("source_export_inlaData.R") # functions to plot general model diagnostics

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
  formula <- Y ~ -1 + f(fips, model = "iid") + f(fips_st, model = "iid") + f(regionID, model = "iid") + intercept_zero + intercept_nonzero + O_imscoverage_zero + O_careseek_zero + X_poverty_zero + X_H3_zero + O_imscoverage_nonzero + O_careseek_nonzero + X_poverty_nonzero + X_H3_nonzero
} else{
#### Import and process data ####
  modData <- model5a_iliSum_v1(path_list) # with driver & sampling effort variables
  #### Model 6a: County-level, after variable selection, one model per season, separate predictors for the 2 likelihoods ####
  formula <- Y ~ -1 + f(fips, model = "iid") + f(fips_st, model = "iid") + f(regionID, model = "iid") + intercept_zero + O_imscoverage_zero + O_careseek_zero + O_insured_zero + X_poverty_zero + X_child_zero + X_adult_zero + X_hospaccess_zero + X_popdensity_zero + X_commute_zero + X_flight_zero + X_vaxcovI_zero + X_vaxcovE_zero + X_H3_zero + X_humidity_zero + intercept_nonzero + O_imscoverage_nonzero + O_careseek_nonzero + O_insured_nonzero + X_poverty_nonzero + X_child_nonzero + X_adult_nonzero + X_hospaccess_nonzero + X_popdensity_nonzero + X_commute_nonzero + X_flight_nonzero + X_vaxcovI_nonzero + X_vaxcovE_nonzero + X_H3_nonzero + X_humidity_nonzero + offset(logE_nonzero)
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
  # modData_full <- combine_shapefile_modelData_cty(path_list, modData, s) ## REMOVE?
  modData_full <- modData %>% filter(season == s) %>% mutate(ID = seq_along(fips))
  modData_hurdle <- convert_2stageModelData_separatePredictors(modData_full)
  
  mod <- inla(formula, 
              family = list("binomial", "gamma"), 
              data = modData_hurdle, 
              # control.family = list(list=(hyper=list(prob=list(fixed=TRUE))),list=(hyper=list(prob=list(fixed=TRUE)))), 
              Ntrials = 1, # binomial likelihood params
              control.fixed = list(mean = 0, prec = 1/100), # set prior parameters for regression coefficients
              control.predictor = list(compute = TRUE, link = 1), # compute summary statistics on fitted values, link designates that NA responses are calculated according to the second likelihood
              control.compute = list(dic = TRUE, cpo = TRUE),
              verbose = TRUE) 

  #### assign seasonal paths ################################
  path_plotExport_rdmFxSample <- paste0(path_plotExport, sprintf("/inla_%s_%s1-6_marg_S%s.png", modCodeStr, rdmFx_RV, s))
  path_plotExport_fixedFxMarginals <- paste0(path_plotExport)
  path_plotExport_diagnostics <- paste0(path_plotExport, sprintf("/diag_%s_S%s.png", modCodeStr, s))
  path_plotExport_yhat <- paste0(path_plotExport, sprintf("/choro_fitY_%s_S%s.png", modCodeStr, s))
  path_plotExport_obsY <- paste0(path_plotExport, sprintf("/choro_obsY_%s_S%s.png", modCodeStr, s))
  path_plotExport_predDBRatio <- paste0(path_plotExport, sprintf("/choro_dbRatio_%s_S%s.png", modCodeStr, s))
  path_plotExport_resid <- paste0(path_plotExport, sprintf("/choro_yResid_%s_S%s.png", modCodeStr, s))
  
  path_csvExport_ids <- paste0(path_csvExport, sprintf("/ids_%s_S%s.csv", modCodeStr, s))
  path_csvExport_summaryStats <- paste0(path_csvExport, sprintf("/summaryStats_%s_S%s.csv", modCodeStr, s))
  path_csvExport_summaryStatsFitted <- paste0(path_csvExport, sprintf("/summaryStatsFitted_%s_S%s.csv", modCodeStr, s))
  path_csvExport_dic <- paste0(path_csvExport, sprintf("/modFit_%s_S%s.csv", modCodeStr, s)) # renamed from dic_%s
  
  #### data processing ################################
  #### save DIC and CPO values in separate tables by season ####
  # DIC & CPO file formatting
  dicData <- tbl_df(data.frame(modCodeStr = c(), season = c(), exportDate = c(), DIC = c(), CPO = c(), cpoFail = c()))
  dicData <- bind_rows(dicData, list(modCodeStr = modCodeStr, season = s, exportDate = as.character(Sys.Date()), DIC = mod$dic$dic, CPO = sum(log(mod$cpo$cpo), na.rm=TRUE), cpoFail = sum(mod$cpo$failure, na.rm=TRUE)))
  

  #### calculate residuals ####
  residDf <- data.frame(y = modData_full$y, residVec = (modData_full$y - mod$summary.fitted.values$mean)/mod$summary.fitted.values$sd)
  
  #### write summary statistics ################################
  #### random and group effect identities ####
  export_ids(path_csvExport_ids, modData_full)
  
  #### fixed and random effects ####
  export_summaryStats(path_csvExport_summaryStats, mod, rdmFx_RV, modCodeStr, dbCodeStr, s) # assuming fixed, spatial, state ID, and region ID exist

  #### fitted values and residuals ####
  fittedDat <- export_summaryStats_fitted(path_csvExport_summaryStatsFitted, mod, residDf, modCodeStr, dbCodeStr, s, dig)  %>%
    select(-modCodeStr, -dbCodeStr, - season, -exportDate, -y)
  
  #### dic ####
  export_DIC(path_csvExport_dic, dicData) # dic & cpo exported by season

  #### INLA diagnostic plots ################################
  #### create full dataset for plotting ####
  plotDat <- left_join(modData_full, fittedDat, by = "ID") %>%
    mutate(dbRatio = yhat_mode/E) 
  
  #### plot a sample of posterior outputs ####
  # first 6 random effects (nu or phi) marginal posteriors (transformed)
  plot_rdmFx_marginalsSample(path_plotExport_rdmFxSample, mod$marginals.random$fips, rdmFx_RV)
  
  # fixed effects marginal posteriors
  plot_fixedFx_marginals(path_plotExport_fixedFxMarginals, mod$marginals.fixed, modCodeStr, s)
  
  #### diagnostics ####
  # 1) residuals v. fitted 2) PIT (should be uniform for good fit)
  plot_fit_diagnostics(path_plotExport_diagnostics, fittedDat, mod)
  
  #### choropleths ####
  # fitted values (yhat_i)
  plot_countyChoro(path_plotExport_yhat, plotDat, "yhat_mode", "gradient")
  
  # observations (y_i)  
  plot_countyChoro(path_plotExport_obsY, plotDat, "y", "tier")
  
  # burden ratio (yhat_i/E) 
  plot_countyChoro(path_plotExport_predDBRatio, plotDat, "dbRatio", "tier")
  
  # residuals of logyhat_i
  plot_countyChoro(path_plotExport_resid, plotDat, "yhat_resid", "tier")
  
}

# #### export model data ###
# setwd(dirname(sys.frame(1)$ofile))
# write_csv(modData_full, "testmethod_inlaData_model3b_v1.csv")