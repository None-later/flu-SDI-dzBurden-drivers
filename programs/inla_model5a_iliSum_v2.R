
## Name: Elizabeth Lee
## Date: 6/8/16
## Function: Model 5a, v2 covariate & sampling effort model with spatial CAR effects -- after variable selection
## v1-1: One model per season, see variables selected in 'Drivers' spreadsheet
## Filenames: physicianCoverage_IMSHealth_state.csv, dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: IMS Health
## Notes: need to SSH into snow server
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
modCodeStr <- "5a_iliSum_v2testing"; testDataOn <- TRUE
seasons <- 2:4
rdmFx_RV <- "phi"
inverseLink <- function(x){exp(x)}
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

setwd('./UScounty_shapefiles')
path_shape_cty <- paste0(getwd(), "/gz_2010_us_050_00_500k")
path_adjMxExport_cty <- paste0(getwd(), "/US_county_adjacency.graph")

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
  formula <- y ~ 1 + f(ID, model = "besag", graph = path_adjMxExport_cty) + f(stateID, model = "iid") + f(regionID, model = "iid") + O_imscoverage + O_careseek + X_poverty + X_H3
} else{
#### Import and process data ####
  modData <- model5a_iliSum_v1(path_list) # with driver & sampling effort variables
  #### Model 5a v1: County-level, after variable selection, one model per season ####
  formula <- y ~ 1 + f(ID, model = "besag", graph = path_adjMxExport_cty) + f(stateID, model = "iid") + f(regionID, model = "iid") + O_imscoverage + O_careseek + O_insured + X_poverty + X_child + X_adult + X_hospaccess + X_popdensity + X_commute + X_flight + X_H3 + X_humidity
}


#### export formatting ####
# diagnostic plot export directories
setwd(dirname(sys.frame(1)$ofile))
dir.create(sprintf("../graph_outputs/inlaModelDiagnostics/%s", modCodeStr), showWarnings = FALSE)
setwd(sprintf("../graph_outputs/inlaModelDiagnostics/%s", modCodeStr))
path_plotExport <- getwd()
path_plotExport_coefDistr <- paste0(path_plotExport, sprintf("/coefDistr_%s_", modCodeStr))

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
  modData_full <- combine_shapefile_modelData_cty(path_list, modData, s)
  mod <- inla(formula, family = "gaussian", data = modData_full, 
              control.family = list(link = "log"),
              control.fixed = list(mean = 0, prec = 1/100, mean.intercept = 0, prec.intercept = 1/100), # set prior parameters for regression coefficients and intercepts
              control.predictor = list(compute = TRUE), # compute summary statistics on fitted values
              control.compute = list(dic = TRUE, cpo = TRUE),
              verbose = TRUE,
              offset = logE) # offset (log link with Gaussian)

  #### assign seasonal paths ################################
  path_plotExport_rdmFxSample <- paste0(path_plotExport, sprintf("/inla_%s_%s1-6_marg_S%s.png", modCodeStr, rdmFx_RV, s))
  path_plotExport_fixedFxMarginals <- paste0(path_plotExport)
  path_plotExport_diagnostics <- paste0(path_plotExport, sprintf("/diag_%s_S%s.png", modCodeStr, s))
  path_plotExport_yhat <- paste0(path_plotExport, sprintf("/choro_fitY_%s_S%s.png", modCodeStr, s))
  path_plotExport_obsY <- paste0(path_plotExport, sprintf("/choro_obsY_%s_S%s.png", modCodeStr, s))
  path_plotExport_predDBRatio <- paste0(path_plotExport, sprintf("/choro_dbRatio_%s_S%s.png", modCodeStr, s))
  path_plotExport_resid <- paste0(path_plotExport, sprintf("/choro_yResid_%s_S%s.png", modCodeStr, s))
  
  path_csvExport_summaryStats <- paste0(path_csvExport, sprintf("/summaryStats_%s_S%s.csv", modCodeStr, s))
  path_csvExport_summaryStatsFitted <- paste0(path_csvExport, sprintf("/summaryStatsFitted_%s_S%s.csv", modCodeStr, s))
  path_csvExport_dic <- paste0(path_csvExport, sprintf("/modFit_%s_S%s.csv", modCodeStr, s)) # renamed from dic_%s
  
  #### data processing ################################
  #### save DIC and CPO values in separate tables by season ####
  # DIC & CPO file formatting
  dicData <- tbl_df(data.frame(modCodeStr = c(), season = c(), exportDate = c(), DIC = c(), CPO = c(), cpoFail = c()))
  dicData <- bind_rows(dicData, list(modCodeStr = modCodeStr, season = s, exportDate = as.character(Sys.Date()), DIC = mod$dic$dic, CPO = sum(log(mod$cpo$cpo), na.rm=TRUE), cpoFail = sum(mod$cpo$failure, na.rm=TRUE)))
  
  #### transform fixed and random effects back to natural scale ####
  # fixed and random effect marginals
  marg.fx.transf <- lapply(mod$marginals.fixed, function(x) inla.tmarginal(inverseLink, x))
  names(marg.fx.transf) <- ifelse(names(marg.fx.transf) == '(Intercept)', 'Intercept', names(marg.fx.transf))
  # marg.rdm.ID.transf <- lapply(mod$marginals.random$ID, function(x) inla.tmarginal(inverseLink, x))
  marg.rdm.stID.transf <- lapply(mod$marginals.random$stateID, function(x) inla.tmarginal(inverseLink, x))
  marg.rdm.regID.transf <- lapply(mod$marginals.random$regionID, function(x) inla.tmarginal(inverseLink, x))
  
  # fixed and random effect summary statistics
  summ.fx.transf <- matrix(unlist(lapply(marg.fx.transf, function(x) inla.zmarginal(x, silent = TRUE))), ncol = 7, byrow = T)
  # summ.rdm.ID.transf <- matrix(unlist(lapply(marg.rdm.ID.transf, function(x) inla.zmarginal(x, silent = TRUE))), ncol = 7, byrow = T)
  summ.rdm.stID.transf <- matrix(unlist(lapply(marg.rdm.stID.transf, function(x) inla.zmarginal(x, silent = TRUE))), ncol = 7, byrow = T)
  summ.rdm.regID.transf <- matrix(unlist(lapply(marg.rdm.regID.transf, function(x) inla.zmarginal(x, silent = TRUE))), ncol = 7, byrow = T)
  
  # combine to single list object
  # summ.stats <- list(summ.fx.transf = summ.fx.transf, summ.rdm.ID.transf = summ.rdm.ID.transf, summ.rdm.stID.transf = summ.rdm.stID.transf, summ.rdm.regID.transf = summ.rdm.regID.transf)
  summ.stats <- list(summ.fx.transf = summ.fx.transf, summ.rdm.ID.transf = 0, summ.rdm.stID.transf = summ.rdm.stID.transf, summ.rdm.regID.transf = summ.rdm.regID.transf)
  fxnames <- names(marg.fx.transf)
  
  #### calculate residuals ####
  residDf <- data.frame(y = modData_full$y, residVec = (modData_full$y - mod$summary.fitted.values$mean)/mod$summary.fitted.values$sd)
  
  #### write summary statistics ################################
  #### fixed and random effects ####
#   export_summaryStats_transformed(path_csvExport_summaryStats, summ.stats, fxnames, rdmFx_RV, modCodeStr, dbCodeStr, s) # assuming fixed, spatial, state ID, and region ID exist

  #### fitted values and residuals ####
  fittedDat <- export_summaryStats_fitted(path_csvExport_summaryStatsFitted, mod, residDf, modCodeStr, dbCodeStr, s, dig)  %>%
    select(-modCodeStr, -dbCodeStr, - season, -exportDate, -y)
  
  #### dic ####
  export_DIC(path_csvExport_dic, dicData) # dic & cpo exported by season
  
  #### create full dataset for plotting ####
  plotDat <- left_join(modData_full, fittedDat, by = "ID") %>%
    mutate(dbRatio = yhat_mode/E) 

  #### INLA diagnostic plots ################################
  
  #### plot a sample of posterior outputs ####
#   # first 6 random effects (nu or phi) marginal posteriors (transformed)
#   plot_rdmFx_marginalsSample(path_plotExport_rdmFxSample, marg.rdm.ID.transf, rdmFx_RV)
  
  # fixed effects marginal posteriors
  plot_fixedFx_marginals(path_plotExport_fixedFxMarginals, marg.fx.transf, modCodeStr, s)
  
  #### diagnostics ####
  # 1) residuals v. fitted 2) PIT (should be uniform for good fit)
  plot_fit_diagnostics(path_plotExport_diagnostics, fittedDat, mod)
  
  #### choropleths ####
  # fitted values (yhat_i)
  plot_countyChoro(path_plotExport_yhat, plotDat, "yhat_mode", "tier")
  
  # observations (y_i)  
  plot_countyChoro(path_plotExport_obsY, plotDat, "y", "tier")
  
  # burden ratio (yhat_i/E) 
  plot_countyChoro(path_plotExport_predDBRatio, plotDat, "dbRatio", "tier")
  
  # residuals of logyhat_i
  plot_countyChoro(path_plotExport_resid, plotDat, "yhat_resid", "tier")
  
}
# 
# #### Across seasons ####
# # coef distributions by season, run only if all seasons are completed
# importPlot_coefDistr_season(path_csvExport, path_plotExport_coefDistr)

# #### export model data ###
# setwd(dirname(sys.frame(1)$ofile))
# write_csv(modData_full, "testmethod_inlaData_model3b_v1.csv")