
## Name: Elizabeth Lee
## Date: 10/26/16
## Function: Model 7a v5 covariate & sampling effort gamma model, add error term for each observation, county spatial structured effect 
## Filenames: physicianCoverage_IMSHealth_state.csv, dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: IMS Health
## Notes: 
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
modCodeStr <- "7a_iliSum_v5-3"; testDataOn <- FALSE
rdmFx_RV <- "phi"
dig <- 4 # number of digits in the number of elements at this spatial scale (~3000 counties -> 4 digits)
s <- 999 # all seasons code for spatiotemporal analysis = 999

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

setwd('./UScounty_shapefiles')
path_adjMxExport_cty <- paste0(getwd(), "/US_county_adjacency.graph")
path_graphIdx_cty <- paste0(getwd(), "/US_county_graph_index.csv")
path_shape_cty <- paste0(getwd(), "/gz_2010_us_050_00_500k") # for dbf metadata only

setwd("../../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_shape_cty = path_shape_cty,
                  path_adjMxExport_cty = path_adjMxExport_cty,
                  path_response_cty = path_response_cty, 
                  path_graphIdx_cty = path_graphIdx_cty)


#### MAIN #################################
#### test data module ####
if (testDataOn){
  dummy <- testing_module(path_list) # with driver & sampling effort variables
  modData <- remove_case_exceptions(dummy)
  # testing module formula
  formula <- Y ~ -1 + 
    f(fips_nonzero, model = "iid") + 
    f(fips_st_nonzero, model = "iid") + 
    f(regionID_nonzero, model = "iid") + 
    f(season_nonzero, model = "iid") +
    intercept_nonzero + O_imscoverage_nonzero + O_careseek_nonzero + X_poverty_nonzero + offset(logE_nonzero)
} else{
#### Import and process data ####
  dummy <- model7a_iliSum_v5(path_list) # with driver & sampling effort variables
  modData <- dummy %>%
    remove_randomObs_stratifySeas(0.4)
    #remove_case_exceptions(.) %>%
    #remove_gammaQQ_outliers(.)
  #### Model 6a: County-level, after variable selection, one model per season, separate predictors for the 2 likelihoods ####
  formula <- Y ~ -1 + 
    f(ID_nonzero, model = "iid") +
    f(fips_nonzero, model = "iid") + 
    f(graphIdx_nonzero, model = "besag", graph = path_adjMxExport_cty) +
    f(fips_st_nonzero, model = "iid") + 
    f(regionID_nonzero, model = "iid") + 
    f(season_nonzero, model = "iid") +
    intercept_nonzero + O_imscoverage_nonzero + O_careseek_nonzero + O_insured_nonzero + X_poverty_nonzero + X_child_nonzero + X_adult_nonzero + X_hospaccess_nonzero + X_popdensity_nonzero + X_housdensity_nonzero + X_flight_nonzero + X_vaxcovI_nonzero + X_vaxcovE_nonzero + X_H3_nonzero + X_humidity_nonzero + offset(logE_nonzero)
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
modData_full <- modData
modData_hurdle <- convert_hurdleModel_gamma_spatiotemporal(modData_full)

# starting1 <- inla(formula, 
#                   family = "gaussian", 
#                   data = modData_hurdle, 
#                   # control.family = list(link="log"),
#                   control.fixed = list(mean = 0, prec = 1/100), # set prior parameters for regression coefficients
#                   control.predictor = list(compute = TRUE, link = rep(1, nrow(modData_full))), 
#                   control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 1000, strategy = "gaussian", int.strategy = "eb"), # http://www.r-inla.org/events/newfeaturesinr-inlaapril2015; http://www.r-inla.org/?place=msg%2Fr-inla-discussion-group%2Fuf2ZGh4jmWc%2FA0rdPE5W7uMJ
#                   # E = E_nonzero,
#                   # offset = log(E_nonzero),
#                   verbose = TRUE)
# 
# starting2 <- inla(formula, 
#                   family = "gaussian", 
#                   data = modData_hurdle, 
#                   # control.family = list(link="log"),
#                   control.fixed = list(mean = 0, prec = 1/100), # set prior parameters for regression coefficients
#                   control.predictor = list(compute = TRUE, link = rep(1, nrow(modData_full))), 
#                   control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 100, strategy = "gaussian", int.strategy = "eb"), # http://www.r-inla.org/events/newfeaturesinr-inlaapril2015; http://www.r-inla.org/?place=msg%2Fr-inla-discussion-group%2Fuf2ZGh4jmWc%2FA0rdPE5W7uMJ
#                   control.mode = list(result = starting1, restart = TRUE),
#                   # E = E_nonzero,
#                   # offset = log(E_nonzero),
#                   verbose = TRUE)

starting3 <- inla(formula, 
                  family = "gaussian", 
                  data = modData_hurdle, 
                  # control.family = list(link="log"),
                  control.fixed = list(mean = 0, prec = 1/100), # set prior parameters for regression coefficients
                  control.predictor = list(compute = TRUE, link = rep(1, nrow(modData_full))), 
                  control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 10, strategy = "gaussian", int.strategy = "eb"), # http://www.r-inla.org/events/newfeaturesinr-inlaapril2015; http://www.r-inla.org/?place=msg%2Fr-inla-discussion-group%2Fuf2ZGh4jmWc%2FA0rdPE5W7uMJ
                  # control.mode = list(result = starting2, restart = TRUE),
                  # E = E_nonzero,
                  # offset = log(E_nonzero),
                  verbose = TRUE)

starting4 <- inla(formula, 
                  family = "gaussian", 
                  data = modData_hurdle, 
                  # control.family = list(link="log"), 
                  control.fixed = list(mean = 0, prec = 1/100), # set prior parameters for regression coefficients
                  control.predictor = list(compute = TRUE, link = rep(1, nrow(modData_full))), 
                  control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 1, strategy = "gaussian", int.strategy = "eb"), # http://www.r-inla.org/events/newfeaturesinr-inlaapril2015; http://www.r-inla.org/?place=msg%2Fr-inla-discussion-group%2Fuf2ZGh4jmWc%2FA0rdPE5W7uMJ
                  control.mode = list(result = starting3, restart = TRUE),
                  # E = E_nonzero,
                  # offset = log(E_nonzero),
                  verbose = TRUE)

mod <- inla(formula, 
            family = "gaussian", 
            data = modData_hurdle, 
            # control.family = list(link="log"), 
            control.fixed = list(mean = 0, prec = 1/100), # set prior parameters for regression coefficients
            control.predictor = list(compute = TRUE, link = rep(1, nrow(modData_full))),
            control.compute = list(dic = TRUE, cpo = TRUE),
            control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 0, tolerance = 1e-8), # http://www.r-inla.org/events/newfeaturesinr-inlaapril2015
            # control.mode = list(result = starting4, restart = TRUE),
            verbose = TRUE,
            keep = TRUE, debug = TRUE) 


#### model summary outputs ################################
# 7/20/16 reorganized

#### write DIC and CPO values in separate tables by season ####
# file path 
path_csvExport_dic <- paste0(path_csvExport, sprintf("/modFit_%s.csv", modCodeStr))
# DIC & CPO file formatting
dicData <- unlist(c(modCodeStr, s, as.character(Sys.Date()), mod$dic$dic, sum(log(mod$cpo$cpo), na.rm=TRUE), sum(mod$cpo$failure, na.rm=TRUE), use.names=FALSE))
dicData2 <- as.data.frame(matrix(dicData, nrow = 1), byrow = TRUE)
names(dicData2) <- c("modCodeStr", "season", "exportDate", "DIC", "CPO", "cpoFail")
# write DIC & CPO to file
export_DIC(path_csvExport_dic, dicData2) 

#### write random and group effect identities ####
# file path
path_csvExport_ids <- paste0(path_csvExport, sprintf("/ids_%s.csv", modCodeStr))
# write identity codes to file
export_ids(path_csvExport_ids, modData_full)

#### write fixed and random effects summary statistics ####
# file path
path_csvExport_summaryStats <- paste0(path_csvExport, sprintf("/summaryStats_%s.csv", modCodeStr))
# write all summary statistics to file
export_summaryStats_hurdle_gamma(path_csvExport_summaryStats, mod, rdmFx_RV, modCodeStr, dbCodeStr, s) # assuming hyperpar, fixed always exist


# #### process fitted values for each model ################################
# gamma model processing
path_csvExport_fittedGamma <- paste0(path_csvExport, sprintf("/summaryStatsFitted_gamma_%s.csv", modCodeStr))
dummy_gam <- mod$summary.fitted.values[1:nrow(modData_full),]
mod_gam_fitted <- export_summaryStats_fitted_hurdle(path_csvExport_fittedGamma, dummy_gam, modData_full, modCodeStr, dbCodeStr, s)


#### Diagnostic plots ################################

#### gamma likelihood figures ####
# marginal posteriors: first 6 county random effects (nu or phi)
path_plotExport_rdmFxSample_nonzero <- paste0(path_plotExport, sprintf("/inla_%s_%s1-6_marg_gamma.png", modCodeStr, rdmFx_RV))
plot_rdmFx_marginalsSample(path_plotExport_rdmFxSample_nonzero, mod$marginals.random$fips_nonzero, "nu")

# marginal posteriors: first 6 observation error terms 
path_plotExport_rdmFxSample_nonzero <- paste0(path_plotExport, sprintf("/inla_%s_ID1-6_marg_gamma.png", modCodeStr))
plot_rdmFx_marginalsSample(path_plotExport_rdmFxSample_nonzero, mod$marginals.random$ID_nonzero, "obs err")

# #### figures (agnostic to likelihood) ####
# # marginal posteriors: fixed effects
# path_plotExport_fixedFxMarginals <- paste0(path_plotExport)
# plot_fixedFx_marginals(path_plotExport_fixedFxMarginals, mod$marginals.fixed, modCodeStr, s)

