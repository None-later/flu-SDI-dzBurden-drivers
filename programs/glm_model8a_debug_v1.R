
## Name: Elizabeth Lee
## Date: 9/26/16
## Function: Model 8a, v1 gamma GLM forr non-zero values only
## see variables selected in 'Drivers' spreadsheet
## Filenames: dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_cty.csv
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
 # main dependencies
require(RColorBrewer); require(ggplot2) # export_inlaData_st dependencies
require(broom)

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
testDataOn <- 9; modCodeStr <- sprintf("8a_debug_v%s-2", testDataOn) 
seasons <- 2:9
rdmFx_RV <- "nu"
dig <- 4 # number of digits in the number of elements at this spatial scale (~3000 counties -> 4 digits)

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
source("source_clean_data_functions.R") # functions to clean covariate data
source("source_prepare_inlaData_cty.R") # functions to aggregate all data sources for model
source("source_export_inlaData_cty.R") # functions to plot county-specific model diagnostics
source("source_export_inlaData.R") # export_ids, plot_coef_distr
# source("source_export_inlaData_hurdle.R") # data export functions for hurdle model
source("source_omnibus_debug_glmData_cty.R") # convert_glmModel_gamma, export_AIC, export_summaryStats_glm, export_summaryStats_fitted_glm, importPlot_coefDistr_season_glm

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
#### debug data modules, build up features ####
if (testDataOn == 1){
  modData <- testing_module(path_list) # response, coverage, careseek, poverty
  formula <- y ~ -1 + intercept + offset(logE)
} else if (testDataOn == 2){
  modData <- testing_module(path_list) # response, coverage, careseek, poverty
  formula <- y ~ -1 + intercept + fips_st + offset(logE)
} else if (testDataOn == 3){
  modData <- testing_module(path_list) # response, coverage, careseek, poverty
  formula <- y ~ -1 + intercept + fips_st + regionID + offset(logE)
} else if (testDataOn == 4){
  modData <- testing_module(path_list) # response, coverage, careseek, poverty
  formula <- y ~ -1 + intercept + O_imscoverage + O_careseek + X_poverty + offset(logE)
} else if (testDataOn == 5){
  modData <- testing_module(path_list) # response, coverage, careseek, poverty
  formula <- y ~ -1 + intercept + O_imscoverage + O_careseek + X_poverty + fips_st + offset(logE)
} else if (testDataOn == 6){
  modData <- testing_module(path_list) # response, coverage, careseek, poverty
  formula <- y ~ -1 + intercept + O_imscoverage + O_careseek + X_poverty + fips_st + regionID + offset(logE)
} else if (testDataOn == 7){
  dummy <- model6a_iliSum_v1(path_list) 
  modData <- remove_case_exceptions(dummy)
  formula <- y ~ -1 +
    intercept + O_imscoverage + O_careseek + O_insured + X_poverty + X_child + X_adult + X_hospaccess + X_popdensity + X_commute + X_flight + X_vaxcovI + X_vaxcovE + X_H3 + X_humidity + offset(logE)
} else if (testDataOn == 8){
  dummy <- model6a_iliSum_v1(path_list) 
  modData <- remove_case_exceptions(dummy)
  formula <- y ~ -1 +
    intercept + O_imscoverage + O_careseek + O_insured + X_poverty + X_child + X_adult + X_hospaccess + X_popdensity + X_commute + X_flight + X_vaxcovI + X_vaxcovE + X_H3 + X_humidity + 
    fips_st + offset(logE)
} else if (testDataOn == 9){
  dummy <- model6a_iliSum_v1(path_list) 
  modData <- remove_case_exceptions(dummy) 
  formula <- y ~ -1 +
    intercept + O_imscoverage + O_careseek + O_insured + X_poverty + X_child + X_adult + X_hospaccess + X_popdensity + X_commute + X_flight + X_vaxcovI + X_vaxcovE + X_H3 + X_humidity + 
   fips_st + regionID + offset(logE)
} 

#### export formatting ####
# diagnostic plot export directories
setwd(dirname(sys.frame(1)$ofile))
dir.create(sprintf("../graph_outputs/glmModelDiagnostics/%s", modCodeStr), showWarnings = FALSE)
setwd(sprintf("../graph_outputs/glmModelDiagnostics/%s", modCodeStr))
path_plotExport <- getwd()

# diagnostic plot formatting
labVec <- paste("Tier", 1:5)
colVec <- brewer.pal(length(labVec), 'RdYlGn')

# csv file export directories
setwd(dirname(sys.frame(1)$ofile))
dir.create(sprintf("../R_export/glmModelData_export/%s", modCodeStr), showWarnings = FALSE)
setwd(sprintf("../R_export/glmModelData_export/%s", modCodeStr))
path_csvExport <- getwd()

# aic dataframe
aicData <- rep(NA, length(seasons)*10)

#### run models by season ################################
for (i in 1:length(seasons)){
  s <- seasons[i]
  modData_full <- modData %>% filter(season == s) 
  modData_glm <- convert_glmModel_gamma(modData_full)
  
  mod <- glm(formula, 
              family = Gamma(link="log"), 
              data = modData_glm) 
  pred <- predict(mod, se = TRUE, type = "response") # back-transformed fitted values
  
  #### model summary outputs ################################

  #### clean AIC values ####
  aicData[((i*10)-9):(i*10)] <- unlist(c(modCodeStr, s, as.character(Sys.Date()), glance(mod)[1], glance(mod)[2], glance(mod)[3], glance(mod)[4], glance(mod)[5], glance(mod)[6], glance(mod)[7]), use.names=FALSE)

  #### write IDs ####
  # file path
  path_csvExport_ids <- paste0(path_csvExport, sprintf("/ids_%s_S%s.csv", modCodeStr, s))
  # write identity codes to file
  export_ids(path_csvExport_ids, modData_glm)
  
  #### write fixed and random effects summary statistics ####
  # file path
  path_csvExport_summaryStats <- paste0(path_csvExport, sprintf("/summaryStats_%s_S%s.csv", modCodeStr, s))
  # write all summary statistics to file
  export_summaryStats_glm(path_csvExport_summaryStats, mod, modCodeStr, dbCodeStr, s) 
  
  #### process fitted values for each model ################################
  # gamma model processing
  path_csvExport_fittedGamma <- paste0(path_csvExport, sprintf("/summaryStatsFitted_%s_S%s.csv", modCodeStr, s))
  mod_gam_fitted <- export_summaryStats_fitted_glm(path_csvExport_fittedGamma, pred, modData_glm, modCodeStr, dbCodeStr, s) 

  #### Diagnostic plots ################################
  # choropleth: observations (y_i)  
  path_plotExport_obsY <- paste0(path_plotExport, sprintf("/choro_obsY_%s_S%s.png", modCodeStr, s))
  plot_countyChoro(path_plotExport_obsY, modData_glm, "y", "tier", TRUE)
  
  # choropleth: fitted mean (yHat)  
  path_plotExport_yHat <- paste0(path_plotExport, sprintf("/choro_yHat_%s_S%s.png", modCodeStr, s))
  plot_countyChoro(path_plotExport_yHat, mod_gam_fitted, "mean", "tier", TRUE)

}

#### write AIC for all years to file #### 
path_csvExport_aic <- paste0(path_csvExport, sprintf("/modFit_%s.csv", modCodeStr))
aicData2 <- as.data.frame(matrix(aicData, nrow = length(seasons), byrow = TRUE))
names(aicData2) <- c("modCodeStr", "season", "exportDate", names(glance(mod)))
export_AIC(path_csvExport_aic, aicData2) 

# #### export model data ###
# setwd(dirname(sys.frame(1)$ofile))
# write_csv(modData_full, "testmethod_inlaData_model3b_v1.csv")
