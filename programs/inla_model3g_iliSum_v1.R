
## Name: Elizabeth Lee
## Date: 2/23/16
## Function: INLA Model 3g - covariate & spatial CAR model
## Filenames: reference_data/USstate_shapefiles/US_state_adjacency_graph, dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr) # clean_data_functions dependencies
require(maptools); require(spdep) # prepare_inlaData_st.R dependencies
require(INLA)
require(RColorBrewer); require(ggplot2) # export_inlaData_st dependencies

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr <- "3g_iliSum_v1"
seasons <- 2:9
rdmFx_RV <- "phi"

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_data_functions.R") # functions to clean original data sources
source("source_prepare_inlaData_st.R") # functions to aggregate all data sources for model
source("source_export_inlaData_st.R") # functions to export data and plots related to model


#### FILEPATHS #################################
setwd('../reference_data')
path_pop_st <- paste0(getwd(), "/pop_st_Census_00-10.csv")
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")

setwd('./USstate_shapefiles')
path_shape_st <- paste0(getwd(), "/gz_2010_us_040_00_500k")
path_adjMxExport_st <- paste0(getwd(), "/US_state_adjacency.graph")

setwd("../../R_export")
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))


# put all paths in a list to pass them around in functions
path_list <- list(path_pop_st = path_pop_st,
                  path_abbr_st = path_abbr_st,
                  path_latlon_st = path_latlon_st,
                  path_shape_st = path_shape_st,
                  path_adjMxExport_st = path_adjMxExport_st,
                  path_response_st = path_response_st)


#### MAIN #################################
#### Import and process data ####
modData <- model3e_iliSum_v1(path_list) # with driver variables


#### INLA modeling ################################
# Model 3g: drivers & spatial CAR prior
formula <- logy ~ -1 + f(ID, model = "besag", graph = path_adjMxExport_st) + I(X_poverty) # v1: poverty

#### export formatting ####
# diagnostic plot export directories
setwd(dirname(sys.frame(1)$ofile))
dir.create(sprintf("../graph_outputs/inlaModelDiagnostics/%s", modCodeStr), showWarnings = FALSE)
# diagnostic plot formatting
labVec <- paste("Tier", 1:5)
colVec <- brewer.pal(length(labVec), 'RdYlGn')
# csv file export directories
setwd(dirname(sys.frame(1)$ofile))
dir.create(sprintf("../R_export/inlaModelData_export/%s", modCodeStr), showWarnings = FALSE)
setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
# csv file formatting
dicData <- tbl_df(data.frame(modCodeStr = c(), season = c(), exportDate = c(), DIC = c()))
path_csvExport_dic <- paste0(getwd(), sprintf("/dic_%s.csv", modCodeStr))


#### run models by season ####
for (s in seasons){
  modData_full <- combine_shapefile_modelData_st(path_list, modData, s)
  mod <- inla(formula, family = "gaussian", data = modData_full, 
              control.predictor = list(compute = TRUE), # compute summary statistics on fitted values
              control.compute = list(dic = TRUE),
              verbose = TRUE,
              offset = logE) # offset of expected cases
  
  #### save DIC values ####
  dicData <- bind_rows(dicData, list(modCodeStr = modCodeStr, season = s, exportDate= as.character(Sys.Date()), DIC = mod$dic$dic))
  
  #### assign seasonal paths ####
  setwd(dirname(sys.frame(1)$ofile))
  setwd(sprintf("../graph_outputs/inlaModelDiagnostics/%s", modCodeStr))
  path_plotExport_rdmFxSample <- paste0(getwd(), sprintf("/inla_%s_%s1-6_marg_S%s.png", modCodeStr, rdmFx_RV, s))
  path_plotExport_rdmFx_st <- paste0(getwd(), sprintf("/map_pred%s_%s_summary_S%s.png", rdmFx_RV, modCodeStr, s))
  path_plotExport_fixedFxMarginals <- paste0(getwd())
  path_plotExport_yhat_st <- paste0(getwd(), sprintf("/choro_fitY_%s_S%s.png", modCodeStr, s))
  path_plotExport_obsY_st <- paste0(getwd(), sprintf("/choro_obsY_%s_S%s.png", modCodeStr, s))
  path_plotExport_predDBRatio_st <- paste0(getwd(), sprintf("/choro_dbRatio_%s_S%s.png", modCodeStr, s))
  
  setwd(dirname(sys.frame(1)$ofile))
  setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
  path_csvExport_summaryStats <- paste0(getwd(), sprintf("/summaryStats_%s_S%s.csv", modCodeStr, s))
  path_csvExport_summaryStatsFitted <- paste0(getwd(), sprintf("/summaryStatsFitted_%s_S%s.csv", modCodeStr, s))
  
  
  #### process plot data ####
  # write csv of summary statistics for fitted values
  fittedDat <- export_summaryStats_fitted(path_csvExport_summaryStatsFitted, mod) %>%
    select(ID, mean, sd, mode) %>% 
    mutate(ID = as.numeric(substring(ID, 5, nchar(ID)))) %>%
    mutate(mean = exp(mean), sd = exp(sd), mode = exp(mode)) %>%
    rename(yhat_mn = mean, yhat_sd = sd, yhat_mode = mode)
  
  plotDat <- left_join(modData_full, mod$summary.random$ID, by = "ID") %>%
    rename(Predphi_mn = mean, Predphi_sd = sd, Predphi_mode = mode) %>%
    select(-contains("quant"), -kld) %>%
    left_join(fittedDat, by = "ID") %>%
    mutate(yhat_bin = cut(yhat_mode, breaks = quantile(yhat_mode, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE, include.lowest = TRUE)) %>%
    mutate(yhat_bin = factor(yhat_bin, levels = rev(levels(yhat_bin)))) %>% 
    mutate(obsY_bin = cut(y, breaks = quantile(y, probs = seq(0, 1, by = 1/5), na.rm=T), include.lowest = TRUE, ordered_result = TRUE)) %>%
    # mutate(obsY_bin = factor(obsY_bin, levels = rev(levels(obsY_bin)), labels = labVec)) %>% 
    mutate(obsY_bin = factor(obsY_bin, levels = rev(levels(obsY_bin)))) %>% 
    mutate(dbRatio = yhat_mode/E) %>%
    mutate(dbRatio_bin = cut(dbRatio, breaks = quantile(dbRatio, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE, include.lowest = TRUE)) %>%
    mutate(dbRatio_bin = factor(dbRatio_bin, levels = rev(levels(dbRatio_bin))))
  
  
  #### INLA diagnostic plots ####
  # plot marginal posteriors for first 6 random effects
  png(path_plotExport_rdmFxSample, width = 6, height = 6, units = "in", res = 300)
  par(mfrow = c(3, 2))
  plot(mod$marginals.random$ID$index.1, xlab = sprintf("%s1", rdmFx_RV))
  plot(mod$marginals.random$ID$index.2, xlab = sprintf("%s2", rdmFx_RV))
  plot(mod$marginals.random$ID$index.3, xlab = sprintf("%s3", rdmFx_RV))
  plot(mod$marginals.random$ID$index.4, xlab = sprintf("%s4", rdmFx_RV))
  plot(mod$marginals.random$ID$index.5, xlab = sprintf("%s5", rdmFx_RV))
  plot(mod$marginals.random$ID$index.6, xlab = sprintf("%s6", rdmFx_RV))
  dev.off()
  
  # plot map of random effects summary data
  plot_rdmFx_summary_map(path_plotExport_rdmFx_st, plotDat, rdmFx_RV)
  
  # plot marginal posteriors for driver coefficients
  plot_fixedFx_marginals(path_plotExport_fixedFxMarginals, mod, modCodeStr, s)
  
  # plot choropleth of fitted values (yhat_i)
  plot_state_choropleth(path_plotExport_yhat_st, plotDat, "yhat_bin", "tier")
  # plot_state_choropleth(path_plotExport_yhat_st, plotDat, "yhat_bin", "gradient")
  
  # plot choropleth of observations (y_i)  
  plot_state_choropleth(path_plotExport_obsY_st, plotDat, "obsY_bin", "tier")
  # plot_state_choropleth(path_plotExport_obsY_st, plotDat, "y", "gradient")
  
  # plot choropleth of burden ratio (mu_i/E) 
  plot_state_choropleth(path_plotExport_predDBRatio_st, plotDat, "dbRatio_bin", "tier")
  
  #### INLA summary statistics export ####
  # write csv of marginal posterior summary statistics -- random coefficients
  export_summaryStats_rdmOnly(path_csvExport_summaryStats, mod, rdmFx_RV)
  
  # write csv of summary statistics for fitted values
  export_summaryStats_fitted(path_csvExport_summaryStatsFitted, mod)
  
}

#### INLA CSV file export - across seasons ####
export_DIC(path_csvExport_dic, dicData)

## 2/3/16: Note that there are the four connected components in the adjacency matrix. The default action is to interpret this as a sum-to-zero constraint for each subgraph.  inla.doc("besag") 




