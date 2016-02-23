
## Name: Elizabeth Lee
## Date: 2/10/16
## Function: Model 3b sampling effort model
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
modCodeStr <- "3b_iliSum_v1"
seasons <- 2:9
rdmFx_RV <- "nu"


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
path_imsCov_st <- paste0(getwd(), "/physicianCoverage_IMSHealth_state.csv")

# put all paths in a list to pass them around in functions
path_list <- list(path_pop_st = path_pop_st,
                  path_abbr_st = path_abbr_st,
                  path_latlon_st = path_latlon_st,
                  path_shape_st = path_shape_st,
                  path_adjMxExport_st = path_adjMxExport_st,
                  path_response_st = path_response_st, 
                  path_imsCov_st = path_imsCov_st)


#### MAIN #################################
#### Import and process data ####
modData <- model3b_iliSum_v1(path_list) # with sampling effort variables


#### INLA modeling ################################
# Model 3b: sampling effort
# Sampling effort & unstructured spatial random effect
formula <- logy ~ -1 + f(ID, model = "iid") + I(O_imscoverage) + I(O_careseek) + I(O_insured)


#### create export directories ####
# diagnostic plot outputs
setwd(dirname(sys.frame(1)$ofile))
dir.create(sprintf("../graph_outputs/inlaModelDiagnostics/%s", modCodeStr), showWarnings = FALSE)

# csv file outputs
setwd(dirname(sys.frame(1)$ofile))
dir.create(sprintf("../R_export/inlaModelData_export/%s", modCodeStr), showWarnings = FALSE)


#### plot formatting ####
labVec <- paste("Tier", 1:5)
colVec <- brewer.pal(length(labVec), 'RdYlGn')


#### run models by season ####
for (s in seasons){
  modData_full <- combine_shapefile_modelData_st(path_list, modData, s)
  mod <- inla(formula, family = "gaussian", data = modData_full, 
              control.predictor = list(compute = TRUE), # compute summary statistics on fitted values
              control.compute = list(dic = TRUE),
              verbose = TRUE,
              offset = logE) # offset (log link with Gaussian)
 
  #### assign seasonal paths ####
  setwd(dirname(sys.frame(1)$ofile))
  setwd(sprintf("../graph_outputs/inlaModelDiagnostics/%s", modCodeStr))
  path_plotExport_rdmFxSample <- paste0(getwd(), sprintf("/inla_%s_%s1-6_marg_S%s.png", modCodeStr, rdmFx_RV, s))
  path_plotExport_rdmFx_st <- paste0(getwd(), sprintf("/map_pred%s_%s_summary_S%s.png", rdmFx_RV, modCodeStr, s))
  path_plotExport_fixedFxMarginals <- paste0(getwd())
  path_plotExport_predMu_st <- paste0(getwd(), sprintf("/choro_predMu_%s_S%s.png", modCodeStr, s))
  path_plotExport_obsY_st <- paste0(getwd(), sprintf("/choro_obsY_%s_S%s.png", modCodeStr, s))
  path_plotExport_predDBRatio_st <- paste0(getwd(), sprintf("/choro_dbRatio_%s_S%s.png", modCodeStr, s))
  
  
  setwd(dirname(sys.frame(1)$ofile))
  setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
  path_csvExport_summaryStats <- paste0(getwd(), sprintf("/summaryStats_%s_S%s.csv", modCodeStr, s))
  
  
  #### process plot data ####
  plotDat <- left_join(modData_full, mod$summary.random$ID, by = "ID") %>%
    rename(Prednu_mn = mean, Prednu_sd = sd, Prednu_mode = mode) %>%
    select(-contains("quant"), -kld) %>%
    mutate(predMu = exp(logE + Prednu_mode)) %>%
    mutate(predMu_bin = cut(predMu, breaks = quantile(predMu, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE, include.lowest = TRUE)) %>%
    mutate(predMu_bin = factor(predMu_bin, levels = rev(levels(predMu_bin)))) %>% 
    mutate(obsY_bin = cut(y, breaks = quantile(y, probs = seq(0, 1, by = 1/5), na.rm=T), include.lowest = TRUE, ordered_result = TRUE)) %>%
    mutate(obsY_bin = factor(obsY_bin, levels = rev(levels(obsY_bin)))) %>% 
    mutate(dbRatio = predMu/E) %>%
    mutate(dbRatio_bin = cut(dbRatio, breaks = quantile(dbRatio, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE, include.lowest = TRUE)) %>%
    mutate(dbRatio_bin = factor(dbRatio_bin, levels = rev(levels(dbRatio_bin))))
 
  
  #### INLA diagnostic plots ####
  # plot marginal posteriors for first 6 random effects (nu or phi)
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
  
  # plot marginal posteriors for fixed effects
  plot_fixedFx_marginals(path_plotExport_fixedFxMarginals, mod, modCodeStr, s)
  
  # plot choropleth of predictions (mu_i)
  plot_state_choropleth(path_plotExport_predMu_st, plotDat, "predMu_bin", "tier")
  # plot_state_choropleth(path_plotExport_predMu_st, plotDat, "predMu", "gradient")
  
  # plot choropleth of observations (y_i)  
  plot_state_choropleth(path_plotExport_obsY_st, plotDat, "obsY_bin", "tier")
  # plot_state_choropleth(path_plotExport_obsY_st, plotDat, "y", "gradient")
  
  # plot choropleth of burden ratio (mu_i/E) 
  plot_state_choropleth(path_plotExport_predDBRatio_st, plotDat, "dbRatio_bin", "tier")
  
  #### INLA summary statistics export ####
  # write csv of marginal posterior summary statistics -- fixed and random coefficients
  export_summaryStats(path_csvExport_summaryStats, mod, rdmFx_RV)
}

# #### export model data ###
# setwd(dirname(sys.frame(1)$ofile))
# write_csv(modData_full, "testmethod_inlaData_model3b_v1.csv")


