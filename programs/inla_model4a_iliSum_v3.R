
## Name: Elizabeth Lee
## Date: 5/9/16
## Function: Model 4a covariate & sampling effort model -- after variable selection
## v3: include variables for proxies with as much data as available for each season (no NAs)
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
modCodeStr <- "4a_iliSum_v3"
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
modData <- model4a_iliSum_v2(path_list) # with driver & sampling effort variables

#### INLA modeling ################################
# Model 4a: Covariates, sampling effort
# Covariates & sampling effort: after variable selection w/o vaxcov
seasgroup1 <- 2:5 # missing X_commute
seasgroup2 <- 6:9 # none missing

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
# DIC & CPO file formatting
dicData <- tbl_df(data.frame(modCodeStr = c(), season = c(), exportDate = c(), DIC = c(), CPO = c(), cpoFail = c()))
path_csvExport_dic <- paste0(getwd(), sprintf("/modFit_%s.csv", modCodeStr)) # renamed from dic_%s


#### run models by season ################################
for (s in seasons){
  
  #### different formulas for each season
  if (s %in% seasgroup1){
    formula <- logy ~ -1 + f(ID, model = "iid") + f(region, model = "iid") + O_imscoverage + O_careseek + O_insured + X_income + X_child + X_adult + X_hospaccess + X_flight + X_fluPos + X_H3 + X_humidity
  }
  else if (s %in% seasgroup2){
    formula <- logy ~ -1 + f(ID, model = "iid") + f(region, model = "iid") + O_imscoverage + O_careseek + O_insured + X_income + X_child + X_adult + X_hospaccess + X_commute + X_flight + X_fluPos + X_H3 + X_humidity
  }
  
  modData_full <- combine_shapefile_modelData_st(path_list, modData, s)
  mod <- inla(formula, family = "gaussian", data = modData_full, 
              control.predictor = list(compute = TRUE), # compute summary statistics on fitted values
              control.compute = list(dic = TRUE, cpo = TRUE),
              verbose = TRUE,
              offset = logE) # offset (log link with Gaussian)
  
  #### assign seasonal paths ################################
  path_plotExport_rdmFxSample <- paste0(path_plotExport, sprintf("/inla_%s_%s1-6_marg_S%s.png", modCodeStr, rdmFx_RV, s))
  path_plotExport_rdmFx_st <- paste0(path_plotExport, sprintf("/map_pred%s_%s_summary_S%s.png", rdmFx_RV, modCodeStr, s))
  path_plotExport_fixedFxMarginals <- paste0(path_plotExport)
  path_plotExport_diagnostics <- paste0(path_plotExport, sprintf("/diag_%s_S%s.png", modCodeStr, s))
  path_plotExport_yhat_st <- paste0(path_plotExport, sprintf("/choro_fitY_%s_S%s.png", modCodeStr, s))
  path_plotExport_obsY_st <- paste0(path_plotExport, sprintf("/choro_obsY_%s_S%s.png", modCodeStr, s))
  path_plotExport_predDBRatio_st <- paste0(path_plotExport, sprintf("/choro_dbRatio_%s_S%s.png", modCodeStr, s))
  path_plotExport_resid_st <- paste0(path_plotExport, sprintf("/choro_logyResid_%s_S%s.png", modCodeStr, s))
  
  path_csvExport_summaryStats <- paste0(path_csvExport, sprintf("/summaryStats_%s_S%s.csv", modCodeStr, s))
  path_csvExport_summaryStatsFitted <- paste0(path_csvExport, sprintf("/summaryStatsFitted_%s_S%s.csv", modCodeStr, s))
  
  #### data processing ################################
  #### save DIC and CPO values ####
  dicData <- bind_rows(dicData, list(modCodeStr = modCodeStr, season = s, exportDate = as.character(Sys.Date()), DIC = mod$dic$dic, CPO = sum(log(mod$cpo$cpo), na.rm=TRUE), cpoFail = sum(mod$cpo$failure, na.rm=TRUE)))
  
  #### calculate residuals ####
  residDf <- data.frame(logy = modData_full$logy, residVec = (modData_full$logy - mod$summary.fitted.values$mean)/mod$summary.fitted.values$sd)
  #### write summary statistics for fitted values & residuals to file ###
  fittedDat <- export_summaryStats_fitted(path_csvExport_summaryStatsFitted, mod, residDf, modCodeStr, dbCodeStr, s)  %>%
    select(-modCodeStr, -dbCodeStr, - season, -exportDate, -logy)
  
  #### create full dataset for plotting ####
  plotDat <- left_join(modData_full, mod$summary.random$ID, by = "ID") %>%
    rename(Prednu_mn = mean, Prednu_sd = sd, Prednu_mode = mode) %>%
    select(-contains("quant"), -kld) %>%
    left_join(fittedDat, by = "ID") %>%
    mutate(yhat_bin = cut(yhat_mode, breaks = quantile(yhat_mode, probs = seq(0, 1, by = 1/3), na.rm=T), ordered_result = TRUE, include.lowest = TRUE)) %>%
    mutate(yhat_bin = factor(yhat_bin, levels = rev(levels(yhat_bin)))) %>% 
    mutate(obsY_bin = cut(y, breaks = quantile(y, probs = seq(0, 1, by = 1/3), na.rm=T), include.lowest = TRUE, ordered_result = TRUE)) %>%
    # mutate(obsY_bin = factor(obsY_bin, levels = rev(levels(obsY_bin)), labels = labVec)) %>% 
    mutate(obsY_bin = factor(obsY_bin, levels = rev(levels(obsY_bin)))) %>% 
    mutate(dbRatio = yhat_mode/E) %>%
    mutate(dbRatio_bin = cut(dbRatio, breaks = quantile(dbRatio, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE, include.lowest = TRUE)) %>%
    mutate(dbRatio_bin = factor(dbRatio_bin, levels = rev(levels(dbRatio_bin)))) %>%
    mutate(logyhatResid_bin = cut(logyhat_resid, breaks = quantile(logyhat_resid, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE, include.lowest = TRUE)) %>%
    mutate(logyhatResid_bin = factor(logyhatResid_bin, levels = rev(levels(logyhatResid_bin))))
  
  #### INLA diagnostic plots ################################
  
  #### plot a sample of posterior outputs ####
  # first 6 random effects (nu or phi) marginal posteriors
  png(path_plotExport_rdmFxSample, width = 6, height = 6, units = "in", res = 200)
  par(mfrow = c(3, 2))
  plot(mod$marginals.random$ID$index.1, xlab = sprintf("%s1", rdmFx_RV))
  plot(mod$marginals.random$ID$index.2, xlab = sprintf("%s2", rdmFx_RV))
  plot(mod$marginals.random$ID$index.3, xlab = sprintf("%s3", rdmFx_RV))
  plot(mod$marginals.random$ID$index.4, xlab = sprintf("%s4", rdmFx_RV))
  plot(mod$marginals.random$ID$index.5, xlab = sprintf("%s5", rdmFx_RV))
  plot(mod$marginals.random$ID$index.6, xlab = sprintf("%s6", rdmFx_RV))
  dev.off()
  
  # summary of random effects
  plot_rdmFx_summary_map(path_plotExport_rdmFx_st, plotDat, rdmFx_RV)
  
  # fixed effects marginal posteriors
  plot_fixedFx_marginals(path_plotExport_fixedFxMarginals, mod, modCodeStr, s)
  
  #### diagnostics ####
  # 1) residuals v. fitted 2) PIT (should be uniform for good fit)
  png(path_plotExport_diagnostics, width = 6, height = 3, units = "in", res = 200)
  par(mfrow = c(1, 2))
  plot(fittedDat$mn, fittedDat$logyhat_resid, xlim = c(floor(min(fittedDat$mn[which(!is.na(fittedDat$logyhat_resid))])), ceiling(max(fittedDat$mn[which(!is.na(fittedDat$logyhat_resid))]))), xlab = "Fitted value mean (logyhat)", ylab = "Residuals")
  abline(h = 0)
  hist(mod$cpo$pit, breaks = 10, main = "", xlab = "Probability integral transform (PIT)")
  dev.off()
  
  #### choropleths ####
  # fitted values (yhat_i)
  plot_state_choropleth(path_plotExport_yhat_st, plotDat, "yhat_bin", "tier")
  # plot_state_choropleth(path_plotExport_yhat_st, plotDat, "yhat_bin", "gradient")
  
  # observations (y_i)  
  plot_state_choropleth(path_plotExport_obsY_st, plotDat, "obsY_bin", "tier")
  # plot_state_choropleth(path_plotExport_obsY_st, plotDat, "y", "gradient")
  
  # burden ratio (mu_i/E) 
  plot_state_choropleth(path_plotExport_predDBRatio_st, plotDat, "dbRatio_bin", "tier")
  
  # residuals of logyhat_i
  plot_state_choropleth(path_plotExport_resid_st, plotDat, "logyhatResid_bin", "tier")
  
  #### INLA summary statistics export ####
  # write csv of marginal posterior summary statistics -- fixed and random coefficients
  export_summaryStats(path_csvExport_summaryStats, mod, rdmFx_RV, modCodeStr, dbCodeStr, s)

  # write csv of summary statistics for fitted values
  # see fittedDat above
  }

#### Across seasons ####
# INLA csv file export
export_DIC(path_csvExport_dic, dicData) # dic & cpo

# coef distributions by season
plot_coefDistr_season(path_csvExport, path_plotExport_coefDistr)

# #### export model data ###
# setwd(dirname(sys.frame(1)$ofile))
# write_csv(modData_full, "testmethod_inlaData_model3b_v1.csv")


