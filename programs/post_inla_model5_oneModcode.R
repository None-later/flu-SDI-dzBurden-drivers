## Name: Elizabeth Lee
## Date: 6/17/16
## Function: post-processing program for inla_model5, for a set of models with the same structure: plot coef distributions by season
## Filenames: 
## Data Source: IMS Health
## Notes: need to SSH into snow server
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
require(readr)

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_export_inlaData.R") # functions to plot general model results
source("source_export_inlaDiagnostics.R") # functions to plot general model diagnostics (plot_diag_scatter)
source("source_clean_response_functions_cty.R") # cty response functions
source("source_export_inlaData_cty.R") # plot_countyChoro

#### set these! ################################
modCodeStr <- "5a_iliSum_v1-5"
seasons <- c(2:9)

#### IMPORT FILEPATHS #################################
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty)

#### EXPORT FILEPATHS #################################
# diagnostic plot export directories
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf("../graph_outputs/inlaModelDiagnostics/%s", modCodeStr))
path_plotExport <- getwd()
path_plotExport_coefDistr <- paste0(path_plotExport, sprintf("/coefDistr_%s_", modCodeStr))
path_plotExport_predVsObs <- paste0(path_plotExport, sprintf("/diag_predVsObs_%s.png", modCodeStr))
path_plotExport_predVsRaw <- paste0(path_plotExport, sprintf("/diag_predVsRaw_%s_", modCodeStr))
path_plotExport_residVsYhat <- paste0(path_plotExport, sprintf("/diag_residVsYhat_%s_%s.png", "gamma", modCodeStr))
path_plotExport_residVsYhat2 <- paste0(path_plotExport, sprintf("/diag_rawresidVsYhat_%s_%s.png", "gamma", modCodeStr))


# csv file export directories
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
path_csvExport <- getwd()

#### results across seasons #################################
# coef distributions by season
importPlot_coefDistr_season(path_csvExport, path_plotExport_coefDistr)

#### diagnostics across seasons #################################

### model validity ###
# predicted vs. observed data
plot_diag_scatter(path_csvExport, path_plotExport_predVsObs, "y", "mean", TRUE)
# predicted vs. raw ili count data
importPlot_diag_predVsRaw(path_csvExport, path_plotExport_predVsRaw, path_list)
# standardized resid vs. fitted
plot_diag_scatter(path_csvExport, path_plotExport_residVsYhat, "mean", "yhat_resid", FALSE)
# raw resid vs. fitted
plot_diag_scatter(path_csvExport, path_plotExport_residVsYhat2, "mean", "yhat_rawresid", FALSE)


#### diagnostics by season #################################
for (s in seasons){
  print(paste("Season", s, "-----------------"))

  ## map county random effect error terms - check for spatial clustering ##
  path_csvImport_estimates <- paste0(path_csvExport, sprintf("/summaryStats_%s_S%s.csv", modCodeStr, s))
  mod_est <- read_csv(path_csvImport_estimates, col_types = c("RV" = col_character())) 
  
  path_plotExport_ctyEffects <- paste0(path_plotExport, sprintf("/choro_spatialEffect_%s_S%s.png", modCodeStr, s))
  mod_est_ctyEffects <- mod_est %>% 
    filter(effectType == "spatial") %>%
    rename(fips = RV)
  plot_countyChoro(path_plotExport_ctyEffects, mod_est_ctyEffects, "q_5", "gradient", FALSE)
  
  ## map fitted values ##
  path_csvImport_fitted <- paste0(path_csvExport, sprintf("/summaryStatsFitted_%s_S%s.csv", modCodeStr, s))
  mod_fitted <- read_csv(path_csvImport_fitted, col_types = c("fips" = col_character(), "ID" = col_character())) %>% 
    mutate(yhat_resid = (y-mean)/sd) %>% # create residual data (y-yhat_mean)/yhat_sd
    mutate(yhat_rawresid = (y-mean))
  
  # choropleth: fitted values (yhat_i) - Magnitude of non-zero epidemic
  path_plotExport_yhat <- paste0(path_plotExport, sprintf("/choro_yHat_%s_S%s.png", modCodeStr, s))
  plot_countyChoro(path_plotExport_yhat, mod_fitted, "mean", "tier", FALSE)
  
  # choropleth: SD of fitted values (yhat_i)
  path_plotExport_yhatSD <- paste0(path_plotExport, sprintf("/choro_yHatSD_%s_S%s.png", modCodeStr, s))
  plot_countyChoro(path_plotExport_yhatSD, mod_fitted, "sd", "gradient", FALSE)
  
  # choropleth: standardized residuals 
  path_plotExport_resid <- paste0(path_plotExport, sprintf("/choro_yResid_%s_S%s.png", modCodeStr, s))
  plot_countyChoro(path_plotExport_resid, mod_fitted, "yhat_resid", "tier", TRUE)
  # choropleth: raw residuals 
  path_plotExport_resid2 <- paste0(path_plotExport, sprintf("/choro_yRawResid_%s_S%s.png", modCodeStr, s))
  plot_countyChoro(path_plotExport_resid2, mod_fitted, "yhat_rawresid", "tier", TRUE)
  
}



