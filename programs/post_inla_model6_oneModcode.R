## Name: Elizabeth Lee
## Date: 6/21/16
## Function: post-processing program for inla_model5, for a set of models with the same structure: plot coef distributions by season (hurdle models)
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
source("source_export_inlaData.R") # plot_coefDistr_season function within importPlot_coefDistr...
source("source_export_inlaData_cty.R") # plot_countyChoro function
source("source_export_inlaData_hurdle.R") # importPlot_coefDistr_season_hurdle function
source("source_export_inlaDiagnostics.R") # plot_diag_scatter_hurdle function
source("source_clean_response_functions_cty.R") # cty response functions

#### set these! ################################
modCodeStr <- "6a_iliSum_v1testing4"
seasons <- 2:9
likStrings <- c("binomial", "gamma")

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

# csv file export directories
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
path_csvExport <- getwd()

#### results across seasons #################################
# coef distributions by season
importPlot_coefDistr_season_hurdle(path_csvExport, path_plotExport_coefDistr)

#### diagnostics across seasons #################################

### model validity ###
for (likStr in likStrings){
  # scatter: predicted vs. observed data (phat - binomial, yhat - gamma) + 95%CI vs. y observed 
  path_plotExport_predVsObs <- paste0(path_plotExport, sprintf("/diag_predVsObs_%s_%s.png", likStr, modCodeStr))
  plot_diag_scatter_hurdle(path_csvExport, path_plotExport_predVsObs, likStr, "y", "mean", TRUE)
}

### model fit ###
# scatter: residuals vs. fitted (yhat - gamma model only)
path_plotExport_residVsYhat <- paste0(path_plotExport, sprintf("/diag_residVsYhat_%s_%s.png", likStr, modCodeStr))
plot_diag_scatter_hurdle(path_csvExport, path_plotExport_residVsYhat, "gamma", "mean", "yhat_resid", FALSE)

#### diagnostics by season #################################
for (s in seasons){
  # readfile paths
  path_csvExport_fittedBinomial <- paste0(path_csvExport, sprintf("/summaryStatsFitted_binomial_%s_S%s.csv", modCodeStr, s))
  path_csvExport_fittedGamma <- paste0(path_csvExport, sprintf("/summaryStatsFitted_gamma_%s_S%s.csv", modCodeStr, s))
  
  # import data
  mod_bin_fitted <- read_csv(path_csvExport_fittedBinomial, col_types = c("fips" = col_character(), "ID" = col_character()))
  mod_gam_fitted <- read_csv(path_csvExport_fittedGamma, col_types = c("fips" = col_character(), "ID" = col_character())) %>% 
    mutate(yhat_resid = (y-mean)/sd) # create residual data (y-yhat_mean)/yhat_sd

  #### binomial model figures ####
  # choropleth: fitted values (phat_i) - Prob(non-epidemic)
  path_plotExport_phat_bin <- paste0(path_plotExport, sprintf("/choro_pHat_%s_S%s.png", modCodeStr, s))
  plot_countyChoro(path_plotExport_phat_bin, mod_bin_fitted, "mean", "gradient")
  
  # choropleth: SD of fitted values (phat_i)
  path_plotExport_phatSD_bin <- paste0(path_plotExport, sprintf("/choro_pHatSD_%s_S%s.png", modCodeStr, s))
  plot_countyChoro(path_plotExport_phatSD_bin, mod_bin_fitted, "sd", "gradient")
  
  #### gamma model figures ####
  # choropleth: fitted values (yhat_i) - Magnitude of non-zero epidemic
  path_plotExport_yhat_gam <- paste0(path_plotExport, sprintf("/choro_yHat_%s_S%s.png", modCodeStr, s))
  plot_countyChoro(path_plotExport_yhat_gam, mod_gam_fitted, "mean", "gradient")
  
  # choropleth: SD of fitted values (phat_i)
  path_plotExport_yhatSD_gam <- paste0(path_plotExport, sprintf("/choro_yHatSD_%s_S%s.png", modCodeStr, s))
  plot_countyChoro(path_plotExport_yhatSD_gam, mod_gam_fitted, "sd", "gradient")
  
  # choropleth: residuals 
  path_plotExport_resid_gam <- paste0(path_plotExport, sprintf("/choro_yResid_%s_S%s.png", modCodeStr, s))
  plot_countyChoro(path_plotExport_resid_gam, mod_gam_fitted, "yhat_resid", "gradient")
}




