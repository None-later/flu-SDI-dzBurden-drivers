## Name: Elizabeth Lee
## Date: 9/26/16
## Function: post-processing program for debug glm models, for a set of models with the same structure
## Filenames: 
## Data Source: IMS Health
## Notes: 
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
source("source_export_inlaDiagnostics.R") # plot_diag_scatter_hurdle function
source("source_clean_response_functions_cty.R") # cty response functions
source("source_omnibus_debug_glmData_cty.R") # 

#### set these! ################################
modCodeStr <- "8a_debug_v9-2"
seasons <- c(2:9)
likStrings <- c("gamma")

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
setwd(sprintf("../graph_outputs/glmModelDiagnostics/%s", modCodeStr))
path_plotExport <- getwd()
path_plotExport_coefDistr <- paste0(path_plotExport, sprintf("/coefDistr_%s_", modCodeStr))

# csv file export directories
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf("../R_export/glmModelData_export/%s", modCodeStr))
path_csvExport <- getwd()

#### results across seasons #################################
# coef distributions by season
importPlot_coefDistr_season_glm(path_csvExport, path_plotExport_coefDistr)

#### diagnostics across seasons #################################
### model fit ###
if ("gamma" %in% likStrings){
  # scatter: predicted vs. observed data (yhat - gamma) + 95%CI vs. y nonzero observed 
  path_plotExport_predVsObs <- paste0(path_plotExport, sprintf("/diag_predVsObs_%s_%s.png", "gamma", modCodeStr))
  plot_diag_scatter_glm(path_csvExport, path_plotExport_predVsObs, "gamma", "y", "mean", TRUE)
  
  # scatter: standardized residuals vs. fitted (yhat - gamma model only)
  path_plotExport_residVsYhat <- paste0(path_plotExport, sprintf("/diag_residVsYhat_%s_%s.png", "gamma", modCodeStr))
  plot_diag_scatter_glm(path_csvExport, path_plotExport_residVsYhat, "gamma", "mean", "yhat_resid", FALSE)
  
  # scatter: raw residuals vs. fitted (yhat - gamma model only)
  path_plotExport_residVsYhat2 <- paste0(path_plotExport, sprintf("/diag_rawresidVsYhat_%s_%s.png", "gamma", modCodeStr))
  plot_diag_scatter_glm(path_csvExport, path_plotExport_residVsYhat2, "gamma", "mean", "yhat_rawresid", FALSE)
  
  # scatter: standardized residuals vs. observed y_nonzero (yhat - gamma model only)
  path_plotExport_residVsObs <- paste0(path_plotExport, sprintf("/diag_residVsObs_%s_%s.png", "gamma", modCodeStr))
  plot_diag_scatter_glm(path_csvExport, path_plotExport_residVsObs, "gamma", "y_nonzero", "yhat_resid", FALSE)
  
  # scatter: raw residuals vs. observed y_nonzero (yhat - gamma model only)
  path_plotExport_residVsObs2 <- paste0(path_plotExport, sprintf("/diag_rawresidVsObs_%s_%s.png", "gamma", modCodeStr))
  plot_diag_scatter_glm(path_csvExport, path_plotExport_residVsObs2, "gamma", "y_nonzero", "yhat_rawresid", FALSE)
}

#### diagnostics by season #################################
for (s in seasons){
  print(paste("Season", s, "-----------------"))
  
  #### gamma model figures ####
  if ("gamma" %in% likStrings){
    path_csvImport_fittedGamma <- paste0(path_csvExport, sprintf("/summaryStatsFitted_%s_S%s.csv", modCodeStr, s))
    mod_gam_fitted <- read_csv(path_csvImport_fittedGamma, col_types = c("fips" = col_character(), "ID" = col_character())) %>% 
      mutate(y_nonzero = ifelse(y > 0, y, NA)) %>%
      mutate(yhat_resid = (y_nonzero-mean)/sd) %>%
      mutate(yhat_rawresid = (y_nonzero-mean)) 
    
    # choropleth: fitted values (yhat_i) - Magnitude of non-zero epidemic
    path_plotExport_yhat_gam <- paste0(path_plotExport, sprintf("/choro_yHat_%s_S%s.png", modCodeStr, s))
    plot_countyChoro(path_plotExport_yhat_gam, mod_gam_fitted, "mean", "tier", FALSE)
    
    # choropleth: SD of fitted values (yhat_i)
    path_plotExport_yhatSD_gam <- paste0(path_plotExport, sprintf("/choro_yHatSD_%s_S%s.png", modCodeStr, s))
    plot_countyChoro(path_plotExport_yhatSD_gam, mod_gam_fitted, "sd", "gradient", FALSE)
    
    # choropleth: standardized residuals 
    path_plotExport_resid_gam <- paste0(path_plotExport, sprintf("/choro_yResid_%s_S%s.png", modCodeStr, s))
    plot_countyChoro(path_plotExport_resid_gam, mod_gam_fitted, "yhat_resid", "tier", TRUE)
    
    # choropleth: raw residuals 
    path_plotExport_resid_gam2 <- paste0(path_plotExport, sprintf("/choro_yRawResid_%s_S%s.png", modCodeStr, s))
    plot_countyChoro(path_plotExport_resid_gam2, mod_gam_fitted, "yhat_rawresid", "tier", TRUE)
  }
  
}




