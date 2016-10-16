## Name: Elizabeth Lee
## Date: 10/12/16
## Function: post-processing program for any inla model, for a set of models with the same structure: plot extra residual diagnostics 
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
source("source_export_inlaDiagnostics.R") # plot_diag_scatter_hurdle function, calculate_residuals
source("source_clean_response_functions_cty.R") # cty response functions
source("source_variableSelection_cty.R") # prepare_allCov_iliSum_cty/_raw

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr <- "7a_iliSum_v3-1"
seasons <- c(2:9)

#### IMPORT FILEPATHS #################################
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

#### IMPORT MODEL DATA #################################
modData <- prepare_allCov_iliSum_cty(path_list)
rmodData <- prepare_allCov_iliSum_cty_raw(path_list)

#### EXPORT FILEPATHS #################################
# diagnostic plot export directories
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf("../graph_outputs/inlaModelDiagnostics/%s", modCodeStr))
dir.create("./diag_predictors", showWarnings = FALSE)
setwd("./diag_predictors")
path_plotExport <- getwd()

# csv file export directories
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
path_csvExport <- getwd()

#### raw residuals vs raw data ####
path_plotExport_scatter_rr <- paste0(path_plotExport, "/diag_rawresid_raw")
importPlot_diag_scatter_predictors_spatiotemporal(path_csvExport, path_plotExport_scatter_rr, paste0("gamma_", modCodeStr), "yhat_rawresid", rmodData)

#### raw residuals vs std data ####
path_plotExport_scatter_rs <- paste0(path_plotExport, "/diag_rawresid_")
importPlot_diag_scatter_predictors_spatiotemporal(path_csvExport, path_plotExport_scatter_rs, paste0("gamma_", modCodeStr), "yhat_rawresid", modData)

#### std residuals vs raw data ####
path_plotExport_scatter_sr <- paste0(path_plotExport, "/diag_resid_raw")
importPlot_diag_scatter_predictors_spatiotemporal(path_csvExport, path_plotExport_scatter_sr, paste0("gamma_", modCodeStr), "yhat_resid", rmodData)

#### std residuals vs std data ####
path_plotExport_scatter_ss <- paste0(path_plotExport, "/diag_resid_")
importPlot_diag_scatter_predictors_spatiotemporal(path_csvExport, path_plotExport_scatter_ss, paste0("gamma_", modCodeStr), "yhat_resid", modData)
