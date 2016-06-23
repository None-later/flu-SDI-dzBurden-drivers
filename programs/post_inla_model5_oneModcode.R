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
source("source_export_inlaDiagnostics.R") # functions to plot general model diagnostics
source("source_clean_response_functions_cty.R") # cty response functions

#### set these! ################################
modCodeStr <- "5b_iliPeak_v1-2"

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
plot_diag_predVsObs(path_csvExport, path_plotExport_predVsObs)
# predicted vs. raw ili count data
importPlot_diag_predVsRaw(path_csvExport, path_plotExport_predVsRaw, path_list)


