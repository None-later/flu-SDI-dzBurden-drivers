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
rm(list = ls())

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_export_inlaData.R") # functions to plot general model diagnostics

#### set these! ################################
modCodeStr <- "5a_iliSum_v1-3"

#### export formatting #################################
# diagnostic plot export directories
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf("../graph_outputs/inlaModelDiagnostics/%s", modCodeStr))
path_plotExport <- getwd()
path_plotExport_coefDistr <- paste0(path_plotExport, sprintf("/coefDistr_%s_", modCodeStr))

# csv file export directories
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
path_csvExport <- getwd()

#### plots across seasons #################################
# coef distributions by season
importPlot_coefDistr_season(path_csvExport, path_plotExport_coefDistr)

