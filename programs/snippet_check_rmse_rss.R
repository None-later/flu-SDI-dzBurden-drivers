## Name: Elizabeth Lee
## Date: 5/16/17
## Function: calculate rss and rmse as compared to observations
## Filenames: 
## Data Source: IMS Health
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
require(readr)
require(tidyr); require(dplyr)

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_export_inlaData.R") # plot_coefDistr_season function within importPlot_coefDistr...
source("source_export_inlaData_cty.R") # plot_countyChoro function
source("source_export_inlaData_hurdle.R") # importPlot_coefDistr_season_hurdle function
source("source_export_inlaDiagnostics.R") # plot_diag_scatter_hurdle function
source("source_clean_response_functions_cty.R") # cty response functions

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2" # _ilinDt_Octfit_span0.4_degree2
seasons <- c(3:9)

modCodeStrLs <- c("8a_iliSum_v2-6", "8a_iliSum_v1-5", "8a_iliSum_v2-6_rmStFx", "8a_iliSum_v2-6_rmRegFx", "8a_iliSum_v2-6_rmSeasFx", "8a_iliSum_v2-6_rmCtyFx", "8a_iliSum_v2-6_rmFixed")

likString <- "normal"
likStrings <- c(paste0(likString, "_", modCodeStrLs))
source("source_calculate_residuals_shift1.R") # calculate_residuals function (source_calculate_residuals_shift1.R for iliSum; source_calculate_residuals.R for epiDur, wksToEpi)

#### FUNCTIONS #################################

calculate_rss_rmse <- function(path_csvExport, likelihoodString){
  # calculate residual sum of squares and residual mean square error for 8a V2-6 diagnostic models (fitted vs observed)
  print(match.call())
  
  #### import fitted values ####
  setwd(path_csvExport)
  readfile_list <- grep(sprintf("summaryStatsFitted_%s", likelihoodString), list.files(), value = TRUE)
  fitDat <- tbl_df(data.frame())
  
  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "ccd_ccdddddddd")
    fitDat <- bind_rows(fitDat, seasFile)
  }
  names(fitDat) <- c("modCodeStr", "dbCodeStr", "season", "fips", "ID", "mean", "sd", "q_025", "q_5", "q_975", "mode", "y", "y1")
  
  #### import id crosswalk ####
  readfile_list2 <- grep("ids_", list.files(), value = TRUE)
  idDat <- tbl_df(data.frame())
  
  for (infile2 in readfile_list2){
    seasFile2 <- read_csv(infile2, col_types = cols_only(season = "d", fips = "c", st = "c", regionID = "d"))
    idDat <- bind_rows(idDat, seasFile2)
  }
  
  #### merge data ####
  plotDat <- left_join(fitDat, idDat, by = c("season", "fips")) %>%
    mutate(season = as.factor(as.integer(season))) %>%
    mutate(regionID = as.factor(as.integer(regionID))) %>% 
    calculate_residuals(TRUE)

  #### calculate ssr & rmse ####
  ncount <- nrow(plotDat %>% select(yhat_rawresid) %>% filter(!is.na(yhat_rawresid)))
  calcDat <- plotDat %>% 
    # rowwise %>% 
    mutate(rawresidSq = (yhat_rawresid)^2)
  ssr <- sum(calcDat$rawresidSq, na.rm = TRUE)
  rmse <- sqrt((ssr/ncount))

  print(paste("----------------", likelihoodString))
  print(paste("residual sum of sqaures", ssr))
  print(paste("residual mean square error", rmse))
}


#### MAIN #################################

for (i in 1:length(modCodeStrLs)){
  modCodeStr <- modCodeStrLs[i]
  likelihoodString <- likStrings[i]
  
  #### EXPORT FILEPATHS ################################# 
  # csv file export directories
  setwd(dirname(sys.frame(1)$ofile))
  setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
  path_csvExport <- getwd()
 
  calculate_rss_rmse(path_csvExport, likelihoodString)
}