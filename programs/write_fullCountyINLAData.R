## Name: Elizabeth Lee
## Date: 10/26/16
## Function: write county level INLA data inputs and outputs for a single modCodeStr
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
source("source_export_inlaDiagnostics.R") # plot_diag_scatter_hurdle function, calculate_residuals

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr <- "7a_iliSum_v3-2"
seasons <- c(2:9)
lik <- "gamma"

#### csv filepath #################################
# csv file export directories
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
path_csvExport <- getwd()

#### functions #################################

cleanData <- function(path_csvExport, likelihoodString){
  # import data and call scatterplot with fitted values/residuals vs. predictors
  print(match.call())
  
  #### import fitted values ####
  setwd(path_csvExport)
  fitfile_list <- grep(sprintf("summaryStatsFitted_%s", likelihoodString), list.files(), value = TRUE)
  fitDat <- tbl_df(data.frame())
  
  for (infile in fitfile_list){
    seasFile <- read_csv(infile, col_types = "ccd_ccddddddd")
    fitDat <- bind_rows(fitDat, seasFile)
  }
  names(fitDat) <- c("modCodeStr", "dbCodeStr", "season", "fips", "ID", "mean", "sd", "q_025", "q_5", "q_975", "mode", "y")
  
  #### import error values ####
  coeffile_list <- grep("summaryStats_", list.files(), value = TRUE)
  coefDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), effectType = c(), likelihood = c(), mean = c(), sd = c(), q_025 = c(), q_5 = c(), q_975 = c()))
  
  for (infile in coeffile_list){
    seasFile <- read_csv(infile, col_types = "ccd_cccddddd__")
    coefDf <- bind_rows(coefDf, seasFile)
  }
  
  #### import id crosswalk ####
  readfile_list2 <- grep("ids_", list.files(), value = TRUE)
  idDat <- tbl_df(data.frame())
  
  for (infile2 in readfile_list2){
    seasFile2 <- read_csv(infile2, col_types = "dc__cd")
    idDat <- bind_rows(idDat, seasFile2)
  }
  
  #### clean fitted data ####
  fitDat2 <- calculate_residuals(fitDat, TRUE)
  fitDat_clean <- fitDat2 %>%
    rename(fit_mn = mean, fit_sd = sd, fit_LB = LB, fit_UB = UB) %>%
    select(season, fips, fit_mn, fit_sd, fit_LB, fit_UB, y, y_nonzero, yhat_resid, yhat_rawresid)
  
  #### clean coefficient data ####
  coefDf_clean <- coefDf %>%
    filter(likelihood == likelihoodString & effectType == "spatial") %>%
    rename(fips = RV, error_mn = mean, error_sd = sd) %>%
    select(fips, error_mn, error_sd) %>%
    mutate(error_LB = error_mn - (2*error_sd), error_UB = error_mn + (2*error_sd))
  
  #### merge data ####
  fullDat <- left_join(fitDat_clean, coefDf_clean, by = "fips") %>%
    left_join(idDat, by = c("season", "fips")) %>%
    mutate(season = as.factor(as.integer(season))) %>%
    mutate(regionID = as.factor(as.integer(regionID)))
  
  #### prepare full dataset for export ####
  exportDat <- fullDat %>%
    select(-contains("_LB"), -contains("_UB")) %>%
    rename(state = st, region = regionID) %>%
    rename(std_resid = yhat_resid, raw_resid = yhat_rawresid) %>%
    select(season, fips, state, region, y, y_nonzero, fit_mn, fit_sd, raw_resid, std_resid, error_mn, error_sd)
  
  return(exportDat)
}

#### main #################################
exportDataset <- cleanData(path_csvExport, lik)
write_csv(exportDataset, paste0("fullcountydata_", lik, "_", modCodeStr, ".csv"))

