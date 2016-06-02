
## Name: Elizabeth Lee
## Date: 6/2/16
## Function: EDA suite of variable selection analyses for iliSum at county level
## Filenames: dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB.csv, source_clean_data_function.R
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
# rm(list = ls())
require(dplyr); require(tidyr); require(readr) # clean_data_functions dependencies
require(RColorBrewer); require(ggplot2)

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
rCode <- "iliSum"
seasons <- 2:9
analysesOn <- c('loadData', 'dataQuality', 'pairwise', 'singleVarWrite', 'singleVarPlot') 
# loadData, dataQuality, pairwise, singleVarWrite, singleVarPlot 


#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
source("source_clean_data_functions.R") # functions to clean covariate data
source("source_variableSelection_cty.R") # functions for variable selection analyses specific to county scale
source("source_variableSelection.R") # functions for variable selection, generally

#### FILEPATHS #################################
setwd('../reference_data')
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")

setwd("../R_export")
path_response_zip3 <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB.csv", dbCodeStr))
path_coefDat <- paste0(getwd(), sprintf("/VS_coefDat_%s_cty.csv", rCode))
path_tempDatQuality <- paste0(getwd(), sprintf("/VS_tempDatQuality_%s_cty.csv", rCode))

# put all paths in a list to pass them around in functions
path_list <- list(path_latlon_cty = path_latlon_cty,
                  path_response_zip3 = path_response_zip3)

setwd(dirname(sys.frame(1)$ofile))
setwd("../graph_outputs")
path_pltExport <- paste0(getwd(), "/VS_analysisSuite_iliSum_cty")

#### PLOT FORMATTING ################################
w <- 13; h <- 13; dp <- 200
w2 <- 7; h2 <- 7
setwd(path_pltExport)

#### MAIN #################################################################

#### Import and process response & covariate data ####################################
if("loadData" %in% analysesOn){
  
  # load data frame with all available cleaned variables
  allDat <- prepare_allCov_iliSum_cty(path_list) 
  summary(allDat)
  
} # end loadData

#### Check data quality ####################################
if("dataQuality" %in% analysesOn){
  
  # output county counts by season for each variable: VS_tempDatQuality_%s_cty.csv
  # check counts in each column
  dataQuality <- allDat2 %>% group_by(season) %>% summarise_each(funs(ct = sum(!is.na(.))))
  write_csv(dataQuality, path_tempDatQuality)
  
} # end dataQuality

#### Pairwise variable comparisons ####################################
if("pairwise" %in% analysesOn){
  
  # full scatterplot matrix
  png(sprintf("scatterMx_%s_cty%s.png", rCode, dbCodeStr), width = w, height = h, units = "in", res = dp)
  scatterMx <- pairs_scatterplotMatrix(allDat2)
  print(scatterMx)
  dev.off()
  
  # full correlation matrix
  png(sprintf("corrMx_spearman_%s_cty%s.png", rCode, dbCodeStr), width = w, height = h, units = "in", res = dp)
  corrMx <- pairs_corrMatrix(allDat2)
  print(corrMx)
  dev.off()
  
} # end pairwise

#### Single variable models - Write to file ####################################
if("singleVarWrite" %in% analysesOn){
  
  varlist <- grep("[OX]{1}[_]{1}", names(allDat2), value = TRUE)  # grab all varnames
  # generate empty data frame to store coefficient data
  coefDat <- tbl_df(data.frame(respCode = c(), singleCov = c(), season = c(), exportDate = c(), coefMode = c(), coefQ025 = c(), coefQ975 = c(), DIC = c()))
  
  # loop through all variables and seasons
  for (varInterest in varlist){
    for (s in seasons){
      modDat <- subset_singleVariable_data(allDat2, s, varInterest)
      if (all(is.na(modDat$varInterest))){
        print(sprintf("%s is all NA for season %s", varInterest, s))
        next
      }
      # save row of model data
      else{
        coefRow <- model_singleVariable_inla(modDat, rCode, s, varInterest) # N.B. model includes intercept
        # append to model data object
        coefDat <- bind_rows(coefDat, coefRow)
      } # end else
    } # end for seasons
  } # end for varlist
  
  # write to file
  write_csv(coefDat, path_coefDat)
  
} # end singleVarWrite

#### Single variable models - plot coef ####################################
if("singleVarPlot" %in% analysesOn){
  setwd(dirname(sys.frame(1)$ofile))
  setwd("../R_export")
  
  coefDat <- read_csv(path_coefDat)
  varlist <- coefDat %>% select(singleCov) %>% unique %>% unlist
  
  setwd(path_pltExport)
  plt_coefTime <- plot_singleVarCoef_time(coefDat)
  ggsave(sprintf("singleVar_coefSeason_%s_cty.png", rCode), width = w2, height = h2, dpi = dp)
}


