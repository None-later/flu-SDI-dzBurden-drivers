
## Name: Elizabeth Lee
## Date: 4/11/16
## Function: EDA suite of variable selection analyses for iliSum
## Filenames: dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: 
## Notes: 6/2/16: moved some of the common functions to source_variableSelection.R
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
analysesOn <- c("singleVarPlot") 
# loadData, dataQuality, pairwise, singleVarWrite, singleVarPlot 


#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_data_functions.R") # functions to clean original data sources
source("source_variableSelection_st.R") # functions for variable selection analyses specific to state level
source("source_variableSelection.R") # functions for variable selection analyses, generally

#### FILEPATHS #################################
setwd('../reference_data')
path_pop_st <- paste0(getwd(), "/pop_st_Census_00-10.csv")
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")

setwd("../R_export")
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
path_imsCov_st <- paste0(getwd(), "/physicianCoverage_IMSHealth_state.csv")
path_coefDat <- paste0(getwd(), sprintf("/VS_coefDat_%s_st.csv", rCode))
path_tempDatQuality <- paste0(getwd(), sprintf("/VS_tempDatQuality_%s_st.csv", rCode))

setwd(dirname(sys.frame(1)$ofile))
setwd("../graph_outputs")
path_pltExport <- paste0(getwd(), "/VS_analysisSuite_iliSum_st")

# put all paths in a list to pass them around in functions
path_list <- list(path_pop_st = path_pop_st,
                  path_abbr_st = path_abbr_st,
                  path_latlon_st = path_latlon_st,
                  path_response_st = path_response_st,
                  path_imsCov_st = path_imsCov_st)

#### PLOT FORMATTING ################################
w <- 13; h <- 13; dp <- 200
w2 <- 7; h2 <- 7
setwd(path_pltExport)

#### MAIN #################################################################

#### Import and process response & covariate data ####################################
if("loadData" %in% analysesOn){
  
  # load data frame with all available cleaned variables
  allDat <- prepare_allCov_iliSum_st(path_list) 
  allDat2 <- allDat %>% 
    select(-X_popdensity, -X_housdensity) # these var were available only for Census 2000 and Census 2010
  summary(allDat2)
  
} # end loadData

#### Check data quality ####################################
if("dataQuality" %in% analysesOn){
  
  # Drivers spreadsheet, output for Avail st-seas table
  # check counts in each column
  dataQuality <- allDat2 %>% group_by(season) %>% summarise_each(funs(ct = sum(!is.na(.))))
  write_csv(dataQuality, path_tempDatQuality)
  
} # end dataQuality

#### Pairwise variable comparisons ####################################
if("pairwise" %in% analysesOn){
  
  # full scatterplot matrix
  png(sprintf("scatterMx_%s_st%s.png", rCode, dbCodeStr), width = w, height = h, units = "in", res = dp)
  scatterMx <- pairs_scatterplotMatrix(allDat2)
  print(scatterMx)
  dev.off()
  
  # full correlation matrix
  png(sprintf("corrMx_spearman_%s_st%s.png", rCode, dbCodeStr), width = w, height = h, units = "in", res = dp)
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
  ggsave(sprintf("singleVar_coefSeason_%s_st.png", rCode), width = w2, height = h2, dpi = dp)
}


