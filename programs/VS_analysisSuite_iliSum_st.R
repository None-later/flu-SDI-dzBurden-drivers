
## Name: Elizabeth Lee
## Date: 4/11/16
## Function: EDA suite of variable selection analyses for iliSum
## Filenames: dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr) # clean_data_functions dependencies
require(RColorBrewer); require(ggplot2)

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
seasons <- 2:9

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_data_functions.R") # functions to clean original data sources
source("source_variableSelection_st.R") # functions for variable selection analyses

#### FILEPATHS #################################
setwd('../reference_data')
path_pop_st <- paste0(getwd(), "/pop_st_Census_00-10.csv")
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")

setwd("../R_export")
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
path_imsCov_st <- paste0(getwd(), "/physicianCoverage_IMSHealth_state.csv")

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
w <- 14; h <- 14; dp <- 200
setwd(path_pltExport)

#### MAIN #################################
#### Import and process response & covariate data ####
# with all available cleaned variables
allDat <- prepare_allCov_iliSum(path_list) 
allDat2 <- allDat %>% 
  select(-X_popdensity, -X_housdensity) # these var were available only for Census 2000 and Census 2010
summary(allDat2)

#### Pairwise variable comparisons ####
# full scatterplot matrix
png(sprintf("scatterMx_iliSum_st%s.png", dbCodeStr), width = w, height = h, units = "in", res = dp)
scatterMx <- pairs_scatterplotMatrix(allDat2)
print(scatterMx)
dev.off()

# full correlation matrix
png(sprintf("corrMx_spearman_iliSum_st%s.png", dbCodeStr), width = w, height = h, units = "in", res = dp)
corrMx <- pairs_corrMatrix(allDat2)
print(corrMx)
dev.off()

