
## Name: Elizabeth Lee
## Date: 8/10/17
## Function: Calculate VIF for 8a V2-6 
## Filenames: physicianCoverage_IMSHealth_state.csv, al.4_degree2_analyzeDB_st.csv
## Data Source: IMS Health
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(tidyverse); require(DBI); require(RMySQL)
require(car)

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
source("source_clean_data_functions.R") # functions to clean covariate data
source("source_prepare_inlaData_cty.R") # functions to aggregate all data sources for model

#### FILEPATHS #################################
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")

setwd('./UScounty_shapefiles')
path_adjMxExport_cty <- paste0(getwd(), "/US_county_adjacency.graph")
path_graphIdx_cty <- paste0(getwd(), "/US_county_graph_index.csv")
path_shape_cty <- paste0(getwd(), "/gz_2010_us_050_00_500k") # for dbf metadata only

setwd('../stateFlightpassenger_graph')
path_graphExport_st <- paste0(getwd(), "/US_statePassenger_edgelist.txt")
path_graphIdx_st <- paste0(getwd(), "/US_statePassenger_graph_index.csv")

setwd("../../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                    path_latlon_cty = path_latlon_cty,
                    path_shape_cty = path_shape_cty,
                    path_adjMxExport_cty = path_adjMxExport_cty,
                    path_response_cty = path_response_cty, 
                    path_graphIdx_cty = path_graphIdx_cty,
                    path_graphExport_st = path_graphExport_st,
                    path_graphIdx_st = path_graphIdx_st)


#### MAIN #################################
# import data
dat <- model8a_iliSum_v7(path_list) 

# UPDATE FORMULA ONCE VARIABLE SELECTION IS COMPLETE
formula <- y1 ~ 1 + O_imscoverage + O_careseek + O_insured + X_poverty + X_child + X_adult + X_hospaccess + X_popdensity + X_housdensity + X_vaxcovI + X_vaxcovE + X_H3A + X_B + X_priorImmunity + X_humidity + X_pollution + X_singlePersonHH + X_H3A*X_adult + X_B*X_child + offset(logE)
  

modVif <- vif(lm(formula, data = dat))
print(modVif)
