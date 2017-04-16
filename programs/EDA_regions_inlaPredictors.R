
## Name: Elizabeth Lee
## Date: 4/16/17
## Function: Explore variation of predictors within region groups
## Filenames: 
## Data Source: IMS Health
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL) # clean_data_functions dependencies
require(maptools); require(spdep) # prepare_inlaData_st.R dependencies
require(RColorBrewer); require(ggplot2) # export_inlaData_st dependencies

# labLs <- c("1&2", "3", "4", "5", "6", "7", "8&9&10")
# regLs <- list(c(1,2), 3, 4, 5, 6, 7, c(8,9,10))
labLs <- c("1&2&3", "4&6", "5&7")
regLs <- list(c(1,2,3), c(4,6), c(5,7), c(8,9,10))
modCodeLs <- paste0("8a_iliSum_v2-6_R", labLs)

  
#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
rdmFx_RV <- "phi"
likString <- "normal"
dig <- 4 # number of digits in the number of elements at this spatial scale (~3000 counties -> 4 digits)
s <- 999 # all seasons code for spatiotemporal analysis = 999

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
#### Import and process data ####
modDat <- model8a_iliSum_regions_raw(path_list, regLs)

mnDat <- modDat %>% 
  group_by(regionGroup) %>%
  summarise_each(funs(mn = mean(., na.rm = TRUE)), contains("_"))

sdDat <- modDat %>% 
  group_by(regionGroup) %>%
  summarise_each(funs(sd(., na.rm = TRUE)), contains("_"))

