require(readr)
require(tidyr)
require(dplyr)


#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr <- "6a_iliSum_v1dicExplore1"; testDataOn <- TRUE
seasons <- 3:3 
rdmFx_RV <- "nu"
dig <- 4 # number of digits in the number of elements at this spatial scale (~3000 counties -> 4 digits)

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
source("source_clean_data_functions.R") # functions to clean covariate data
source("source_prepare_inlaData_cty.R") # functions to aggregate all data sources for model
source("source_export_inlaData_cty.R") # functions to plot county-specific model diagnostics
source("source_export_inlaData.R") # functions to plot general model diagnostics
source("source_export_inlaData_hurdle.R") # data export functions for hurdle model

#### FILEPATHS #################################
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


#### MAIN #################################
#### import model data ################################
modData <- testing_module(path_list)
modData_full <- modData %>% filter(season == 3) %>% mutate(ID = seq_along(fips))
modData_hurdle <- convert_hurdleModel_separatePredictors(modData_full)


#### import fitted data ################################
setwd(dirname(sys.frame(1)$ofile))
setwd("../R_export/inlaModelData_export/6a_iliSum_v1dicExplore1")
binDat <- read_csv("summaryStatsFitted_binomial_6a_iliSum_v1dicExplore1_S3.csv")
gamDat <- read_csv("summaryStatsFitted_gamma_6a_iliSum_v1dicExplore1_S3.csv")

naFips <- c("13265", "21201", "32009", "37095", "41055", "48235", "48261", "48301", "48311", "48425", "48431")

#### examine original data with NA fitted modes ################################
naModData <- modData_full %>% filter(fips %in% naFips)
