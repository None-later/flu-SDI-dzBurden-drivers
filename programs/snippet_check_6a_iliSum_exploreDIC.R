require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL)

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr <- "6a_iliSum_v1-14"; testDataOn <- FALSE
s <- 4 
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
if (testDataOn){
  modData <- testing_module(path_list)
} else{
  modData <- model6a_iliSum_v1(path_list)
}

#### import fitted data ################################
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
binDat <- read_csv(sprintf("summaryStatsFitted_binomial_%s_S%s.csv", modCodeStr, s))
gamDat <- read_csv(sprintf("summaryStatsFitted_gamma_%s_S%s.csv", modCodeStr, s))

# naFips <- c("13265", "21201", "32009", "37095", "41055", "48235", "48261", "48301", "48311", "48425", "48431")

#### identify NA in fitted binomial likelihood modes ################################
binNA_df <- binDat %>% filter(is.na(mode)) %>% select(-modCodeStr, -dbCodeStr, -exportDate)
binNAs <- binNA_df %>% select(fips) %>% unlist
gamDat_binNAs <- gamDat %>% filter(fips %in% binNAs) %>% select(-modCodeStr, -dbCodeStr, -exportDate)

#### check locations where sd > mean ################################
binDat_largeSD <- binDat %>% select(-modCodeStr, -dbCodeStr, -exportDate) %>% filter(sd > mean) # no entries
gamDat_largeSD <- gamDat %>% select(-modCodeStr, -dbCodeStr, -exportDate) %>% filter(sd > mean)
gamchecks <- gamDat_largeSD %>% select(fips) %>% unlist

#### look at modData for binNAs ####
modDat_check1 <- modData %>% filter(fips %in% binNAs) %>% select(-lat, -lon, -year, -fips_st)
modDat_check1_S4 <- modDat_check1 %>% filter(season == 4)
modDat_check1_S5 <- modDat_check1 %>% filter(season == 5)

#### look at modData for gamchecks ####
modDat_check2 <- modData %>% filter(fips %in% gamchecks) %>% select(-lat, -lon, -year, -fips_st)
