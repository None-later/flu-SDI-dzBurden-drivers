require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL)

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr <- "6a_iliSum_v1-15"; testDataOn <- FALSE
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
# #### import model data ################################
# if (testDataOn){
#   modData <- testing_module(path_list)
# } else{
#   modData <- model6a_iliSum_v1(path_list)
# }

#### import fitted data & calculate gamma residuals ################################
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
gamCoef <- read_csv(sprintf("summaryStats_%s_S%s.csv", modCodeStr, s))
binFit <- read_csv(sprintf("summaryStatsFitted_binomial_%s_S%s.csv", modCodeStr, s))
gamFit <- read_csv(sprintf("summaryStatsFitted_gamma_%s_S%s.csv", modCodeStr, s)) %>%
  mutate(yhat_resid = (y-mean)/sd) %>%
  mutate(yhat_rawresid = (y-mean)) %>%
  mutate(LB = mean-(sd*2), UB = mean+(sd*2))

#### look for binomial fitted values above 1 ################################
prob1 <- binFit %>% filter(mode > 1 | q_975 > 1 | mean > 1) %>% select(-modCodeStr, -dbCodeStr, -exportDate)
View(prob1)

#### check large residuals ################################
# for fips with large residuals, look at gamma fitted values
residFit <- gamFit %>% filter(yhat_resid > 30) %>% select(-modCodeStr, -dbCodeStr, -exportDate)
View(residFit)

# get list of fips with large standardized residuals
fipsResid <- residFit %>% distinct(fips) %>% unlist

# for fips with large residuals, look at random effects
residCoef <- gamCoef %>% filter(effectType == "spatial") %>% filter(RV %in% fipsResid)
View(residCoef)

# for fips with large residuals, look at binomial fitted values
residBin <- binFit %>% filter(fips %in% fipsResid) %>% select(-modCodeStr, -dbCodeStr, -exportDate)
View(residBin)

# #### check large fitted values ################################
# largeFit <- gamFit %>% filter(mean > 100) %>% select(-modCodeStr, -dbCodeStr, -exportDate)
# View(largeFit)
# 
# fipsLarge <- largeFit %>% distinct(fips) %>% unlist
# largeCoef <- gamCoef %>% filter(effectType == "spatial") %>% filter(RV %in% fipsLarge)
# View(largeCoef)


