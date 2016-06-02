
## Name: Elizabeth Lee
## Date: 6/2/16
## Function: check the data quality of the functions in source_clean_response_functions_cty.R
## Filenames: source_clean_response_functions_cty.R
## Data Source: 
## Notes: need to SSH into snow server
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL) # clean_data_functions dependencies


#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr <- "4a_iliSum_v3"
seasons <- 2:9
rdmFx_RV <- "nu"

#### SOURCE: import data cleaning functions #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean original data sources

#### check iliSum response variable ###################
#### FILEPATHS ####
setwd('../reference_data')
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")

setwd("../R_export")
path_response_zip3 <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB.csv", dbCodeStr))

# put all paths in a list to pass them around in functions
path_list <- list(path_latlon_cty = path_latlon_cty,
                  path_response_zip3 = path_response_zip3)

#### checks ####
cw <- cw_zip3_cty() 
rDat <- cleanR_iliSum_cty(path_list)
origDat <- read_csv(path_list$path_response_zip3, col_types = "iclcd")
popDat <- clean_pop_cty(path_list)

# is the has.epi variable treated appropriately when F & T zip3s overlap with the same county? YES, has.epi is TRUE if at least one zip3 had has.epi = TRUE. The weighted.mean works as expected as well.

# how frequently are zOverlaps for a county response less than that in the spatial crosswalk?
cwCt <- cw %>% group_by(fips) %>% summarise(trueOverlaps = length(zip3))
rCt <- rDat %>% select(fips, season, zOverlaps)
# 25,144 season-fip combos; 18,972 combos where !is.na(diffOver) -- these didn't have burden data & turned NA in the full_join with cw 
# 5,231 combos with differences
mergeCt <- full_join(rCt, cwCt, by = "fips") %>%
  mutate(diffOver = ifelse(zOverlaps != trueOverlaps, TRUE, FALSE))
# counties typically have a zip3 with missing burden data only in 0/1 seasons or inall 8 seasons
byFips <- mergeCt %>% group_by(fips) %>% summarise(diffOver = sum(diffOver, na.rm = TRUE))
hist(byFips$diffOver)
# the discrepancies seem evenly split across seasons
bySeas <- mergeCt %>% group_by(season) %>% summarise(diffOver = sum(diffOver, na.rm = TRUE))

#### IMS physician coverage: conversion from zip3 to cty ###################
cw <- cw_zip3_cty() 
covDat <- cleanO_imsCoverage_cty()

# # write data to file for Sandra
# setwd("/home/elee/Dropbox/Sandra Goldlust's Work/FLU_DATA")
# write_csv(covDat, "physicianCoverage_IMSHealth_county.csv")
# # exported 6/2/16

# ## Notes about checks performed for physician coverage data - 6/2/16 ############
# fips 01001, year 2002, zOverlaps 2, adjProviderCoverage 0.06160619
# fips 01001, zip 360 (prop = 0.98453), zip 367 (0.01547)
# 
# select zip3, adjProviderCoverage, year from IMS_physicianCoverage_zip3 where (zip3 = '360' or zip3 = '367') and year = 2002;
# ############
# fips 01007, year 2007, zOverlaps 4, adProviderCoverage 0.12038610
# zip 350 (prop = 0.57789), zip 351 (prop = 0.37130), zip 354 (prop = 0.00067), zip 367 (prop = 0.05015)
# 
# select zip3, adjProviderCoverage, year from IMS_physicianCoverage_zip3 where (zip3 = '350' or zip3 = '351' or zip3 = '354' or zip3 = '367') and year = 2007;
# ############
# fips 56045, year 2004, zOverlaps 1, adjProviderCov 0.2077
# zip3 = '827'
# 
# select zip3, adjProviderCoverage, year from IMS_physicianCoverage_zip3 where (zip3 = '827') and year = 2004;









