
## Name: Elizabeth Lee
## Date: 1/22/16
## Function: Functions for cleaning response and covariate data for INLA
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### functions for model data cleaning ################################
require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL)

##### RESPONSE VARIABLES ##########
cleanR_iliSum_st <- function(filepathList){
  # clean response variable: ilinDt.sum
  print(match.call())
  
  # grab disease burden metric (e.g., ilinDt): match "ili" 1+ times
  dbCode <- grep("ili+", strsplit(filepathList$path_response_st, "_")[[1]], value=T)
  # clean data
  iliSum_data <- read_csv(filepathList$path_response_st, col_types = "iccd") %>%
    filter(metric == sprintf("%s.sum", dbCode)) %>%
    select(-metric) %>%
    rename(y = burden, abbr = state)
  
  pop_data <- clean_pop_st(filepathList)
  
  return_data <- left_join(iliSum_data, pop_data, by = c("season", "abbr")) %>%
    select(fips, abbr, state, lat, lon, season, year, pop, y) %>% 
    group_by(season) %>%
    mutate(E = weighted.mean(y, pop))
  
  return(return_data)
}

##### REFERENCE DATA ##########
clean_pop_st <- function(filepathList){
  # clean pop data with state abbr
  print(match.call())
  
  pop_data <- read_csv(filepathList$path_pop_st, col_types = "ccii")
  abbr_data <- read_csv(filepathList$path_abbr_st, col_types = "_cc")
  coord_data <- read_csv(filepathList$path_latlon_st , col_types = "cdd", col_names = c("abbr", "lat", "lon"), skip = 1)
  
  dummy <- left_join(pop_data, abbr_data, by = c("st_fip" = "FIPS")) %>%
    rename(fips = st_fip, abbr = Abbreviation) %>%
    mutate(season = as.numeric(substring(year, 3, 4))) %>%
    select(fips, abbr, state, season, year, pop) %>%
    filter(season > 1)
  
  fulldata <- left_join(dummy, coord_data, by = "abbr")
  
  return(fulldata)
}

##### SAMPLING EFFORT DATA ##########
cleanC_imsCoverage_st <- function(filepathList){
  # clean IMS Health adjusted physician coverage (database coverage) and physician per visit ratio (care-seeking behavior)
  # logit transform the adjusted percentage of provider coverage in IMS database; log transform the ratio of visits per provider in the IMS database
  # center and standardize the transformed variables
  print(match.call())
  
  cov_data <- read_csv(filepathList$path_imsCov_st, col_types = "iciiiidddi") %>% 
    rename(abbr = state) %>%
    rename(adjProviderCoverage = covAdjProv) %>%
    mutate(visitsPerProvider_raw = sampViz/sampProv) %>%
    mutate(adjProviderCoverage = centerStandardize(log(adjProviderCoverage/(1-adjProviderCoverage)))) %>%
    mutate(visitsPerProvider = centerStandardize(log(visitsPerProvider_raw))) %>%
    select(year, abbr, adjProviderCoverage, visitsPerProvider)
    
  return(cov_data)
  
}

cleanC_cpsasecInsured_st <- function(){
  # clean CSP-ASEC insured data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "HI_CPSasec_state")
  # sel.head.uninsured <- "Select * from HI_CPSasec_state limit 5"
  sel.statement.insured <- "SELECT year, state_id AS fips, Percent_covered/100 AS insured_prop FROM HI_CPSasec_state WHERE type = 'state'"
  dummy <- dbGetQuery(con, sel.statement.insured)
 
  dbDisconnect(con)
  
  insured <- tbl_df(dummy) %>%
    mutate(insured = centerStandardize(log(insured_prop/(1-insured_prop)))) %>%
    select(-insured_prop)
  
  return(insured)
}



##### DRIVER DATA ##########
