## Name: Elizabeth Lee
## Date: 5/27/16
## Function: Functions for cleaning disease burden response data at the county level for INLA
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### functions for model data cleaning ################################
require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL)

##### COUNTY-LEVEL VARIABLES ##########################################
cleanR_iliSum_cty <- function(filepathList){
  # clean response variable: ilinDt.sum
  print(match.call())
  
  # spatial crosswalk: fips, zip3, proportion (of overlap in zip3 & fips population)
  cw <- cw_zip3_cty() 
  # pop data: fips, county, st, season, year, pop, lat lon
  pop_data <- clean_pop_cty(filepathList)
  
  # grab disease burden metric (e.g., ilinDt): match "ili" 1+ times
  dbCode <- grep("ili+", strsplit(filepathList$path_response_zip3, "_")[[1]], value=T)
  # clean burden data
  iliSum_data <- read_csv(filepathList$path_response_zip3, col_types = "icllcd") %>%
    filter(metric == sprintf("%s.sum", dbCode)) %>%
    select(-metric) %>%
    rename(y = burden) %>%
    full_join(cw, by = "zip3") %>% 
    group_by(fips, season) %>%
    summarise(has.epi = sum(has.epi), zOverlaps = length(zip3), y = weighted.mean(y, proportion, na.rm = TRUE)) %>%
    mutate(has.epi = ifelse(has.epi > 0, TRUE, FALSE))
  
  # merge final data
  return_data <- full_join(iliSum_data, pop_data, by = c("season", "fips")) %>%
    select(fips, county, st, lat, lon, season, year, pop, y, has.epi, zOverlaps) %>%
    group_by(season) %>%
    mutate(E = weighted.mean(y, pop, na.rm = TRUE)) %>%
    ungroup %>%
    filter(season != 1) %>%
    mutate(logy = ifelse(has.epi, log(y), log(1E-2)), logE = log(E)) # 6/1/16 log(1E-2) so no -Inf burden (need to do sensitivity on this)
  return(return_data)
}

##### SAMPLING EFFORT DATA ##########################################
cleanO_imsCoverage_cty <- function(){
  # clean IMS Health adjusted physician coverage (database coverage) and physician per visit ratio (care-seeking behavior) from zip3 to county level, using overlapping pop bw zip3 & county as a weight for the weighted average
  print(match.call())

  # spatial crosswalk: fips, zip3, proportion (of overlap in zip3 & fips population)
  cw <- cw_zip3_cty() 
  
  # import physician coverage data
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "IMS_physicianCoverage_zip3")
  # sel.statement <- "Select * from IMS_physicianCoverage_zip3 limit 5"
  sel.statement <- "SELECT year, zip3, adjProviderCoverage, sampViz, sampProv FROM IMS_physicianCoverage_zip3"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)

  # clean zip3 coverage to county coverage
  covDat <- dummy %>%
    full_join(cw, by = "zip3") %>%
    group_by(fips, year) %>%
    summarise(zOverlaps = length(zip3), adjProviderCoverage = weighted.mean(adjProviderCoverage, proportion, na.rm = TRUE), sampViz = weighted.mean(sampViz, proportion, na.rm = TRUE), sampProv = weighted.mean(sampProv, proportion, na.rm = TRUE)) %>% 
    ungroup %>%
    filter(!is.na(fips)) %>% 
    mutate(visitsPerProvider = sampViz/sampProv) %>%
    select(fips, year, adjProviderCoverage, visitsPerProvider)
  return(covDat)
}

##### REFERENCE DATA ##########################################
cw_zip3_cty <- function(){
  # spatial crosswalk for zip3-county
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "spatialcw_zip3_county")
  # sel.statement <- "Select * from spatialcw_zip3_county limit 5"
  sel.statement <- "SELECT fips, zip3, zinf_prop as proportion FROM spatialcw_zip3_county"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) 
  return(output)
}

################################
clean_pop_cty <- function(filepathList){
  # clean pop data at county level
  print(match.call())
  
  # read coord data by county: reference_data/cty_pop_latlon.csv
  coord_data <- read_csv(filepathList$path_latlon_cty , col_types = "cc__dd", col_names = c("st", "fips", "lat", "lon"), skip = 1) 
  # read state name data: reference_data/state_abbreviations_FIPS.csv
  abbr_data <- read_csv(filepathList$path_abbr_st, col_types = "cc_", col_names = c("state", "st"), skip = 1)
  
  # import population data from mysql
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "demog_Census_agePop_county")
  # sel.statement <- "Select * from demog_Census_agePop_county limit 5"
  sel.statement <- "SELECT fips, county, agegroup, year, pop FROM demog_Census_agePop_county WHERE scale = 'county' and agegroup = 'total' and year >= 2002 and year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  # clean final dataset
  output <- tbl_df(dummy) %>% 
    mutate(season = as.numeric(substring(year, 3, 4))) %>%
    left_join(coord_data, by = "fips") %>%
    left_join(abbr_data, by = "st") %>% 
    select(fips, county, st, state, season, year, pop, lat, lon)
  return(output)
}

