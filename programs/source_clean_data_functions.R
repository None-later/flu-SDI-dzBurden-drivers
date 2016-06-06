
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

##### RESPONSE VARIABLES ##########################################
cleanR_iliSum_st <- function(filepathList){
  # clean response variable: ilinDt.sum
  print(match.call())
  
  # grab disease burden metric (e.g., ilinDt): match "ili" 1+ times
  dbCode <- grep("ili+", strsplit(filepathList$path_response_st, "_")[[1]], value=T)
  # clean data
  iliSum_data <- read_csv(filepathList$path_response_st, col_types = "iclcd") %>%
    filter(metric == sprintf("%s.sum", dbCode)) %>%
    select(-metric) %>%
    rename(y = burden, abbr = state)
  
  pop_data <- clean_pop_st(filepathList) # 4/12/16 all 51 pops are there
  
  return_data <- full_join(iliSum_data, pop_data, by = c("season", "abbr")) %>% # 4/12/16 full_join so pops don't drop
    select(fips, abbr, state, lat, lon, season, year, pop, y, has.epi) %>% 
    group_by(season) %>%
    mutate(E = weighted.mean(y, pop, na.rm=TRUE)) %>%
    ungroup %>%
    mutate(logy = ifelse(has.epi, log(y), log(1E-2)), logE = log(E)) # 5/9/16 log(1E-2) so no -Inf as db metric
  
  return(return_data)
}

##### REFERENCE DATA ##########################################
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

##### SAMPLING EFFORT DATA ##########################################
cleanO_imsCoverage_st <- function(filepathList){
  # clean IMS Health adjusted physician coverage (database coverage) and physician per visit ratio (care-seeking behavior)
  # logit transform the adjusted percentage of provider coverage in IMS database; log transform the ratio of visits per provider in the IMS database
  # center and standardize the transformed variables
  print(match.call())
  
  cov_data <- read_csv(filepathList$path_imsCov_st, col_types = "iciiiidddi") %>% 
    rename(abbr = state) %>%
    rename(adjProviderCoverage = covAdjProv) %>%
    mutate(visitsPerProvider = sampViz/sampProv) %>%
    select(year, abbr, adjProviderCoverage, visitsPerProvider) 
    
  return(cov_data)
  
}
################################

cleanO_cpsasecInsured_st <- function(){
  # clean CPS-ASEC insured data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "HI_CPSasec_state")
  # sel.statement <- "Select * from HI_CPSasec_state limit 5"
  sel.statement <- "SELECT year, state_id AS fips, Percent_covered/100 AS insured_prop FROM HI_CPSasec_state WHERE type = 'state'"
  dummy <- dbGetQuery(con, sel.statement)
 
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    rename(insured = insured_prop) %>%
    select(fips, year, insured) %>%
    filter(year >= 2002 & year <= 2009)
  
  return(output)
}
################################

cleanO_sahieInsured_cty <- function(){
  # clean ACS & CPS-based SAHIE insured data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "HI_SAHIE_aggregate_ACS")
  # sel.statement <- "Select * from HI_SAHIE_aggregate_ACS limit 5"
  sel.statement <- "SELECT year, county_id AS fips, pctic/100 AS insured_prop FROM HI_SAHIE_aggregate_ACS WHERE type = 'county'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbListFields(con, "HI_SAHIE_aggregate_CPS")
  # sel.statement <- "Select * from HI_SAHIE_aggregate_CPS limit 5"
  sel.statement <- "SELECT year, county_id AS fips, pctic/100 AS insured_prop FROM HI_SAHIE_aggregate_CPS WHERE type = 'county'"
  dummy2 <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- bind_rows(dummy2, dummy) %>%
    dplyr::rename(insured = insured_prop) %>%
    select(fips, year, insured) %>%
    filter(year >= 2002 & year <= 2009)
  
  return(output)
}

##### DRIVER DATA ##########################################

##### social determinants ##########
cleanX_saipePoverty_st <- function(){
  # clean SAIPE percentage of population in poverty state data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "SAIPE_poverty")
  # sel.statement <- "SELECT * from SAIPE_poverty limit 5"
  sel.statement <- "SELECT year, state_id AS fips, all_poverty_percent/100 AS inPoverty_prop FROM SAIPE_poverty WHERE type = 'state'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    rename(poverty = inPoverty_prop) %>%
    select(fips, year, poverty) %>%
    filter(year >= 2002 & year <= 2009)
  
  return(output)
}
################################

cleanX_saipePoverty_cty <- function(){
  # clean SAIPE percentage of population in poverty county data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "SAIPE_poverty")
  # sel.statement <- "SELECT * from SAIPE_poverty limit 5"
  sel.statement <- "SELECT year, county_id AS fips, all_poverty_percent/100 AS inPoverty_prop FROM SAIPE_poverty WHERE type = 'county'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    rename(poverty = inPoverty_prop) %>%
    select(fips, year, poverty)
  
  return(output)
}
################################

cleanX_saipeIncome_st <- function(){
  # clean SAIPE median household income state data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "SAIPE_income")
  # sel.statement <- "SELECT * from SAIPE_income limit 5"
  sel.statement <- "SELECT year, state_id AS fips, med_income as medianIncome FROM SAIPE_income WHERE type = 'state'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    rename(income = medianIncome) %>%
    select(fips, year, income) %>%
    filter(year >= 2002 & year <= 2009)
  
  return(output)
}
################################

cleanX_saipeIncome_cty <- function(){
  # clean SAIPE median household income county data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "SAIPE_income")
  # sel.statement <- "SELECT * from SAIPE_income limit 5"
  sel.statement <- "SELECT year, county_id AS fips, med_income as medianIncome FROM SAIPE_income WHERE type = 'county'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    rename(income = medianIncome) %>%
    select(fips, year, income) 
  
  return(output)
}
################################

cleanX_ahrfMedicaidEligibles_st <- function(){
  # clean AHRF Medicaid eligibility data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "access_medicare_medicaid")
  # sel.statement <- "SELECT * from access_medicare_medicaid limit 5"
  sel.statement <- "SELECT year, FIPS AS fips_cty, (mcaid_child + mcaid_adult) AS mcaidEligTot, population as pop FROM access_medicare_medicaid WHERE year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(fips = substring(fips_cty, 1, 2)) %>%
    group_by(fips, year) %>%
    summarise(mcaidEligTot = sum(mcaidEligTot, na.rm=TRUE), pop = sum(pop, na.rm=TRUE)) %>%
    mutate(mcaidEligTot = ifelse(mcaidEligTot == 0, NA, mcaidEligTot)) %>% # group_by(fips, year) adds 0s in years with no data after summarise step, convert 0s to NAs
    mutate(mcaidElig = mcaidEligTot/pop) %>%
    filter(year %in% 2004:2008) %>% # 3/2/16: 2005 mcaidElig is NA if years before 2004 are included; not sure why
    select(fips, year, mcaidElig) 
  
  return(output)
}
################################

cleanX_ahrfMedicaidEligibles_cty <- function(){
  # clean AHRF Medicaid eligibility data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "access_medicare_medicaid")
  # sel.statement <- "SELECT * from access_medicare_medicaid limit 5"
  sel.statement <- "SELECT year, FIPS AS fips, (mcaid_child + mcaid_adult) AS mcaidEligTot, population as pop FROM access_medicare_medicaid WHERE year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(mcaidElig = mcaidEligTot/pop) %>%
    filter(year %in% 2004:2008) %>% 
    select(fips, year, mcaidElig) 
  
  return(output)
}

################################


##### demography ##########
cleanX_ahrfMedicareEligibles_st <- function(){
  # clean AHRF Medicare eligibility data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "access_medicare_medicaid")
  # sel.statement <- "SELECT * from access_medicare_medicaid limit 5"
  sel.statement <- "SELECT year, FIPS AS fips_cty, mdcr_elig AS mcareEligTot, population AS pop FROM access_medicare_medicaid WHERE year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(fips = substring(fips_cty, 1, 2)) %>%
    group_by(fips, year) %>%
    summarise(mcareEligTot = sum(mcareEligTot, na.rm=TRUE), pop = sum(pop, na.rm=TRUE)) %>%
    mutate(mcareEligTot = ifelse(mcareEligTot == 0, NA, mcareEligTot)) %>% # group_by(fips, year) adds 0s in years with no data after summarise step, convert 0s to NAs
    mutate(mcareElig = mcareEligTot/pop) %>%
    select(fips, year, mcareElig) 
  
  return(output)
}
################################

cleanX_ahrfMedicareEligibles_cty <- function(){
  # clean AHRF Medicare eligibility data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "access_medicare_medicaid")
  # sel.statement <- "SELECT * from access_medicare_medicaid limit 5"
  sel.statement <- "SELECT year, FIPS AS fips, mdcr_elig AS mcareEligTot, population AS pop FROM access_medicare_medicaid WHERE year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(mcareElig = mcareEligTot/pop) %>%
    select(fips, year, mcareElig) 
  
  return(output)
}
################################

cleanX_censusInfantPop_st <- function(){
  # clean Census population data for <=2 yo, exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "demog_Census_agePop_state")
  # sel.statement <- "SELECT * from demog_Census_agePop_state limit 5"
  sel.statement <- "SELECT year, fips, scale, agegroup, pop FROM demog_Census_agePop_state WHERE scale = 'state' AND (agegroup = 'infant' OR agegroup = 'total') AND year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    spread(agegroup, pop) %>%
    mutate(infant = infant/total) %>%
    select(-total, -scale) 
  
  return(output)
}
################################

cleanX_censusInfantToddlerPop_cty <- function(){
  # clean Census population data for <=2 yo, exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "demog_Census_agePop_county")
  # sel.statement <- "SELECT * from demog_Census_agePop_county limit 5"
  sel.statement <- "SELECT year, fips, scale, agegroup, pop FROM demog_Census_agePop_county WHERE scale = 'county' AND (agegroup = 'infant/toddler' OR agegroup = 'total') AND year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(agegroup = ifelse(agegroup == 'infant/toddler', 'infantToddler', agegroup)) %>%
    spread(agegroup, pop) %>%
    mutate(infantToddler = infantToddler/total) %>%
    select(-total, -scale) 
  
  return(output)
}
################################

cleanX_censusToddlerPop_st <- function(){
  # clean Census population data for 3-4 yo, exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "demog_Census_agePop_state")
  # sel.statement.toddler <- "SELECT * from demog_Census_agePop_state limit 5"
  sel.statement.toddler <- "SELECT year, fips, scale, agegroup, pop FROM demog_Census_agePop_state WHERE scale = 'state' AND (agegroup = 'toddler' OR agegroup = 'total') AND year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement.toddler)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    spread(agegroup, pop) %>%
    mutate(toddler = toddler/total) %>%
    select(-total, -scale) 
  
  return(output)
}
################################

cleanX_censusChildPop_st <- function(){
  # clean Census population data for 5-19 yo, exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "demog_Census_agePop_state")
  # sel.statement.child <- "SELECT * from demog_Census_agePop_state limit 5"
  sel.statement.child <- "SELECT year, fips, scale, agegroup, pop FROM demog_Census_agePop_state WHERE scale = 'state' AND (agegroup = 'child' OR agegroup = 'total') AND year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement.child)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    spread(agegroup, pop) %>%
    mutate(child = child/total) %>%
    select(-total, -scale) 
  
  return(output)
}
################################

cleanX_censusChildPop_cty <- function(){
  # clean Census population data for 5-19 yo, exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "demog_Census_agePop_county")
  # sel.statement.child <- "SELECT * from demog_Census_agePop_county limit 5"
  sel.statement.child <- "SELECT year, fips, scale, agegroup, pop FROM demog_Census_agePop_county WHERE scale = 'county' AND (agegroup = 'child' OR agegroup = 'total') AND year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement.child)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    spread(agegroup, pop) %>%
    mutate(child = child/total) %>%
    select(-total, -scale) 
  
  return(output)
}
################################

cleanX_censusAdultPop_st <- function(){
  # clean Census population data for 20-64 yo, exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "demog_Census_agePop_state")
  # sel.statement <- "SELECT * from demog_Census_agePop_state limit 5"
  sel.statement <- "SELECT year, fips, scale, agegroup, pop FROM demog_Census_agePop_state WHERE scale = 'state' AND (agegroup = 'adult' OR agegroup = 'total') AND year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    spread(agegroup, pop) %>%
    mutate(adult = adult/total) %>%
    select(-total, -scale) 
  
  return(output)
}
################################

cleanX_censusAdultPop_cty <- function(){
  # clean Census population data for 20-64 yo, exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "demog_Census_agePop_county")
  # sel.statement <- "SELECT * from demog_Census_agePop_county limit 5"
  sel.statement <- "SELECT year, fips, scale, agegroup, pop FROM demog_Census_agePop_county WHERE scale = 'county' AND (agegroup = 'adult' OR agegroup = 'total') AND year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    spread(agegroup, pop) %>%
    mutate(adult = adult/total) %>%
    select(-total, -scale) 
  
  return(output)
}
################################

cleanX_censusElderlyPop_st <- function(){
  # clean Census population data for 65+ yo, exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "demog_Census_agePop_state")
  # sel.statement <- "SELECT * from demog_Census_agePop_state limit 5"
  sel.statement <- "SELECT year, fips, scale, agegroup, pop FROM demog_Census_agePop_state WHERE scale = 'state' AND (agegroup = 'elderly' OR agegroup = 'total') AND year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    spread(agegroup, pop) %>%
    mutate(elderly = elderly/total) %>%
    select(-total, -scale) 
  
  return(output)
}
################################

cleanX_censusElderlyPop_cty <- function(){
  # clean Census population data for 65+ yo, exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "demog_Census_agePop_county")
  # sel.statement <- "SELECT * from demog_Census_agePop_county limit 5"
  sel.statement <- "SELECT year, fips, scale, agegroup, pop FROM demog_Census_agePop_county WHERE scale = 'county' AND (agegroup = 'elderly' OR agegroup = 'total') AND year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    spread(agegroup, pop) %>%
    mutate(elderly = elderly/total) %>%
    select(-total, -scale) 
  
  return(output)
}
################################

##### access to care ##########
cleanX_ahrfHospitals_st <- function(){
  # clean AHRF hospitals per pop data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "AHRF_access")
  # sel.statement <- "SELECT * from AHRF_access limit 5"
  sel.statement <- "SELECT year, FIPS AS fips_cty, hosp, population AS pop FROM AHRF_access WHERE year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(fips = substring(fips_cty, 1, 2)) %>%
    group_by(fips, year) %>%
    summarise(hospCt = sum(hosp, na.rm=TRUE), pop = sum(pop, na.rm=TRUE)) %>%
    mutate(hospCt = ifelse(hospCt == 0, NA, hospCt)) %>% # group_by(fips, year) adds 0s in years with no data after summarise step, convert 0s to NAs
    mutate(hospitalAccess = hospCt/pop) %>%
    select(fips, year, hospitalAccess) 
  
  return(output)
}
################################

cleanX_ahrfHospitals_cty <- function(){
  # clean AHRF hospitals per pop data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "AHRF_access")
  # sel.statement <- "SELECT * from AHRF_access limit 5"
  sel.statement <- "SELECT year, FIPS AS fips, hosp, population AS pop FROM AHRF_access WHERE year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(hospitalAccess = hosp/pop) %>%
    select(fips, year, hospitalAccess) 
  
  return(output)
}
################################

cleanX_ahrfPhysicians_st <- function(){
  # clean AHRF physicians per pop data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "AHRF_access")
  # sel.statement <- "SELECT * from AHRF_access limit 5"
  sel.statement <- "SELECT year, FIPS AS fips_cty, physicians, population AS pop FROM AHRF_access WHERE year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(fips = substring(fips_cty, 1, 2)) %>%
    group_by(fips, year) %>%
    summarise(physCt = sum(physicians, na.rm=TRUE), pop = sum(pop, na.rm=TRUE)) %>%
    mutate(physCt = ifelse(physCt == 0, NA, physCt)) %>% # group_by(fips, year) adds 0s in years with no data after summarise step, convert 0s to NAs
    mutate(physicianAccess = physCt/pop) %>%
    select(fips, year, physicianAccess) 
  
  return(output)
}
################################

cleanX_ahrfPhysicians_cty <- function(){
  # clean AHRF physicians per pop data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "AHRF_access")
  # sel.statement <- "SELECT * from AHRF_access limit 5"
  sel.statement <- "SELECT year, FIPS AS fips, physicians, population AS pop FROM AHRF_access WHERE year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(physicianAccess = physicians/pop) %>%
    select(fips, year, physicianAccess) 
  
  return(output)
}
################################

##### contact/travel patterns ##########
cleanX_popDensity_st <- function(){
  # clean population density data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "Census_popdensity_county")
  # sel.statement <- "SELECT * from Census_popdensity_county limit 5"
  sel.statement <- "SELECT year, fips AS fips_cty, popDens_land FROM Census_popdensity_county WHERE type = 'state' AND year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(fips = substring(fips_cty, 1, 2)) %>%
    rename(popDensity = popDens_land) %>%
    select(fips, year, popDensity) %>%
    arrange(fips, year)
  
  return(output)
}
################################

cleanX_popDensity_cty <- function(){
  # clean population density data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "Census_popdensity_county")
  # sel.statement <- "SELECT * from Census_popdensity_county limit 5"
  sel.statement <- "SELECT year, fips, popDens_land FROM Census_popdensity_county WHERE type = 'county' AND year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    rename(popDensity = popDens_land) %>%
    select(fips, year, popDensity) 
  
  return(output)
}
################################

cleanX_housDensity_st <- function(){
  # clean housing unit density data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "Census_popdensity_county")
  # sel.statement <- "SELECT * from Census_popdensity_county limit 5"
  sel.statement <- "SELECT year, fips AS fips_cty, popDens_housing FROM Census_popdensity_county WHERE type = 'state' AND year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(fips = substring(fips_cty, 1, 2)) %>%
    rename(housDensity = popDens_housing) %>%
    select(fips, year, housDensity) %>%
    arrange(fips, year)
  
  return(output)
}
################################

cleanX_housDensity_cty <- function(){
  # clean pop per housing unit density data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "Census_popdensity_county")
  # sel.statement <- "SELECT * from Census_popdensity_county limit 5"
  sel.statement <- "SELECT year, fips, popDens_housing FROM Census_popdensity_county WHERE type = 'county' AND year >= 2002 AND year <= 2009"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    rename(housDensity = popDens_housing) %>%
    select(fips, year, housDensity) 
  
  return(output)
}
################################

cleanX_acsCommutInflows_st <- function(){
  # clean out-of-state commuters per population entering the state
  # will need to calculate incoming commuters per population in source_prepare_inlaData_st.R
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "transport_ACS0610_iconv")
  # sel.statement <- "SELECT * from transport_ACS0610_iconv limit 5"
  sel.statement <- "SELECT state_id_residence_2digit, county_id_residence_3digit, state_id_workplace_3digit, state_workplace, Number FROM transport_ACS0610_iconv"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(domesticWork = ifelse(substring(state_id_workplace_3digit, 1, 1) == "0", TRUE, FALSE)) %>%
    filter(domesticWork) %>%
    mutate(fips_wrk = substr.Right(state_id_workplace_3digit, 2)) %>%
    rename(fips_res = state_id_residence_2digit) %>%
    filter(fips_res != fips_wrk) %>%
    select(-state_id_workplace_3digit, -domesticWork) %>%
    group_by(fips_wrk) %>%
    summarise(ct_2006 = sum(Number)) %>%
    mutate(ct_2007 = ct_2006, ct_2008 = ct_2006, ct_2009 = ct_2006) %>%
    gather(year, commutInflows_prep, ct_2006:ct_2009, convert = TRUE) %>%
    mutate(year = as.numeric(substr.Right(year, 4))) %>%
    ungroup %>%
    select(fips_wrk, year, commutInflows_prep)
  
  return(output)
}
################################

cleanX_acsCommutInflows_cty <- function(){
  # clean out-of-state commuters per population entering the county
  # will need to calculate incoming commuters per population in source_prepare_inlaData_st.R
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  # mysql data from Census 2000
  dbListFields(con, "transport_Census00")
  # sel.statement <- "SELECT * from transport_Census00 limit 5"
  sel.statement00 <- "SELECT Res_ST, Res_CO, Wrk_ST, Wrk_CO, Count FROM transport_Census00"
  dummy00 <- dbGetQuery(con, sel.statement00)
  
  # mysql data from ACS 2006-10
  dbListFields(con, "transport_ACS0610_iconv")
  # sel.statement <- "SELECT * from transport_ACS0610_iconv limit 5"
  sel.statement0610 <- "SELECT county_id_residence_3digit, county_id_workplace_3digit, state_id_residence_2digit, state_id_workplace_3digit, Number FROM transport_ACS0610_iconv"
  dummy0610 <- dbGetQuery(con, sel.statement0610)
  
  dbDisconnect(con)
  
  # clean data for 2000, apply to 2001-05
  output00 <- tbl_df(dummy00) %>%
    mutate(domesticWork = ifelse(Wrk_CO != "000", TRUE, FALSE)) %>% # work out of US
    filter(domesticWork) %>%
    mutate(fips_wrk = substr.Right(paste0(Wrk_ST, Wrk_CO), 5)) %>%
    mutate(fips_res = substr.Right(paste0(Res_ST, Res_CO), 5)) %>% 
    filter(fips_res != fips_wrk) %>%
    select(fips_res, fips_wrk, Count) %>%
    group_by(fips_wrk) %>%
    summarise(ct_2002 = sum(Count)) %>%
    mutate(ct_2003 = ct_2002, ct_2004 = ct_2002, ct_2005 = ct_2002) %>%
    gather(year, commutInflows_prep, ct_2002:ct_2005, convert = TRUE) %>%
    mutate(year = as.numeric(substr.Right(year, 4))) %>%
    ungroup %>%
    select(fips_wrk, year, commutInflows_prep)
  
  # clean data for 2006-10, apply to 2006-09
  output0610 <- tbl_df(dummy0610) %>%
    mutate(domesticWork = ifelse(county_id_workplace_3digit != "000", TRUE, FALSE)) %>% # work out of US
    filter(domesticWork) %>%
    mutate(fips_wrk = substr.Right(paste0(state_id_workplace_3digit, county_id_workplace_3digit), 5)) %>%
    mutate(fips_res = substr.Right(paste0(state_id_residence_2digit, county_id_residence_3digit), 5)) %>% # substr.Right should be redundant
    filter(fips_res != fips_wrk) %>%
    select(fips_res, fips_wrk, Number) %>%
    group_by(fips_wrk) %>%
    summarise(ct_2006 = sum(Number)) %>%
    mutate(ct_2007 = ct_2006, ct_2008 = ct_2006, ct_2009 = ct_2006) %>%
    gather(year, commutInflows_prep, ct_2006:ct_2009, convert = TRUE) %>%
    mutate(year = as.numeric(substr.Right(year, 4))) %>%
    ungroup %>%
    select(fips_wrk, year, commutInflows_prep)
  
  # combine the two datasets
  output <- bind_rows(output00, output0610) %>%
    filter(year >= 2002 & year <= 2009)
  
  return(output)
}
################################

cleanX_btsPassInflows_st <- function(){
  # clean flight passengers per population entering the state on average during flu months
  # will need to calculate incoming passengers per population in source_prepare_inlaData_st.R
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "transport_BTS0014_T100D_Market_All_Carrier")
  # sel.statement <- "SELECT * from transport_BTS0014_T100D_Market_All_Carrier limit 5"
  sel.statement <- "SELECT PASSENGERS, ORIGIN, ORIGIN_STATE_FIPS, DEST, DEST_STATE_FIPS, YEAR, MONTH from transport_BTS0014_T100D_Market_All_Carrier where (PASSENGERS > 0 and YEAR >= 2001 and YEAR <= 2009 and (MONTH <= 4 or MONTH >= 11))"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
 
  output <- tbl_df(dummy) %>%
    rename(pass = PASSENGERS, fips_origin = ORIGIN_STATE_FIPS, fips_dest = DEST_STATE_FIPS, year = YEAR, month = MONTH) %>%
    filter(!(year == 2001 & month <= 4)) %>%
    filter(!(year == 2009 & month >= 11)) %>%
    filter(fips_origin != fips_dest) %>%
    group_by(fips_dest, year, month) %>%
    summarise(pass = sum(pass, na.rm = TRUE)) %>%
    ungroup %>%
    mutate(season = ifelse(month <= 4, as.integer(substr.Right(year, 2)), as.integer(substr.Right(year, 2))+1)) %>%
    group_by(fips_dest, season) %>%
    summarise(pass_prep = mean(pass, na.rm = TRUE)) %>% 
    ungroup %>%
    select(season, fips_dest, pass_prep)
  
  return(output)
}
################################

cleanX_btsPassInflows_cty <- function(){
  # clean flight passengers per population entering the county on average during flu months
  # airports were assigned to the nearest county and counties without airports had no passengers entering
  # data were cleaned prior to upload to mysql (transport_BTS0014_T100D_Market_cty.R)
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "transport_BTS0014_T100D_Market_county")
  # sel.statement <- "SELECT * from transport_BTS0014_T100D_Market_county limit 5"
  sel.statement <- "SELECT nearestCty, season, pass from transport_BTS0014_T100D_Market_county"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
 
  output <- tbl_df(dummy) %>%
    rename(fips_dest = nearestCty) %>%
    select(season, fips_dest, pass) # here, not signaling to normalize by population because the underlying population is not entirely clear
  
  return(output)
}

##### flu-related ##########

cleanX_cdcFluview_fluPos_region <- function(){
  # percentage of laboratory samples that are positive
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "flu_cdcFluview9714_subtype_region")
  # sel.statement <- "SELECT * from flu_cdcFluview9714_subtype_region limit 5"
  sel.statement <- "SELECT season, region, fips, perc_fluPositive from flu_cdcFluview9714_subtype_region where season >= 2 AND season <= 9"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    rename(fluPos = perc_fluPositive)
  
  return(output)
}

################################

cleanX_cdcFluview_H3_region <- function(){
  # proportion of seasonal flu positives that are H3
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "flu_cdcFluview9714_subtype_region")
  # sel.statement <- "SELECT * from flu_cdcFluview9714_subtype_region limit 5"
  sel.statement <- "SELECT season, region, fips, prop_aAllH3 from flu_cdcFluview9714_subtype_region where season >= 2 and season <= 9"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    rename(H3 = prop_aAllH3)
  
  return(output)
}
################################

cleanX_nisInfantAnyVaxCov_st <- function(){
  # clean vaccination coverage by state (any level of flu vaccination)
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "immunity_nisBRFSSVaxCoverage_state")
  # sel.statement <- "SELECT * from immunity_nisBRFSSVaxCoverage_state limit 5"
  sel.statement <- "SELECT season, location, scale, vaxlevel, agegroup, coverage, interval95 from immunity_nisBRFSSVaxCoverage_state where scale = 'State' and season >= 2 and season <= 9 and agegroup = 'infant' and vaxlevel = 'anyvax'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    arrange(season, location) %>%
    select(season, location, coverage) %>%
    rename(infantAnyVax = coverage)
    
  return(output)
}
################################

cleanX_nisInfantFullVaxCov_st <- function(){
  # clean vaccination coverage by state (full 2 doses of flu vaccination)
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "immunity_nisBRFSSVaxCoverage_state")
  # sel.statement <- "SELECT * from immunity_nisBRFSSVaxCoverage_state limit 5"
  sel.statement <- "SELECT season, location, scale, vaxlevel, agegroup, coverage, interval95 from immunity_nisBRFSSVaxCoverage_state where scale = 'State' and season >= 2 and season <= 9 and agegroup = 'infant' and vaxlevel = 'fullvax'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    arrange(season, location) %>%
    select(season, location, coverage) %>%
    rename(infantFullVax = coverage)
  
  return(output)
}
################################

cleanX_brfssElderlyAnyVaxCov_st <- function(){
  # clean vaccination coverage by state (any level of flu vaccination)
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "immunity_nisBRFSSVaxCoverage_state")
  # sel.statement <- "SELECT * from immunity_nisBRFSSVaxCoverage_state limit 5"
  sel.statement <- "SELECT season, location, scale, vaxlevel, agegroup, coverage, interval95 from immunity_nisBRFSSVaxCoverage_state where scale = 'State' and season >= 2 and season <= 9 and agegroup = 'elderly' and vaxlevel = 'anyvax'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    arrange(season, location) %>%
    select(season, location, coverage) %>%
    rename(elderlyAnyVax = coverage)
  
  return(output)
}


##### environmental factors ##########
cleanX_noaanarrSpecHum_st <- function(){
  # clean average specific humidity near population-weighted centroid of the state during flu months (daily)
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "env_NOAANARR_specHum_state")
  # sel.statement <- "SELECT * from env_NOAANARR_specHum_state limit 5"
  sel.statement <- "SELECT fips, year, date as dayDate, humidity from env_NOAANARR_specHum_state where (date >= '2001-11-01' and date < '2009-05-01' and (MONTH(date) <= 4 or MONTH(date) >= 11))"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(season = as.numeric(substr.Right(as.character(year), 2))) %>%
    mutate(season = ifelse(as.numeric(substring(dayDate, 6, 7)) >= 11, season + 1, season)) %>%
    group_by(fips, season) %>%
    summarise(humidity = mean(humidity, na.rm = TRUE)) %>%
    ungroup
  
  return(output)
  
}
################################

cleanX_noaanarrSpecHum_cty <- function(){
  # clean average specific humidity near population-weighted centroid of the county during flu months (daily)
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "env_NOAANARR_specHum_county")
  # sel.statement <- "SELECT * from env_NOAANARR_specHum_county limit 5"
  sel.statement <- "SELECT fips, year, date as dayDate, humidity from env_NOAANARR_specHum_county where (date >= '2001-11-01' and date < '2009-05-01' and (MONTH(date) <= 4 or MONTH(date) >= 11))"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(season = as.numeric(substr.Right(as.character(year), 2))) %>%
    mutate(season = ifelse(as.numeric(substring(dayDate, 6, 7)) >= 11, season + 1, season)) %>%
    group_by(fips, season) %>%
    summarise(humidity = mean(humidity, na.rm = TRUE)) %>%
    ungroup
  
  return(output)
  
}
################################

cleanX_noaanarrSfcTemp_st <- function(){
  # clean average surface temperature near population-weighted centroid of the state during flu months (daily, Kelvin)
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "env_NOAANARR_sfcTemp_state")
  # sel.statement <- "SELECT * from env_NOAANARR_sfcTemp_state limit 5"
  sel.statement <- "SELECT fips, year, date as dayDate, temperature from env_NOAANARR_sfcTemp_state where (date >= '2001-11-01' and date < '2009-05-01' and (MONTH(date) <= 4 or MONTH(date) >= 11))"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(season = as.numeric(substr.Right(as.character(year), 2))) %>%
    mutate(season = ifelse(as.numeric(substring(dayDate, 6, 7)) >= 11, season + 1, season)) %>%
    group_by(fips, season) %>%
    summarise(temperature = mean(temperature, na.rm = TRUE)) %>%
    ungroup
  
  return(output)
  
}
################################

cleanX_noaanarrSfcTemp_cty <- function(){
  # clean average surface temperature near population-weighted centroid of the county during flu months (daily, Kelvin)
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "env_NOAANARR_sfcTemp_county")
  # sel.statement <- "SELECT * from env_NOAANARR_sfcTemp_county limit 5"
  sel.statement <- "SELECT fips, year, date as dayDate, temperature from env_NOAANARR_sfcTemp_county where (date >= '2001-11-01' and date < '2009-05-01' and (MONTH(date) <= 4 or MONTH(date) >= 11))"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(season = as.numeric(substr.Right(as.character(year), 2))) %>%
    mutate(season = ifelse(as.numeric(substring(dayDate, 6, 7)) >= 11, season + 1, season)) %>%
    group_by(fips, season) %>%
    summarise(temperature = mean(temperature, na.rm = TRUE)) %>%
    ungroup
  
  return(output)
  
}

#### testing area ################################

# # all county tables
# sahieIns_cty_df <- cleanO_sahieInsured_cty()
# saipePov_cty_df <- cleanX_saipePoverty_cty()
# saipeInc_cty_df <- cleanX_saipeIncome_cty()
# ahrfMcaid_cty_df <- cleanX_ahrfMedicaidEligibles_cty()
# ahrfMcare_cty_df <- cleanX_ahrfMedicareEligibles_cty()
# censusInfTodPop_cty_df <- cleanX_censusInfantToddlerPop_cty()
# censusChPop_cty_df <- cleanX_censusChildPop_cty()
# censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
# censusEldPop_cty_df <- cleanX_censusElderlyPop_cty()
# ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
# ahrfPhys_cty_df <- cleanX_ahrfPhysicians_cty()
# popDens_cty_df <- cleanX_popDensity_cty()
# housDens_cty_df <- cleanX_housDensity_cty()
# acsCommutInflows_cty_prep <- cleanX_acsCommutInflows_cty()
# btsPass_cty_df <- cleanX_btsPassInflows_cty()
# narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
# narrSfcTemp_cty_df <- cleanX_noaanarrSfcTemp_cty()
 
# # all region tables
# cdcFluPos_df <- cleanX_cdcFluview_fluPos_region()
# cdcH3_df <- cleanX_cdcFluview_H3_region()

# # all state tables
# cpsasecInsured_df <- cleanO_cpsasecInsured_st()
# saipePoverty_df <- cleanX_saipePoverty_st()
# saipeIncome_df <- cleanX_saipeIncome_st() 
# ahrfMcaid_df <- cleanX_ahrfMedicaidEligibles_st()
# ahrfMcare_df <- cleanX_ahrfMedicareEligibles_st()
# censusInfPop_df <- cleanX_censusInfantPop_st()
# censusTodPop_df <- cleanX_censusToddlerPop_st()
# censusChPop_df <- cleanX_censusChildPop_st()
# censusAdPop_df <- cleanX_censusAdultPop_st()
# censusEldPop_df <- cleanX_censusElderlyPop_st()
# ahrfHospAccess_df <- cleanX_ahrfHospitals_st() 
# ahrfPhys_df <- cleanX_ahrfPhysicians_st()
# popDens_df <- cleanX_popDensity_st()
# housDens_df <- cleanX_housDensity_st()
# acsCommut_prep <- cleanX_acsCommutInflows_st()
# btsPass_prep <- cleanX_btsPassInflows_st()
# infantAnyVax_df <- cleanX_nisInfantAnyVaxCov_st()
# infantFullVax_df <- cleanX_nisInfantFullVaxCov_st()
# elderlyAnyVax_df <- cleanX_brfssElderlyAnyVaxCov_st()
# narrSpecHum_df <- cleanX_noaanarrSpecHum_st()
# narrSfc_df <- cleanX_noaanarrSfcTemp_st()



# To do:
#   prior immunity from last year's seasonal burden
#   skip spatialcw tables -- these are just crosswalks between different areal units




