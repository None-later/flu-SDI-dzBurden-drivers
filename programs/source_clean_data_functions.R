
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
  iliSum_data <- read_csv(filepathList$path_response_st, col_types = "iccd") %>%
    filter(metric == sprintf("%s.sum", dbCode)) %>%
    select(-metric) %>%
    rename(y = burden, abbr = state)
  
  pop_data <- clean_pop_st(filepathList) # 4/12/16 all 51 pops are there
  
  return_data <- full_join(iliSum_data, pop_data, by = c("season", "abbr")) %>% # 4/12/16 full_join so pops don't drop
    select(fips, abbr, state, lat, lon, season, year, pop, y) %>% 
    group_by(season) %>%
    mutate(E = weighted.mean(y, pop, na.rm=TRUE)) %>%
    ungroup %>%
    mutate(logy = log(y), logE = log(E))
  
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
  # sel.head.uninsured <- "Select * from HI_CPSasec_state limit 5"
  sel.statement.insured <- "SELECT year, state_id AS fips, Percent_covered/100 AS insured_prop FROM HI_CPSasec_state WHERE type = 'state'"
  dummy <- dbGetQuery(con, sel.statement.insured)
 
  dbDisconnect(con)
  
  insured <- tbl_df(dummy) %>%
    rename(insured = insured_prop) %>%
    select(fips, year, insured) 
  
  return(insured)
}


##### DRIVER DATA ##########################################

##### social determinants ##########
cleanX_saipePoverty_st <- function(){
  # clean SAIPE percentage of population in poverty data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "SAIPE_poverty")
  # sel.head.poverty <- "SELECT * from SAIPE_poverty limit 5"
  sel.statement.poverty <- "SELECT year, state_id AS fips, all_poverty_percent/100 AS inPoverty_prop FROM SAIPE_poverty WHERE type = 'state'"
  dummy <- dbGetQuery(con, sel.statement.poverty)
  
  dbDisconnect(con)
  
  poverty <- tbl_df(dummy) %>%
    rename(poverty = inPoverty_prop) %>%
    select(fips, year, poverty)
  
  return(poverty)
}
################################
cleanX_saipeIncome_st <- function(){
  # clean SAIPE median household income data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "SAIPE_income")
  # sel.head.income <- "SELECT * from SAIPE_income limit 5"
  sel.statement.income <- "SELECT year, state_id AS fips, med_income as medianIncome FROM SAIPE_income WHERE type = 'state'"
  dummy <- dbGetQuery(con, sel.statement.income)
  
  dbDisconnect(con)
  
  income <- tbl_df(dummy) %>%
    rename(income = medianIncome) %>%
    select(fips, year, income) 
  
  return(income)
}
################################

cleanX_ahrfMedicaidEligibles_st <- function(){
  # clean AHRF Medicaid eligibility data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "access_medicare_medicaid")
  # sel.statement.medicaid <- "SELECT * from access_medicare_medicaid limit 5"
  sel.statement.medicaid <- "SELECT year, FIPS AS fips_cty, (mcaid_child + mcaid_adult) AS mcaidEligTot, population as pop FROM access_medicare_medicaid"
  dummy <- dbGetQuery(con, sel.statement.medicaid)
  
  dbDisconnect(con)
  
  mcaidElig <- tbl_df(dummy) %>%
    mutate(fips = substring(fips_cty, 1, 2)) %>%
    group_by(fips, year) %>%
    summarise(mcaidEligTot = sum(mcaidEligTot, na.rm=TRUE), pop = sum(pop, na.rm=TRUE)) %>%
    mutate(mcaidEligTot = ifelse(mcaidEligTot == 0, NA, mcaidEligTot)) %>%
    mutate(mcaidElig = mcaidEligTot/pop) %>%
    filter(year %in% 2004:2008) %>% # 3/2/16: 2005 mcaidElig is NA if years before 2004 are included; not sure why
    select(fips, year, mcaidElig) 
  
  return(mcaidElig)
}


################################


##### demography ##########
cleanX_ahrfMedicareEligibles_st <- function(){
  # clean AHRF Medicare eligibility data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "access_medicare_medicaid")
  # sel.statement.medicare <- "SELECT * from access_medicare_medicaid limit 5"
  sel.statement.medicare <- "SELECT year, FIPS AS fips_cty, mdcr_elig AS mcareEligTot, population AS pop FROM access_medicare_medicaid"
  dummy <- dbGetQuery(con, sel.statement.medicare)
  
  dbDisconnect(con)
  
  mcareElig <- tbl_df(dummy) %>%
    mutate(fips = substring(fips_cty, 1, 2)) %>%
    group_by(fips, year) %>%
    summarise(mcareEligTot = sum(mcareEligTot, na.rm=TRUE), pop = sum(pop, na.rm=TRUE)) %>%
    mutate(mcareEligTot = ifelse(mcareEligTot == 0, NA, mcareEligTot)) %>%
    mutate(mcareElig = mcareEligTot/pop) %>%
    select(fips, year, mcareElig) 
  
  return(mcareElig)
}


################################

##### access to care ##########
cleanX_ahrfHospitals_st <- function(){
  # clean AHRF hospitals per pop data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "AHRF_access")
  # sel.statement.ahrfHospitals <- "SELECT * from AHRF_access limit 5"
  sel.statement.ahrfHospitals <- "SELECT year, FIPS AS fips_cty, hosp, population AS pop FROM AHRF_access"
  dummy <- dbGetQuery(con, sel.statement.ahrfHospitals)
  
  dbDisconnect(con)
  
  hospitalAccess <- tbl_df(dummy) %>%
    mutate(fips = substring(fips_cty, 1, 2)) %>%
    group_by(fips, year) %>%
    summarise(hospCt = sum(hosp, na.rm=TRUE), pop = sum(pop, na.rm=TRUE)) %>%
    mutate(hospCt = ifelse(hospCt == 0, NA, hospCt)) %>%
    mutate(hospitalAccess = hospCt/pop) %>%
    select(fips, year, hospitalAccess) 
  
  return(hospitalAccess)
}
################################

cleanX_ahrfPhysicians_st <- function(){
  # clean AHRF physicians per pop data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "AHRF_access")
  # sel.statement.ahrfPhysicians <- "SELECT * from AHRF_access limit 5"
  sel.statement.ahrfPhysicians <- "SELECT year, FIPS AS fips_cty, physicians, population AS pop FROM AHRF_access"
  dummy <- dbGetQuery(con, sel.statement.ahrfPhysicians)
  
  dbDisconnect(con)
  
  physicianAccess <- tbl_df(dummy) %>%
    mutate(fips = substring(fips_cty, 1, 2)) %>%
    group_by(fips, year) %>%
    summarise(physCt = sum(physicians, na.rm=TRUE), pop = sum(pop, na.rm=TRUE)) %>%
    mutate(physCt = ifelse(physCt == 0, NA, physCt)) %>%
    mutate(physicianAccess = physCt/pop) %>%
    select(fips, year, physicianAccess) 
  
  return(physicianAccess)
}
################################

# cleanX_hpsa_st <- function(){
#   # clean HPSA data exported from mysql
#   print(match.call())
#   
#   con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
#   dbListTables(con)
#   
#   dbListFields(con, "HPSA_PC")
#   sel.statement.hpsa <- "SELECT * from HPSA_PC limit 5"
#   # sel.statement.hpsa <- "SELECT year, FIPS AS fips_cty, hosp, population AS pop FROM AHRF_access"
#   dummy <- dbGetQuery(con, sel.statement.hpsa)
#   
#   dbDisconnect(con)
#   
# #   hospitalAccess <- tbl_df(dummy) %>%
# #     mutate(fips = substring(fips_cty, 1, 2)) %>%
# #     group_by(fips, year) %>%
# #     summarise(hospCt = sum(hosp, na.rm=TRUE), pop = sum(pop, na.rm=TRUE)) %>%
# #     mutate(hospCt = ifelse(hospCt == 0, NA, hospCt)) %>%
# #     mutate(hospPerPop = hospCt/pop) %>%
# #     group_by(year) %>%
# #     mutate(hospitalAccess = centerStandardize(log(hospPerPop))) %>%
# #     select(fips, year, hospitalAccess) %>%
# #     ungroup
#   
#   return(dummy)
# }
################################


##### contact/travel patterns ##########
cleanX_popDensity_st <- function(){
  # clean population density data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "Census_popdensity_county")
  # sel.statement.censusPopdensity <- "SELECT * from Census_popdensity_county limit 5"
  sel.statement.censusPopdensity <- "SELECT year, fips_st AS fips, popDens_land FROM Census_popdensity_county WHERE type = 'state'"
  dummy <- dbGetQuery(con, sel.statement.censusPopdensity)
  
  dbDisconnect(con)
  
  popDensity <- tbl_df(dummy) %>%
    rename(popDensity = popDens_land) %>%
    select(fips, year, popDensity) 
  
  return(popDensity)
}
################################

cleanX_housDensity_st <- function(){
  # clean housing unit density data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "Census_popdensity_county")
  # sel.statement.censusHousdensity <- "SELECT * from Census_popdensity_county limit 5"
  sel.statement.censusHousdensity <- "SELECT year, fips_st AS fips, housDens_land FROM Census_popdensity_county WHERE type = 'state'"
  dummy <- dbGetQuery(con, sel.statement.censusHousdensity)
  
  dbDisconnect(con)
  
  housDensity <- tbl_df(dummy) %>%
    rename(housDensity = housDens_land) %>%
    select(fips, year, housDensity) 
  
  return(housDensity)
}
################################

cleanX_acsCommutInflows_st <- function(){
  # clean out-of-state commuters per population entering the state
  # will need to calculate incoming commuters per population in source_prepare_inlaData_st.R
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "transport_ACS0610_iconv")
  # sel.statement.acsCommutingInflows <- "SELECT * from transport_ACS0610_iconv limit 5"
  sel.statement.acsCommutingInflows <- "SELECT state_id_residence_2digit, county_id_residence_3digit, state_id_workplace_3digit, state_workplace, Number FROM transport_ACS0610_iconv"
  dummy <- dbGetQuery(con, sel.statement.acsCommutingInflows)
  
  dbDisconnect(con)
  
  commutInflows <- tbl_df(dummy) %>%
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
  
  return(commutInflows)
}
################################

cleanX_btsPassInflows_st <- function(){
  # clean out-of-state commuters per population entering the state on average during flu months
  # will need to calculate incoming commuters per population in source_prepare_inlaData_st.R
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "transport_BTS0014_T100D_Market_All_Carrier")
  # sel.statement.btsPassInflows <- "SELECT * from transport_BTS0014_T100D_Market_All_Carrier limit 5"
  sel.statement.btsPassInflows <- "SELECT PASSENGERS, ORIGIN, ORIGIN_STATE_FIPS, DEST, DEST_STATE_FIPS, YEAR, MONTH from transport_BTS0014_T100D_Market_All_Carrier where (PASSENGERS > 0 and YEAR >= 2001 and YEAR <= 2009 and (MONTH <= 4 or MONTH >= 11))"
  dummy <- dbGetQuery(con, sel.statement.btsPassInflows)
  
  dbDisconnect(con)
 
  passInflows <- tbl_df(dummy) %>%
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
  
  return(passInflows)
}

##### immunity ##########
## 3/30/16 - Kristofer is cleaning a new version of this data which will need to be uploaded into mysql


################################

##### environmental factors ##########
cleanX_noaanarrSpecHum_st <- function(){
  # clean average specific humidity near population-weighted centroid of the state during flu months (daily)
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "env_NOAANARR_specHum_state")
  # sel.statement.noaanarrSpecHum <- "SELECT * from env_NOAANARR_specHum_state limit 5"
  sel.statement.noaanarrSpecHum <- "SELECT fips, year, date as dayDate, humidity from env_NOAANARR_specHum_state where (date >= '2001-11-01' and date < '2009-05-01' and (MONTH(date) <= 4 or MONTH(date) >= 11))"
  dummy <- dbGetQuery(con, sel.statement.noaanarrSpecHum)
  
  dbDisconnect(con)
  
  specHum <- tbl_df(dummy) %>%
    mutate(season = as.numeric(substr.Right(as.character(year), 2))) %>%
    mutate(season = ifelse(as.numeric(substring(dayDate, 6, 7)) >= 11, season + 1, season)) %>%
    group_by(fips, season) %>%
    summarise(humidity = mean(humidity, na.rm = TRUE)) %>%
    ungroup
          
  
  return(specHum)
  
}
################################

cleanX_noaanarrSfcTemp_st <- function(){
  # clean average surface temperature near population-weighted centroid of the state during flu months (daily, Kelvin)
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "env_NOAANARR_sfcTemp_state")
  # sel.statement.noaanarrSfcTemp <- "SELECT * from env_NOAANARR_sfcTemp_state limit 5"
  sel.statement.noaanarrSfcTemp <- "SELECT fips, year, date as dayDate, temperature from env_NOAANARR_sfcTemp_state where (date >= '2001-11-01' and date < '2009-05-01' and (MONTH(date) <= 4 or MONTH(date) >= 11))"
  dummy <- dbGetQuery(con, sel.statement.noaanarrSfcTemp)
  
  dbDisconnect(con)
  
  sfcTemp <- tbl_df(dummy) %>%
    mutate(season = as.numeric(substr.Right(as.character(year), 2))) %>%
    mutate(season = ifelse(as.numeric(substring(dayDate, 6, 7)) >= 11, season + 1, season)) %>%
    group_by(fips, season) %>%
    summarise(temperature = mean(temperature, na.rm = TRUE)) %>%
    ungroup
  
  return(sfcTemp)
  
}

#### testing area ################################
# test <- cleanX_noaanarrSfcTemp_st()


# To do:
#   cleanX_hpsa_st -- uncertain aggregation
#   cleanX_nhfspufFluVax_st -- 09-10 flu vax surveys only
#   cleanX_nisteenFluVax_st -- uncertain aggregation
#   cleanX_nisFluVax_st -- uncertain aggregation
#   prior immunity from last year's seasonal burden
#   skip spatialcw tables -- these are just crosswalks between different areal units




