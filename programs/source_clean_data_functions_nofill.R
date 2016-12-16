
## Name: Elizabeth Lee
## Date: 12/14/16
## Function: Functions for cleaning response and covariate data for INLA - alternate versions of functions in source_clean_data_functions.R. These versions do not fill in predictor data where they were NA in the original dataset. I've also removed larger aggregations of data when data were collected on multiple spatial scales, for conciseness.
## Filenames: 
## Data Source: 
## Notes: 12/15/16: Rm year restrictions in SQL queries and data cleaning as much as possible, so data for future years may be more readily gathered
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### functions for model data cleaning ################################
require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL)


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
################################  
  

##### SAMPLING EFFORT DATA ##########################################

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
  
  origDat <- bind_rows(dummy2, dummy) %>%
    dplyr::rename(insured = insured_prop) %>%
    select(fips, year, insured) 
  
  # 12/15/16 rm duplicate data cleaning
  output <- origDat
  
  return(output)
}

##### DRIVER DATA ##########################################

##### social determinants ##########

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

cleanX_ahrfMedicaidEligibles_cty <- function(){
  # clean AHRF Medicaid eligibility data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "access_medicare_medicaid")
  # sel.statement <- "SELECT * from access_medicare_medicaid limit 5"
  sel.statement <- "SELECT year, FIPS AS fips, (mcaid_child + mcaid_adult) AS mcaidEligTot, population as pop FROM access_medicare_medicaid"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(mcaidElig = mcaidEligTot/pop) %>%
    filter(year %in% 2004:2008) %>% # all other available years are only NAs
    select(fips, year, mcaidElig) 
  
  return(output)
}


##### demography ##########
################################

cleanX_ahrfMedicareEligibles_cty <- function(){
  # clean AHRF Medicare eligibility data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "access_medicare_medicaid")
  # sel.statement <- "SELECT * from access_medicare_medicaid limit 5"
  sel.statement <- "SELECT year, FIPS AS fips, mdcr_elig AS mcareEligTot, population AS pop FROM access_medicare_medicaid"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(mcareElig = mcareEligTot/pop) %>%
    filter(!(year %in% 2006:2007)) %>%
    select(fips, year, mcareElig) 
  
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
  sel.statement <- "SELECT year, fips, scale, agegroup, pop FROM demog_Census_agePop_county WHERE scale = 'county' AND (agegroup = 'infant/toddler' OR agegroup = 'total')"
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

cleanX_censusChildPop_cty <- function(){
  # clean Census population data for 5-19 yo, exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "demog_Census_agePop_county")
  # sel.statement.child <- "SELECT * from demog_Census_agePop_county limit 5"
  sel.statement.child <- "SELECT year, fips, scale, agegroup, pop FROM demog_Census_agePop_county WHERE scale = 'county' AND (agegroup = 'child' OR agegroup = 'total')"
  dummy <- dbGetQuery(con, sel.statement.child)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    spread(agegroup, pop) %>%
    mutate(child = child/total) %>%
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
  sel.statement <- "SELECT year, fips, scale, agegroup, pop FROM demog_Census_agePop_county WHERE scale = 'county' AND (agegroup = 'adult' OR agegroup = 'total')"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    spread(agegroup, pop) %>%
    mutate(adult = adult/total) %>%
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
  sel.statement <- "SELECT year, fips, scale, agegroup, pop FROM demog_Census_agePop_county WHERE scale = 'county' AND (agegroup = 'elderly' OR agegroup = 'total')"
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

cleanX_ahrfHospitals_cty <- function(){
  # clean AHRF hospitals per pop data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "AHRF_access")
  # sel.statement <- "SELECT * from AHRF_access limit 5"
  sel.statement <- "SELECT year, FIPS AS fips, hosp, population AS pop FROM AHRF_access"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(hospitalAccess = hosp/pop) %>%
    filter(!(year %in% 2013:2014)) %>% # rm years with all NAs
    select(fips, year, hospitalAccess) 
  
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
  sel.statement <- "SELECT year, FIPS AS fips, physicians, population AS pop FROM AHRF_access"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(physicianAccess = physicians/pop) %>%
    filter(!(year %in% c(2009, 2014))) %>% # rm years with all NAs
    select(fips, year, physicianAccess) 
  
  return(output)
}
################################

cleanX_brfssMedCost_cty <- function(){
  # clean BRFSS data on whether survey respondents could not see the doctor in the last 12 months due to cost
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "medcost_brfss0414_5yr_county")
  # sel.statement <- "SELECT * from medcost_brfss0414_5yr_county limit 5"
  sel.statement <- "SELECT fips, timeframe, level, medcost from medcost_brfss0414_5yr_county WHERE level = 'County'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  dummy2 <- tbl_df(dummy) %>% 
    mutate(timeframe = ifelse(timeframe == '2006-2012', '2007', timeframe)) %>%
    mutate(year = as.integer(substr(timeframe, 1, 4))) %>%
    select(fips, year, medcost)
  
  output <- dummy2 %>% 
    arrange(fips, year) 
  # missing counties in each year, data avail: 2004-08, 10
  
  return(output)
  
}
################################

##### contact/travel patterns ##########

cleanX_popDensity_cty <- function(){
  # clean population density data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "Census_popdensity_county")
  # sel.statement <- "SELECT * from Census_popdensity_county limit 5"
  sel.statement <- "SELECT year, fips, popDens_land FROM Census_popdensity_county WHERE type = 'county'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    rename(popDensity = popDens_land) %>%
    select(fips, year, popDensity) %>%
    filter(year != 0) %>% # rm odd data error
    arrange(fips, year)
  
  return(output)
}
################################

cleanX_housDensity_cty <- function(){
  # clean pop per housing unit density per land area data exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "Census_popdensity_county")
  # sel.statement <- "SELECT * from Census_popdensity_county limit 5"
  sel.statement <- "SELECT year, fips, popDens_housing FROM Census_popdensity_county WHERE type = 'county'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    rename(housDensity = popDens_housing) %>%
    select(fips, year, housDensity) %>%
    filter(year != 0) %>% # rm odd data error
    arrange(fips, year)
  
  return(output)
}
################################

cleanX_acsAvgHHSize_cty <- function(){
  # clean ACS surveyed average household size (5-year estimates have better county coverage) exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "contact_avgHHSize_ACS0514_5yr_county")
  # sel.statement <- "SELECT * from contact_avgHHSize_ACS0514_5yr_county limit 5"
  sel.statement <- "SELECT fips, timeframe, avgHHSize FROM contact_avgHHSize_ACS0514_5yr_county"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(year = as.numeric(substr(timeframe, 1, 4))) %>%
    select(fips, year, avgHHSize) %>%
    arrange(fips, year)
  # data avail 2005-10

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
    arrange(fips_wrk, year) # rm fillValues_years step
  
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
  # roughly 275 fips_dest with airports per year
  
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
  sel.statement <- "SELECT season, region, fips, perc_fluPositive from flu_cdcFluview9714_subtype_region"
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
  sel.statement <- "SELECT season, region, fips, prop_H3_all from flu_cdcFluview9714_subtype_region"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    rename(H3 = prop_H3_all)
  
  return(output)
}
################################

cleanX_cdcFluview_H3A_region <- function(){
  # proportion of seasonal flu positive A subtypes that are H3
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "flu_cdcFluview9714_subtype_region")
  # sel.statement <- "SELECT * from flu_cdcFluview9714_subtype_region limit 5"
  sel.statement <- "SELECT season, region, fips, prop_H3_a from flu_cdcFluview9714_subtype_region"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) 
  
  return(output)
}
################################

cleanX_cdcFluview_B_region <- function(){
  # proportion of seasonal flu positives that are B
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "flu_cdcFluview9714_subtype_region")
  # sel.statement <- "SELECT * from flu_cdcFluview9714_subtype_region limit 5"
  sel.statement <- "SELECT season, region, fips, prop_b_all from flu_cdcFluview9714_subtype_region"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) 
  
  return(output)
}
################################

cleanX_multsrcSubtypeDistrStrainSim_reg <- function(){
  # clean previous and current season subtype/type (H1/H3/B) distribution by region and strain similarity between seasons -- create new variable for "proportion of individuals infected in the previous flu season that will have protection during this year, according to the current distribution of subtypes/types"
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "subtypeDistribution_hiAssayStrainSimilarity_multSources_region")
  # sel.statement <- "SELECT * from subtypeDistribution_hiAssayStrainSimilarity_multSources_region limit 5"
  sel.statement <- "SELECT year, region, category, prevYrProportion as prevProp, strainSimilarity as strainSim, proportion as currProp from subtypeDistribution_hiAssayStrainSimilarity_multSources_region"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  dummy2 <- tbl_df(dummy) %>%
    rowwise %>%
    mutate(estImmuneProp = prod(prevProp, strainSim, currProp)) %>%
    ungroup
  output <- dummy2 %>%
    group_by(year, region) %>%
    summarise(estImmuneProp = sum(estImmuneProp)) %>%
    ungroup %>%
    mutate(season = year-2000) %>%
    mutate(region = as.numeric(region)) %>%
    filter(!(season %in% c(-2, 10, 14))) %>% # rm seasons with all NA data
    select(season, region, estImmuneProp)
  
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
  sel.statement <- "SELECT season, state AS st, scale, vaxlevel, agegroup, coverage, interval95 from immunity_nisBRFSSVaxCoverage_state where scale = 'State' and agegroup = 'infant' and vaxlevel = 'anyvax'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  origDat <- tbl_df(dummy) %>%
    select(season, st, coverage) %>%
    mutate(season = paste0("S", season)) %>%
    spread(season, coverage) %>% 
    mutate(S5 = ifelse(st == "UT", (S4+S6)/2, S5)) %>% # 9/27/16 decisions to average adjacent season for missing S5 UT data
    mutate(S6 = ifelse(st == "DE", (S5+S7)/2, S6)) %>% # 9/27/16 decisions to average adjacent season for missing S6 DE data
    gather(season, infantAnyVax, S3:S9) %>%
    mutate(season = as.numeric(substring(season, 2, 2)))
  
  output <- origDat %>%
    arrange(season, st)
    
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
  sel.statement <- "SELECT season, state AS st, scale, vaxlevel, agegroup, coverage, interval95 from immunity_nisBRFSSVaxCoverage_state where scale = 'State' and agegroup = 'infant' and vaxlevel = 'fullvax'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  origDat <- tbl_df(dummy) %>%
    select(season, st, coverage) %>%
    rename(infantFullVax = coverage)
  
  output <- origDat %>%
    arrange(season, st)
  
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
  sel.statement <- "SELECT season, state AS st, scale, vaxlevel, agegroup, coverage, interval95 from immunity_nisBRFSSVaxCoverage_state where scale = 'State' and agegroup = 'elderly' and vaxlevel = 'anyvax'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  origDat <- tbl_df(dummy) %>%
    select(season, st, coverage) %>%
    rename(elderlyAnyVax = coverage)
  
  output <- origDat %>%
    arrange(season, st)
  
  return(output)
}

################################

cleanX_brfssAdultAnyVaxCov_st <- function(){
  # clean adult vaccination coverage by state (any level of flu vaccination)
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "immunity_nisBRFSSVaxCoverage_state")
  # sel.statement <- "SELECT * from immunity_nisBRFSSVaxCoverage_state limit 5"
  sel.statement <- "SELECT season, state AS st, scale, vaxlevel, agegroup, coverage, interval95 from immunity_nisBRFSSVaxCoverage_state where scale = 'State' and agegroup = 'adult' and vaxlevel = 'anyvax'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    select(season, st, coverage) %>%
    rename(adultAnyVax = coverage) %>%
    arrange(season, st)
  
  return(output)
}

##### environmental factors ##########

cleanX_noaanarrSpecHum_cty <- function(){
  # clean average specific humidity near population-weighted centroid of the county during flu months (daily)
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "env_NOAANARR_specHum_county")
  # sel.statement <- "SELECT * from env_NOAANARR_specHum_county limit 5"
  sel.statement <- "SELECT fips, year, date as dayDate, humidity from env_NOAANARR_specHum_county where (MONTH(date) <= 4 or MONTH(date) >= 11)"
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

cleanX_noaanarrSfcTemp_cty <- function(){
  # clean average surface temperature near population-weighted centroid of the county during flu months (daily, Kelvin)
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "env_NOAANARR_sfcTemp_county")
  # sel.statement <- "SELECT * from env_NOAANARR_sfcTemp_county limit 5"
  sel.statement <- "SELECT fips, year, date as dayDate, temperature from env_NOAANARR_sfcTemp_county where MONTH(date) <= 4 or MONTH(date) >= 11"
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

cleanX_wonderAirParticulateMatter_cty <- function(){
  # clean data on fine particulate matter (air pollution) with aerodynamic diameter < 2.5 micrometers by county, which was aggregated from 10 km square grids (monthly, micrograms/meter^3); monthly data are averages of daily observations
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "airpollution_wonder0311_county")
  # sel.statement <- "SELECT * from airpollution_wonder0311_county limit 5"
  sel.statement <- "SELECT fips, season, month, avg_pm from airpollution_wonder0311_county where month <= 4 or month >= 11"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  dummy2 <- tbl_df(dummy) %>%
    group_by(fips, season) %>%
    summarise(avg_pm = mean(avg_pm, na.rm = TRUE)) %>%
    ungroup %>%
    mutate(fips = ifelse(fips == "12025", "12086", fips)) # Miami-Dade, FL renumbered

  # Broomfield county, CO (fips 08014) was created in 2001 from 08001, 08013, 08059, 081233 -- avg these to get 08014 data
  # 12/15/16: Broomfield county, CO was removed from popdensity & commute data bc Census data sources existed only in 2000
  fips08014 <- dummy2 %>%
    filter(fips %in% c("08013", "08001", "08059", "08123")) %>%
    group_by(season) %>% 
    summarise(avg_pm = mean(avg_pm, na.rm = TRUE)) %>%
    ungroup %>%
    mutate(fips = "08014") %>%
    select(fips, season, avg_pm)
  
  output <- bind_rows(dummy2, fips08014) %>%
    arrange(fips, season)
  
  return(output)
}

##### social cohesion ##########

cleanX_cbpSocialAssoc_cty <- function(){
  # clean Census Business Patterns data on social assoc/orgs by NAICS code per 10,000 pop
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "socialcohesion_socialassoc_CBP9814_county")
  # sel.statement <- "SELECT * from socialcohesion_socialassoc_CBP9814_county limit 5"
  sel.statement <- "SELECT fips, year, orgType, socialassoc from socialcohesion_socialassoc_CBP9814_county"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  popDat <- clean_pop_cty_plain()
  
  output <- tbl_df(dummy) %>%
    group_by(fips, year) %>%
    summarise(socialOrgs = sum(socialassoc, na.rm = TRUE)) %>%
    ungroup %>%
    full_join(popDat, by = c("fips", "year")) %>%
    mutate(socialOrgs = socialOrgs/pop*10000) %>%
    select(fips, year, socialOrgs) %>%
    mutate(socialOrgs = ifelse(is.na(socialOrgs), 0, socialOrgs)) %>% # some fips don't have social associations according to cbp database
    arrange(fips, year)
  
  return(output)
  
}
################################

cleanX_acsOneParentFamHH_cty <- function(){
  # clean ACS data on single parent family households, county-level data available only with 5-year estimates; first year of timeframe is the year assigned to data
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "socialcohesion_oneParentFamHH_ACS0514_5yr_county")
  # sel.statement <- "SELECT * from socialcohesion_oneParentFamHH_ACS0514_5yr_county limit 5"
  sel.statement <- "SELECT fips, timeframe, prop_oneParentFamHH from socialcohesion_oneParentFamHH_ACS0514_5yr_county"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>% 
    mutate(year = as.integer(substr(timeframe, 1, 4))) %>%
    select(fips, year, prop_oneParentFamHH) %>%
    arrange(fips, year)
  
  return(output)
  
}
################################

cleanX_acsOnePersonHH_cty <- function(){
  # clean ACS data on single person households, county-level data available only with 5-year estimates; first year of timeframe is the year assigned to data
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "socialcohesion_onePersonHH_ACS0514_5yr_county")
  # sel.statement <- "SELECT * from socialcohesion_onePersonHH_ACS0514_5yr_county limit 5"
  sel.statement <- "SELECT fips, timeframe, perc_hh_1p from socialcohesion_onePersonHH_ACS0514_5yr_county"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>% 
    mutate(year = as.integer(substr(timeframe, 1, 4))) %>%
    select(fips, year, perc_hh_1p) %>%
    arrange(fips, year)
  
  return(output)
  
}
################################

##### population health status (susceptibility) ##########

cleanX_brfssPoorHealth_cty <- function(){
  # clean BRFSS data on respondents with poor or fair health, county-level data available only with 5-year estimates; first year of timeframe is the year assigned to data
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "susceptibility_poorhealth_brfss0212_county")
  # sel.statement <- "SELECT * from susceptibility_poorhealth_brfss0212_county limit 5"
  sel.statement <- "SELECT fips, timeframe, level, poorhealth from susceptibility_poorhealth_brfss0212_county WHERE level = 'County' and timeframe = '2003-2009'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  # 12/15/16 all years should have data from 2003-2009 timeframe
  dupDat <- tbl_df(dummy) %>% 
    mutate(year = as.integer(substr(timeframe, 1, 4))) %>%
    select(fips, year, poorhealth)
  
  output <- bind_rows(dupDat, dupDat %>% mutate(year = 2004), dupDat %>% mutate(year = 2005), dupDat %>% mutate(year = 2006), dupDat %>% mutate(year = 2007), dupDat %>% mutate(year = 2008), dupDat %>% mutate(year = 2009)) %>%
    arrange(fips, year)
  
  return(output)
  
}
################################

cleanX_brfssUnhealthyDays_cty <- function(){
  # clean BRFSS data on average number of physically unhealthy days in the past 30 days among respondents, county-level data available only with 5-year estimates; first year of timeframe is the year assigned to data
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "susceptibility_unhealthydays_brfss0212_county")
  # sel.statement <- "SELECT * from susceptibility_unhealthydays_brfss0212_county limit 5"
  sel.statement <- "SELECT fips, timeframe, level, unhealthydays from susceptibility_unhealthydays_brfss0212_county WHERE level = 'County' and timeframe = '2003-2009'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  # 12/15/16 all years should have data from 2003-2009 timeframe
  dupDat <- tbl_df(dummy) %>% 
    mutate(year = as.integer(substr(timeframe, 1, 4))) %>%
    select(fips, year, unhealthydays)
  
  output <- bind_rows(dupDat, dupDat %>% mutate(year = 2004), dupDat %>% mutate(year = 2005), dupDat %>% mutate(year = 2006), dupDat %>% mutate(year = 2007), dupDat %>% mutate(year = 2008), dupDat %>% mutate(year = 2009)) %>%
    arrange(fips, year)
  
  return(output)
  
}
################################

#### broad data cleaning functions ################################
fillValues_years <- function(.data, varname){
  # create function to automate the process of populating values with the subsequent then prior year's values
  # years x2003-x2009 are the columns, one row per spatial unit
  print(match.call())
  
  .data %>%
    filter(year >= 2003 & year <= 2009) %>%
    mutate(year = paste0("x", year)) %>%
    spread_("year", varname) %>%
    mutate(x2004 = ifelse(is.na(x2004), x2003, x2004)) %>%
    mutate(x2005 = ifelse(is.na(x2005), x2004, x2005)) %>%
    mutate(x2006 = ifelse(is.na(x2006), x2005, x2006)) %>%
    mutate(x2007 = ifelse(is.na(x2007), x2006, x2007)) %>%
    mutate(x2008 = ifelse(is.na(x2008), x2007, x2008)) %>%
    mutate(x2009 = ifelse(is.na(x2009), x2008, x2009)) %>%
    mutate(x2008 = ifelse(is.na(x2008), x2009, x2008)) %>%
    mutate(x2007 = ifelse(is.na(x2007), x2008, x2007)) %>%
    mutate(x2006 = ifelse(is.na(x2006), x2007, x2006)) %>%
    mutate(x2005 = ifelse(is.na(x2005), x2006, x2005)) %>%
    mutate(x2004 = ifelse(is.na(x2004), x2005, x2004)) %>%
    mutate(x2003 = ifelse(is.na(x2003), x2004, x2003)) %>%
    gather_("year", varname, paste0("x", 2003:2009)) %>%
    mutate(year = as.numeric(substr(year, 2, 5)))

}

#### testing area ################################
# setwd(dirname(sys.frame(1)$ofile))
# setwd('../reference_data')
# path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
# path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
# setwd("../R_export")
# path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
# 
# # put all paths in a list to pass them around in functions
# path_list <- list(path_abbr_st = path_abbr_st,
#                   path_latlon_cty = path_latlon_cty,
#                   path_response_cty = path_response_cty)
# setwd(dirname(sys.frame(1)$ofile))

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
# brfssMedCost_cty_df <- cleanX_brfssMedCost_cty()
# popDens_cty_df <- cleanX_popDensity_cty()
# housDens_cty_df <- cleanX_housDensity_cty()
# acsCommutInflows_cty_prep <- cleanX_acsCommutInflows_cty()
# btsPass_cty_df <- cleanX_btsPassInflows_cty()
# narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
# narrSfcTemp_cty_df <- cleanX_noaanarrSfcTemp_cty()
# wonderPollution_cty_df <- cleanX_wonderAirParticulateMatter_cty()
# cbpSocialAssoc_cty_df <- cleanX_cbpSocialAssoc_cty()
# acsOneParentHH_cty_df <- cleanX_acsOneParentFamHH_cty()
# acsOnePersonHH_cty_df <- cleanX_acsOnePersonHH_cty()
# acsAvgHHSize_cty_df <- cleanX_acsAvgHHSize_cty()
# brfssPoorHealth_cty_df <- cleanX_brfssPoorHealth_cty()
# brfssUnhealthyDays_cty_df <- cleanX_brfssUnhealthyDays_cty()
 
# # all region tables
# cdcFluPos_df <- cleanX_cdcFluview_fluPos_region()
# cdcH3_df <- cleanX_cdcFluview_H3_region()
# cdcH3A_df <- cleanX_cdcFluview_H3A_region() %>% select(-region)
# cdcB_df <- cleanX_cdcFluview_B_region() %>% select(-region)
# protectedPriorSeas_df <- cleanX_protectedFromPrevSeason_cty(path_list)
# subtypeDistrStrainSim_df <- cleanX_multsrcSubtypeDistrStrainSim_reg()

# # full data state tables 
# infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
# elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st()

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


