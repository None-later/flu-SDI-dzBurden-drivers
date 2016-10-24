
## Name: Elizabeth Lee
## Date: 1/22/16
## Function: functions to create model_version data and adjacency matrix of spatial neighbors for INLA -- state scale
## Filenames: reference_data/USstate_shapefiles/gz_2010_us_040_00_500k
## Data Source: shapefile from US Census 2010 - https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
## Notes: 
## 8/30/16: move testing module to source_prepare_testing_inlaData_cty.R
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(dplyr); require(tidyr); require(maptools); require(spdep)

#### functions for model data aggregation  ################################

remove_case_exceptions <- function(full_df){
  # 1) https://www.cdc.gov/nchs/data/nvss/bridged_race/county_geography_changes.pdf: Broomfield County, Colorado (FIPS code=08014) was created effective November 15, 2001 from parts of four Colorado counties: Adams, Boulder, Jefferson, and Weld. There are estimates for this county on some, but not all, of the bridged-race files. Note that data for Broomfield County do not appear on NCHS birth or mortality files until data year 2003
  # 2) fips 48301 (Loving Cty, TX) is an outlier in all seasons
  print(match.call())
  
  full_df2 <- full_df %>%
    filter(!(fips == "08014" & season %in% 2:5)) %>% # 1
    filter(!(fips == "48301")) # 2
  
  return(full_df2)
}

################################
testing_module <- function(filepathList){
  # iliSum response, all sampling effort, and driver variables
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  #### import data ####
  mod_cty_df <- cleanR_iliSum_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  cdcH3_df <- cleanX_cdcFluview_H3_region()
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, saipePov_cty_df, by = c("year", "fips")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcH3_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region)
  
  full_df <- dummy_df2 %>%
    group_by(season) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerPop)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    ungroup %>%
    filter(fips_st %in% continentalOnly) %>% # include data for continental states only
    select(-stateID, -adjProviderCoverage, -visitsPerProvider, -visitsPerPop, -poverty, -H3) %>%
    filter(season %in% 2:9) %>%
    mutate(logE = log(E))
  
  return(full_df)
}
################################

model5a_iliSum_v1 <- function(filepathList){
  # iliSum response, all sampling effort, and driver variables
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_iliSum_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  acsCommutInflows_cty_df <- cleanX_acsCommutInflows_cty()
  btsPass_cty_df <- cleanX_btsPassInflows_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st() 
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcH3_df <- cleanX_cdcFluview_H3_region()
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(acsCommutInflows_cty_df, by = c("year", "fips" = "fips_wrk")) %>%
    full_join(btsPass_cty_df, by = c("season", "fips" = "fips_dest")) %>%
    mutate(pass = ifelse(is.na(pass), 0, pass)) %>% # counties with NA from merge had 0 passengers entering
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcH3_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    group_by(season) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerPop)) %>% # 9/15/16 changed from visitsPerProvider
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>% 
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_commute = centerStandardize(commutInflows_prep)) %>% # commutInflows_prep/pop and commutInflows_prep raw look similar in EDA choropleths
    mutate(X_flight = centerStandardize(pass)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_H3 = centerStandardize(H3)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    ungroup %>%
    filter(fips_st %in% continentalOnly) %>% # include data for continental states only
    select(-stateID, -adjProviderCoverage, -visitsPerProvider, -visitsPerPop, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -commutInflows_prep, -pass, -infantAnyVax, -elderlyAnyVax, -H3, -humidity) %>%
    filter(season %in% 2:9) %>%
    mutate(logE = log(E)) # 7/18/16: for gamma likelihood offset
  
  return(full_df)
}
################################

model5b_iliPeak_v1 <- function(filepathList){
  # iliPeak response, all sampling effort, and driver variables
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_iliPeak_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  acsCommutInflows_cty_df <- cleanX_acsCommutInflows_cty()
  btsPass_cty_df <- cleanX_btsPassInflows_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st() 
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcH3_df <- cleanX_cdcFluview_H3_region()
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(acsCommutInflows_cty_df, by = c("year", "fips" = "fips_wrk")) %>%
    full_join(btsPass_cty_df, by = c("season", "fips" = "fips_dest")) %>%
    mutate(pass = ifelse(is.na(pass), 0, pass)) %>% # counties with NA from merge had 0 passengers entering
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcH3_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    group_by(season) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerProvider)) %>%
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>% 
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_commute = centerStandardize(commutInflows_prep)) %>% # commutInflows_prep/pop and commutInflows_prep raw look similar in EDA choropleths
    mutate(X_flight = centerStandardize(pass)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_H3 = centerStandardize(H3)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    ungroup %>%
    filter(fips_st %in% continentalOnly) %>% # include data for continental states only
    select(-stateID, -adjProviderCoverage, -visitsPerProvider, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -commutInflows_prep, -pass, -infantAnyVax, -elderlyAnyVax, -H3, -humidity) %>%
    filter(season %in% 2:9) %>%
    mutate(logE = log(E)) # 7/18/16: for gamma likelihood offset
  
  return(full_df)
}
################################

model6a_iliSum_v1 <- function(filepathList){
  # iliSum response, all sampling effort, and driver variables
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_iliSum_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  acsCommutInflows_cty_df <- cleanX_acsCommutInflows_cty()
  btsPass_cty_df <- cleanX_btsPassInflows_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st() 
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcH3_df <- cleanX_cdcFluview_H3_region()
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(acsCommutInflows_cty_df, by = c("year", "fips" = "fips_wrk")) %>%
    full_join(btsPass_cty_df, by = c("season", "fips" = "fips_dest")) %>%
    mutate(pass = ifelse(is.na(pass), 0, pass)) %>% # counties with NA from merge had 0 passengers entering
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcH3_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    group_by(season) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerPop)) %>% # 8/10/16 changed from visitsPerProvider
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>% 
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_commute = centerStandardize(commutInflows_prep)) %>% # commutInflows_prep/pop and commutInflows_prep raw look similar in EDA choropleths
    mutate(X_flight = centerStandardize(pass)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_H3 = centerStandardize(H3)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    ungroup %>%
    filter(fips_st %in% continentalOnly) %>% # include data for continental states only
    select(-stateID, -adjProviderCoverage, -visitsPerProvider, -visitsPerPop, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -commutInflows_prep, -pass, -infantAnyVax, -elderlyAnyVax, -H3, -humidity) %>%
    filter(season %in% 2:9) %>%
    mutate(logE = log(E)) # 7/18/16: for gamma likelihood offset
  
  return(full_df)
}
################################

model6a_iliSum_v2 <- function(filepathList){
  # iliSum response, all sampling effort, and driver variables
  # 10/3/16: v2 includes housDensity (pop per housing unit)
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_iliSum_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  housDens_cty_df <- cleanX_housDensity_cty()
  acsCommutInflows_cty_df <- cleanX_acsCommutInflows_cty()
  btsPass_cty_df <- cleanX_btsPassInflows_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st() 
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcH3_df <- cleanX_cdcFluview_H3_region()
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(housDens_cty_df, by = c("year", "fips")) %>%
    full_join(acsCommutInflows_cty_df, by = c("year", "fips" = "fips_wrk")) %>%
    full_join(btsPass_cty_df, by = c("season", "fips" = "fips_dest")) %>%
    mutate(pass = ifelse(is.na(pass), 0, pass)) %>% # counties with NA from merge had 0 passengers entering
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcH3_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    group_by(season) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerPop)) %>% # 8/10/16 changed from visitsPerProvider
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>% 
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_housdensity = centerStandardize(housDensity)) %>%
    mutate(X_commute = centerStandardize(commutInflows_prep)) %>% # commutInflows_prep/pop and commutInflows_prep raw look similar in EDA choropleths
    mutate(X_flight = centerStandardize(pass)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_H3 = centerStandardize(H3)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    ungroup %>%
    filter(fips_st %in% continentalOnly) %>% # include data for continental states only
    select(-stateID, -adjProviderCoverage, -visitsPerProvider, -visitsPerPop, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, - housDensity, -commutInflows_prep, -pass, -infantAnyVax, -elderlyAnyVax, -H3, -humidity) %>%
    filter(season %in% 2:9) %>%
    mutate(logE = log(E)) # 7/18/16: for gamma likelihood offset
  
  return(full_df)
}
################################

model6a_iliSum_v3 <- function(filepathList){
  # iliSum response, all sampling effort, and driver variables
  # 10/20/16: v3 cleans inverse of population density
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_iliSum_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  housDens_cty_df <- cleanX_housDensity_cty()
  acsCommutInflows_cty_df <- cleanX_acsCommutInflows_cty()
  btsPass_cty_df <- cleanX_btsPassInflows_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st() 
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcH3_df <- cleanX_cdcFluview_H3_region()
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(housDens_cty_df, by = c("year", "fips")) %>%
    full_join(acsCommutInflows_cty_df, by = c("year", "fips" = "fips_wrk")) %>%
    full_join(btsPass_cty_df, by = c("season", "fips" = "fips_dest")) %>%
    mutate(pass = ifelse(is.na(pass), 0, pass)) %>% # counties with NA from merge had 0 passengers entering
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcH3_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    group_by(season) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerPop)) %>% # 8/10/16 changed from visitsPerProvider
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>% 
    mutate(X_popdensity2Inv = centerStandardize(1/(popDensity^2))) %>%
    mutate(X_housdensity = centerStandardize(housDensity)) %>%
    mutate(X_commute = centerStandardize(commutInflows_prep)) %>% # commutInflows_prep/pop and commutInflows_prep raw look similar in EDA choropleths
    mutate(X_flight = centerStandardize(pass)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_H3 = centerStandardize(H3)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    ungroup %>%
    filter(fips_st %in% continentalOnly) %>% # include data for continental states only
    select(-stateID, -adjProviderCoverage, -visitsPerProvider, -visitsPerPop, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, - housDensity, -commutInflows_prep, -pass, -infantAnyVax, -elderlyAnyVax, -H3, -humidity) %>%
    filter(season %in% 2:9) %>%
    mutate(logE = log(E)) # 7/18/16: for gamma likelihood offset
  
  return(full_df)
}
################################

model6c_iliExcessBL_v1 <- function(filepathList){
  # iliOverBL response, all sampling effort, and driver variables
  # 10/24/16: v3 cleans population density
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_iliExcessBL_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  housDens_cty_df <- cleanX_housDensity_cty()
  acsCommutInflows_cty_df <- cleanX_acsCommutInflows_cty()
  btsPass_cty_df <- cleanX_btsPassInflows_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st() 
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcH3_df <- cleanX_cdcFluview_H3_region()
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(housDens_cty_df, by = c("year", "fips")) %>%
    full_join(acsCommutInflows_cty_df, by = c("year", "fips" = "fips_wrk")) %>%
    full_join(btsPass_cty_df, by = c("season", "fips" = "fips_dest")) %>%
    mutate(pass = ifelse(is.na(pass), 0, pass)) %>% # counties with NA from merge had 0 passengers entering
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcH3_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    group_by(season) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerPop)) %>% # 8/10/16 changed from visitsPerProvider
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>% 
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_housdensity = centerStandardize(housDensity)) %>%
    mutate(X_commute = centerStandardize(commutInflows_prep)) %>% # commutInflows_prep/pop and commutInflows_prep raw look similar in EDA choropleths
    mutate(X_flight = centerStandardize(pass)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_H3 = centerStandardize(H3)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    ungroup %>%
    filter(fips_st %in% continentalOnly) %>% # include data for continental states only
    select(-stateID, -adjProviderCoverage, -visitsPerProvider, -visitsPerPop, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, - housDensity, -commutInflows_prep, -pass, -infantAnyVax, -elderlyAnyVax, -H3, -humidity) %>%
    filter(season %in% 2:9) %>%
    mutate(logE = log(E)) # 7/18/16: for gamma likelihood offset
  
  return(full_df)
}
################################

model6d_iliExcessThresh_v1 <- function(filepathList){
  # iliExcessThresh response, all sampling effort, and driver variables
  # 10/24/16: v3 cleans population density
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_iliExcessThresh_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  housDens_cty_df <- cleanX_housDensity_cty()
  acsCommutInflows_cty_df <- cleanX_acsCommutInflows_cty()
  btsPass_cty_df <- cleanX_btsPassInflows_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st() 
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcH3_df <- cleanX_cdcFluview_H3_region()
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(housDens_cty_df, by = c("year", "fips")) %>%
    full_join(acsCommutInflows_cty_df, by = c("year", "fips" = "fips_wrk")) %>%
    full_join(btsPass_cty_df, by = c("season", "fips" = "fips_dest")) %>%
    mutate(pass = ifelse(is.na(pass), 0, pass)) %>% # counties with NA from merge had 0 passengers entering
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcH3_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    group_by(season) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerPop)) %>% # 8/10/16 changed from visitsPerProvider
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>% 
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_housdensity = centerStandardize(housDensity)) %>%
    mutate(X_commute = centerStandardize(commutInflows_prep)) %>% # commutInflows_prep/pop and commutInflows_prep raw look similar in EDA choropleths
    mutate(X_flight = centerStandardize(pass)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_H3 = centerStandardize(H3)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    ungroup %>%
    filter(fips_st %in% continentalOnly) %>% # include data for continental states only
    select(-stateID, -adjProviderCoverage, -visitsPerProvider, -visitsPerPop, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, - housDensity, -commutInflows_prep, -pass, -infantAnyVax, -elderlyAnyVax, -H3, -humidity) %>%
    filter(season %in% 2:9) %>%
    mutate(logE = log(E)) # 7/18/16: for gamma likelihood offset
  
  return(full_df)
}

################################

model6a_iliSum_v2_raw <- function(filepathList){
  # iliSum response, all sampling effort, and driver variables - before centerStandardize
  # 10/11/16: v2 includes housDensity (pop per housing unit)
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_iliSum_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  housDens_cty_df <- cleanX_housDensity_cty()
  acsCommutInflows_cty_df <- cleanX_acsCommutInflows_cty()
  btsPass_cty_df <- cleanX_btsPassInflows_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st() 
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcH3_df <- cleanX_cdcFluview_H3_region()
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(housDens_cty_df, by = c("year", "fips")) %>%
    full_join(acsCommutInflows_cty_df, by = c("year", "fips" = "fips_wrk")) %>%
    full_join(btsPass_cty_df, by = c("season", "fips" = "fips_dest")) %>%
    mutate(pass = ifelse(is.na(pass), 0, pass)) %>% # counties with NA from merge had 0 passengers entering
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcH3_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    group_by(season) %>%
    mutate(rO_imscoverage = adjProviderCoverage) %>%
    mutate(rO_careseek = visitsPerPop) %>% # 8/10/16 changed from visitsPerProvider
    mutate(rO_insured = insured) %>%
    mutate(rX_poverty = poverty) %>%
    mutate(rX_child = child) %>%
    mutate(rX_adult = adult) %>%
    mutate(rX_hospaccess = hospitalAccess) %>% 
    mutate(rX_popdensity = popDensity) %>%
    mutate(rX_housdensity = housDensity) %>%
    mutate(rX_commute = commutInflows_prep) %>% 
    mutate(rX_flight = pass) %>%
    mutate(rX_vaxcovI = infantAnyVax) %>%
    mutate(rX_vaxcovE = elderlyAnyVax) %>%
    mutate(rX_H3 = H3) %>%
    mutate(rX_humidity = humidity) %>%
    ungroup %>%
    filter(fips_st %in% continentalOnly) %>% # include data for continental states only
    select(-stateID, -adjProviderCoverage, -visitsPerProvider, -visitsPerPop, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, - housDensity, -commutInflows_prep, -pass, -infantAnyVax, -elderlyAnyVax, -H3, -humidity) %>%
    filter(season %in% 2:9) %>%
    mutate(logE = log(E)) # 7/18/16: for gamma likelihood offset
  
  return(full_df)
}
################################

model6b_iliPeak_v1 <- function(filepathList){
  # iliPeak response, all sampling effort, and driver variables
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_iliPeak_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  acsCommutInflows_cty_df <- cleanX_acsCommutInflows_cty()
  btsPass_cty_df <- cleanX_btsPassInflows_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st() 
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcH3_df <- cleanX_cdcFluview_H3_region()
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(acsCommutInflows_cty_df, by = c("year", "fips" = "fips_wrk")) %>%
    full_join(btsPass_cty_df, by = c("season", "fips" = "fips_dest")) %>%
    mutate(pass = ifelse(is.na(pass), 0, pass)) %>% # counties with NA from merge had 0 passengers entering
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcH3_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    group_by(season) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerPop)) %>% # 8/10/16 changed from visitsPerProvider
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>% 
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_commute = centerStandardize(commutInflows_prep)) %>% # commutInflows_prep/pop and commutInflows_prep raw look similar in EDA choropleths
    mutate(X_flight = centerStandardize(pass)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_H3 = centerStandardize(H3)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    ungroup %>%
    filter(fips_st %in% continentalOnly) %>% # include data for continental states only
    select(-stateID, -adjProviderCoverage, -visitsPerProvider, -visitsPerPop, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -commutInflows_prep, -pass, -infantAnyVax, -elderlyAnyVax, -H3, -humidity) %>%
    filter(season %in% 2:9) %>%
    mutate(logE = log(E)) # 7/18/16: for gamma likelihood offset
  
  return(full_df)
}
################################

#### functions for shapefile manipulation ################################
read_shapefile_cty <- function(filepathList){
  # read & clean county shapefile
  print(match.call())
  print(filepathList)
  
  # read fips codes for continental US states only
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  # read shapefile into R
  cty.poly.full <- readShapePoly(filepathList$path_shape_cty) 
  # restrict shapefile to only US states
  cty.poly.states <- cty.poly.full[cty.poly.full@data$STATE %in% continentalOnly,]
  
  ## LOG ##
  # 6/20/16: remove GEO_IDs for Nantucket County MA (25019) & San Juan County (53055), which have no neighbors, from shapefile --> c("0500000US25019", "0500000US53055") 
  # 7/6/16: removals from shapefile need to by synced with .graph output (reference_censusCtyShapefile_oneComponent.R)
  # 7/8/16: replaced '.graph' output with ASCII file with no islands (US_county_adjacency_fips.dat) derived from true Census neighbors list where IDs are county fips codes (exported from Census/programs/reference_censusCtyAdjacency_inlaFormat.py). No longer need to remove specific GEO_IDs or keep shapefile in sync with .graph output as discussed above.
  
  return(cty.poly.states)
}
################################

combine_shapefile_modelData_cty <- function(filepathList, modelData, seasNum){
  # merge model data with poly data for a single season
  print(match.call())

  # grab state shapefile data
  cty.poly.full <- read_shapefile_cty(filepathList)
  cty.poly.only <- attr(cty.poly.full, "data") # grab only polygon data

  # merge with model data
  modelData2 <- modelData %>%
    filter(season == seasNum) 
  modelData3 <- tbl_df(cty.poly.only) %>%
    mutate(fips = paste0(as.character(STATE), as.character(COUNTY))) %>%
    select(fips, GEO_ID) %>%
    left_join(modelData2, by = "fips") %>%
    mutate(ID = seq_along(fips)) # 7/6/16: for the spatial models, the ID variable uniquely identifies the neighbors in the .graph file
  
  return(modelData3)
}
################################

convert_hurdleModel_separatePredictors_spatiotemporal <- function(modData_seas){
  # 8/17/16: prepare all seasons model data for 2 stage (hurdle) model in INLA 
  print(match.call())
  
  # top half response matrix with epi/no-epi indicator (binomial lik) and NA (gamma lik)
  Y_bin <- modData_seas %>% 
    select(y) %>%
    mutate(y0 = ifelse(y == 0, 0, ifelse(y > 0, 1, NA))) %>% # 0 = no epidemic, 1 = epidemic, NA = NA
    mutate(y1 = NA) %>%
    select(-y) 
  
  # bottom half response matrix with NA (binomial lik) and non-zeros/NA (gamma lik)
  Y_gam <- modData_seas %>% 
    select(y) %>%
    mutate(y0 = NA) %>% # 0 = no epidemic, 1 = epidemic, NA = NA
    mutate(y1 = ifelse(y > 0, y, NA)) %>%
    select(-y) 
  
  Y <- bind_rows(Y_bin, Y_gam) %>% data.matrix
  
  # covariate matrix for binomial lik: response, predictors, random effects
  Mx_bin <- modData_seas %>%
    select(contains("X_"), contains("O_"), fips, fips_st, regionID, season) %>%
    mutate(intercept = 1) 
  colnames(Mx_bin) <- paste0(colnames(Mx_bin), "_bin")
  
  # covariate matrix for gamma lik: response, predictors, random effects & offset
  Mx_gam <- modData_seas %>%
    select(contains("X_"), contains("O_"), fips, fips_st, regionID, logE, season) %>%
    mutate(intercept = 1) 
  colnames(Mx_gam) <- paste0(colnames(Mx_gam), "_nonzero")
  
  # NA block for bin & gam Mx
  NA_bin <- data.frame(matrix(data = NA, nrow = nrow(Mx_bin), ncol = ncol(Mx_bin)))
  names(NA_bin) <- colnames(Mx_bin)
  NA_gam <- data.frame(matrix(data = NA, nrow = nrow(Mx_gam), ncol = ncol(Mx_gam)))
  names(NA_gam) <- colnames(Mx_gam)
  # add NAs to appropriate locations
  Mx_bin2 <- bind_rows(Mx_bin , NA_bin)
  Mx_gam2 <- bind_rows(NA_gam, Mx_gam)
  
  # convert matrix information to a list of lists/matrixes
  Mx <- bind_cols(Mx_bin2, Mx_gam2)
  modData_seas_lists <- list()
  for (column in colnames(Mx)){
    modData_seas_lists[[column]] <- Mx[[column]]
  }
  # add Y response matrix as a list
  modData_seas_lists[['Y']] <- Y
  
  return(modData_seas_lists)
}

################################

convert_hurdleModel_separatePredictors <- function(modData_seas){
  # prepare data seasonal model data for 2 stage (hurdle) model in INLA 
  # 7/20/16 duplicate locations
  print(match.call())
  
  # top half response matrix with epi/no-epi indicator (binomial lik) and NA (gamma lik)
  Y_bin <- modData_seas %>% 
    select(y) %>%
    mutate(y0 = ifelse(y == 0, 0, ifelse(y > 0, 1, NA))) %>% # 0 = no epidemic, 1 = epidemic, NA = NA
    mutate(y1 = NA) %>%
    select(-y) 
  
  # bottom half response matrix with NA (binomial lik) and non-zeros/NA (gamma lik)
  Y_gam <- modData_seas %>% 
    select(y) %>%
    mutate(y0 = NA) %>% # 0 = no epidemic, 1 = epidemic, NA = NA
    mutate(y1 = ifelse(y > 0, y, NA)) %>%
    select(-y) 
  
  Y <- bind_rows(Y_bin, Y_gam) %>% data.matrix
  View(Y)
  
  # covariate matrix for binomial lik: response, predictors, random effects
  Mx_bin <- modData_seas %>%
    select(contains("X_"), contains("O_"), fips, fips_st, regionID) %>%
    mutate(intercept = 1) 
  colnames(Mx_bin) <- paste0(colnames(Mx_bin), "_bin")
  
  # covariate matrix for gamma lik: response, predictors, random effects & offset
  Mx_gam <- modData_seas %>%
    select(contains("X_"), contains("O_"), fips, fips_st, regionID, logE) %>%
    mutate(intercept = 1) 
  colnames(Mx_gam) <- paste0(colnames(Mx_gam), "_nonzero")
  
  # NA block for bin & gam Mx
  NA_bin <- data.frame(matrix(data = NA, nrow = nrow(Mx_bin), ncol = ncol(Mx_bin)))
  names(NA_bin) <- colnames(Mx_bin)
  NA_gam <- data.frame(matrix(data = NA, nrow = nrow(Mx_gam), ncol = ncol(Mx_gam)))
  names(NA_gam) <- colnames(Mx_gam)
  # add NAs to appropriate locations
  Mx_bin2 <- bind_rows(Mx_bin , NA_bin)
  Mx_gam2 <- bind_rows(NA_gam, Mx_gam)
  
  # convert matrix information to a list of lists/matrixes
  Mx <- bind_cols(Mx_bin2, Mx_gam2)
  modData_seas_lists <- list()
  for (column in colnames(Mx)){
    modData_seas_lists[[column]] <- Mx[[column]]
  }
  # add Y response matrix as a list
  modData_seas_lists[['Y']] <- Y
  
  return(modData_seas_lists)
}
################################

convert_hurdleModel_gamma_spatiotemporal <- function(modData_seas){
  # 10/11/16: prepare data seasonal model data for gamma model component
  print(match.call())
  
  # bottom half response matrix with NA (binomial lik) and non-zeros/NA (gamma lik)
  Y_gam <- modData_seas %>% 
    select(y) %>%
    mutate(y1 = ifelse(y > 0, y, NA)) %>%
    select(y1) %>%
    unlist
  
  # covariate matrix for gamma lik: response, predictors, random effects & offset
  Mx_gam <- modData_seas %>%
    select(contains("X_"), contains("O_"), fips, fips_st, regionID, logE, season) %>%
    mutate(intercept = 1) 
  colnames(Mx_gam) <- paste0(colnames(Mx_gam), "_nonzero")
  
  # convert matrix information to a list of lists/matrixes
  modData_seas_lists <- list()
  for (column in colnames(Mx_gam)){
    modData_seas_lists[[column]] <- Mx_gam[[column]]
  }
  # add Y response vector as a list
  modData_seas_lists[['Y']] <- Y_gam
  
  return(modData_seas_lists)
}

################################

convert_hurdleModel_binomial <- function(modData_seas){
  # 7/22/16: prepare data seasonal model data with 2 stage (hurdle) model structure, but only for binomial model component
  print(match.call())
  
  # top half response matrix with epi/no-epi indicator (binomial lik) and NA (gamma lik)
  Y_bin <- modData_seas %>% 
    select(y) %>%
    mutate(y0 = ifelse(y == 0, 0, ifelse(y > 0, 1, NA))) %>% # 0 = no epidemic, 1 = epidemic, NA = NA
    select(y0) %>% 
    unlist
  
  # covariate matrix for binomial lik: response, predictors, random effects
  Mx_bin <- modData_seas %>%
    select(contains("X_"), contains("O_"), fips, fips_st, regionID) %>%
    mutate(intercept = 1) 
  colnames(Mx_bin) <- paste0(colnames(Mx_bin), "_bin")

  # convert matrix information to a list of lists/matrixes
  modData_seas_lists <- list()
  for (column in colnames(Mx_bin)){
    modData_seas_lists[[column]] <- Mx_bin[[column]]
  }
  # add Y response vector as a list
  modData_seas_lists[['Y']] <- Y_bin
  
  return(modData_seas_lists)
}
################################

convert_hurdleModel_gamma <- function(modData_seas){
  # 7/22/16: prepare data seasonal model data for 2 stage (hurdle) model structure, but only for gamma model component
  print(match.call())
  
  # bottom half response matrix with NA (binomial lik) and non-zeros/NA (gamma lik)
  Y_gam <- modData_seas %>% 
    select(y) %>%
    mutate(y1 = ifelse(y > 0, y, NA)) %>%
    select(y1) %>%
    unlist
  
  # covariate matrix for gamma lik: response, predictors, random effects & offset
  Mx_gam <- modData_seas %>%
    select(contains("X_"), contains("O_"), fips, fips_st, regionID, logE) %>%
    mutate(intercept = 1) 
  colnames(Mx_gam) <- paste0(colnames(Mx_gam), "_nonzero")
  
  # convert matrix information to a list of lists/matrixes
  modData_seas_lists <- list()
  for (column in colnames(Mx_gam)){
    modData_seas_lists[[column]] <- Mx_gam[[column]]
  }
  # add Y response vector as a list
  modData_seas_lists[['Y']] <- Y_gam
  
  return(modData_seas_lists)
}
################################

convert_2stageModelData_sharedPredictors <- function(modData_seas){
  # (7/18/16: No longer in use) prepare data seasonal model data for 2 stage (hurdle) model in INLA 
  print(match.call())
  
  # create response matrix with 0s and non-zeros
  Y <- modData_seas %>% 
    select(y) %>%
    mutate(y0 = ifelse(y == 0, y, NA)) %>%
    mutate(y1 = ifelse(y > 0, y, NA)) %>%
    select(-y) %>%
    data.matrix
  
  # create matrix for response, predictors, random effects, offset
  Mx <- modData_seas %>%
    select(fips, fips_st, regionID, logE, contains("X_"), contains("O_")) %>%
    mutate(intercept = 1) 
  
  # duplicate rows if covariates are shared across the two likelihoods
  Mx_dup <- bind_rows(Mx, Mx)
  
  # convert matrix information to a list of lists/matrixes
  modData_seas_lists <- list()
  for (column in colnames(Mx)){
    modData_seas_lists[[column]] <- Mx[[column]]
  }
  # add Y response matrix as a list
  modData_seas_lists[['Y']] <- Y
  
  return(modData_seas_lists)
}

#### test the functions here  ################################



