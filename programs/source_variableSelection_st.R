
## Name: Elizabeth Lee
## Date: 3/30/16
## Function: functions to perform EDA for variable selection, create model_version data for EDA with linear models
## Filenames: reference_data/USstate_shapefiles/gz_2010_us_040_00_500k
## Data Source: shapefile from US Census 2010 - https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
## Notes: renamed from source_prepare_lmData_st.R on 4/11/16
## 6/2/16: moved "functions for pairwise comparison" and "functions for single variable modeling" and "functions for single variable coef plotting" to source_variableSelection.R script
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(dplyr); require(tidyr); require(GGally) 
require(INLA)

#### functions for data aggregation  ################################

prepare_allCov_iliSum_st <- function(filepathList){
  # iliSum response, all sampling effort, and driver variables
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  #### import data ####
  mod_df <- cleanR_iliSum_st(filepathList)
  imsCov_df <- cleanO_imsCoverage_st(filepathList)
  cpsasecInsured_df <- cleanO_cpsasecInsured_st()
  saipePoverty_df <- cleanX_saipePoverty_st()
  saipeIncome_df <- cleanX_saipeIncome_st() 
  ahrfMcaidElig_df <- cleanX_ahrfMedicaidEligibles_st() 
  ahrfMcareElig_df <- cleanX_ahrfMedicareEligibles_st() 
  censusInfantPop_st <- cleanX_censusInfantPop_st()
  censusToddlerPop_st <- cleanX_censusToddlerPop_st()
  censusChildPop_st <- cleanX_censusChildPop_st()
  censusAdultPop_st <- cleanX_censusAdultPop_st()
  censusElderlyPop_st <- cleanX_censusElderlyPop_st()
  ahrfHospAccess_df <- cleanX_ahrfHospitals_st() 
  ahrfPhysAccess_df <- cleanX_ahrfPhysicians_st() 
  censusPopDens_df <- cleanX_popDensity_st()
  censusHousDens_df <- cleanX_housDensity_st()
  acsCommut_prep <- cleanX_acsCommutInflows_st()
  btsPass_prep <- cleanX_btsPassInflows_st()
  cdcFluPos_df <- cleanX_cdcFluview_fluPos_region()
  cdcH3_df <- cleanX_cdcFluview_H3_region() %>% select(-region)
  narrSpecHum_df <- cleanX_noaanarrSpecHum_st()
  narrSfcTemp_df <- cleanX_noaanarrSfcTemp_st()
  
  #### join data ####
  dummy_df <- full_join(mod_df, imsCov_df, by = c("year", "abbr"))
  dummy_df2 <- full_join(dummy_df, cpsasecInsured_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePoverty_df, by = c("year", "fips")) %>%
    full_join(saipeIncome_df, by = c("year", "fips")) %>%
    full_join(ahrfMcaidElig_df, by = c("year", "fips")) %>%
    full_join(ahrfMcareElig_df, by = c("year", "fips")) %>%
    full_join(censusInfantPop_st, by = c("year", "fips")) %>%
    full_join(censusToddlerPop_st, by = c("year", "fips")) %>%
    full_join(censusChildPop_st, by = c("year", "fips")) %>%
    full_join(censusAdultPop_st, by = c("year", "fips")) %>%
    full_join(censusElderlyPop_st, by = c("year", "fips")) %>%
    full_join(ahrfHospAccess_df, by = c("year", "fips")) %>%
    full_join(ahrfPhysAccess_df, by = c("year", "fips")) %>%
    full_join(censusPopDens_df, by = c("year", "fips")) %>%
    full_join(censusHousDens_df, by = c("year", "fips")) %>%
    full_join(acsCommut_prep, by = c("year", "fips" = "fips_wrk")) %>%
    full_join(btsPass_prep, by = c("season", "fips" = "fips_dest")) %>%
    full_join(cdcFluPos_df, by = c("season", "fips")) %>%
    full_join(cdcH3_df, by = c("season", "fips")) %>%
    full_join(narrSpecHum_df, by = c("season", "fips")) %>%
    full_join(narrSfcTemp_df, by = c("season", "fips")) %>%
    group_by(season) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerProvider)) %>%
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_income = centerStandardize(income)) %>%
    mutate(X_mcaid = centerStandardize(mcaidElig)) %>%
    mutate(X_mcare = centerStandardize(mcareElig)) %>%
    mutate(X_infant = centerStandardize(infant)) %>%
    mutate(X_toddler = centerStandardize(toddler)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_elderly = centerStandardize(elderly)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>% 
    mutate(X_physaccess = centerStandardize(physicianAccess)) %>%
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_housdensity = centerStandardize(housDensity)) %>%
    mutate(X_commute = centerStandardize(commutInflows_prep/pop)) %>%
    mutate(X_flight = centerStandardize(pass_prep/pop)) %>%
    mutate(X_fluPos = centerStandardize(fluPos)) %>%
    mutate(X_H3 = centerStandardize(H3)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    mutate(X_temperature = centerStandardize(temperature)) %>%
    ungroup %>%
    select(-adjProviderCoverage, -visitsPerProvider, -insured, -poverty, -income, -mcaidElig, -mcareElig, -infant, -toddler, -child, -adult, -elderly, -hospitalAccess, -physicianAccess, -popDensity, -housDensity, -commutInflows_prep, -pass_prep, -fluPos, -H3, -humidity, -temperature) %>%
    filter(season %in% 2:9)
  
  return(full_df)
}
################################

stepLm_iliSum_v1 <- function(filepathList){
  # pairs with EDA_stepLm_iliSum_v1.R
  # iliSum response, sampling effort, and driver variables: models 3f, 3h
  # v1 = proportion in poverty
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  #### import data ####
  mod_df <- cleanR_iliSum_st(filepathList)
  imsCov_df <- cleanO_imsCoverage_st(filepathList)
  cpsasecInsured_df <- cleanO_cpsasecInsured_st()
  saipePoverty_df <- cleanX_saipePoverty_st()
  saipeIncome_df <- cleanX_saipeIncome_st() 
  ahrfMcaidElig_df <- cleanX_ahrfMedicaidEligibles_st() 
  ahrfMcareElig_df <- cleanX_ahrfMedicareEligibles_st() 
  ahrfHospAccess_df <- cleanX_ahrfHospitals_st() 
  ahrfPhysAccess_df <- cleanX_ahrfPhysicians_st() 
  censusPopDens_df <- cleanX_popDensity_st()
  censusHousDens_df <- cleanX_housDensity_st()
  acsCommut_prep <- cleanX_acsCommutInflows_st()
  btsPass_prep <- cleanX_btsPassInflows_st()
  
  
  #### join data ####
  dummy_df <- left_join(mod_df, imsCov_df, by = c("year", "abbr"))
  dummy_df2 <- left_join(dummy_df, cpsasecInsured_df, by = c("year", "fips"))
  
  full_df <- left_join(dummy_df2, saipePoverty_df, by = c("year", "fips")) %>%
    left_join(saipeIncome_df, by = c("year", "fips")) %>%
    left_join(ahrfMcaidElig_df, by = c("year", "fips")) %>%
    left_join(ahrfMcareElig_df, by = c("year", "fips")) %>%
    left_join(ahrfHospAccess_df, by = c("year", "fips")) %>%
    left_join(ahrfPhysAccess_df, by = c("year", "fips")) %>%
    left_join(censusPopDens_df, by = c("year", "fips")) %>%
    left_join(censusHousDens_df, by = c("year", "fips")) %>%
    left_join(acsCommut_prep, by = c("year", "fips" = "fips_wrk")) %>%
    left_join(btsPass_prep, by = c("season", "fips" = "fips_dest")) %>%
    group_by(year) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerProvider)) %>%
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_income = centerStandardize(income)) %>%
    mutate(X_mcaid = centerStandardize(mcaidElig)) %>%
    mutate(X_elderly = centerStandardize(mcareElig)) %>% # renamed to X_mcare
    mutate(X_hcaccess = centerStandardize(hospitalAccess)) %>% # renamed to X_hospaccess
    mutate(X_physaccess = centerStandardize(physicianAccess)) %>%
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_housdensity = centerStandardize(housDensity)) %>%
    mutate(X_commute = centerStandardize(commutInflows_prep/pop)) %>%
    mutate(X_flight = centerStandardize(pass_prep/pop)) %>%
    select(-adjProviderCoverage, -visitsPerProvider, -insured, -poverty, -income, -mcaidElig, -mcareElig, -hospitalAccess, -physicianAccess, -popDensity, -housDensity, -commutInflows_prep, -pass_prep)
  
  return(full_df)
}
################################


#### test the functions here  ################################

# test <- model_singleVariable_inla_st(allDat2, "iliSum", 2, "X_poverty")
