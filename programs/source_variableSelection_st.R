
## Name: Elizabeth Lee
## Date: 3/30/16
## Function: functions to create model_version data for EDA with linear models
## Filenames: reference_data/USstate_shapefiles/gz_2010_us_040_00_500k
## Data Source: shapefile from US Census 2010 - https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(dplyr); require(tidyr); 

#### functions for model data aggregation  ################################

stepLm_iliSum_v1 <- function(filepathList){
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
    mutate(X_elderly = centerStandardize(mcareElig)) %>%
    mutate(X_hcaccess = centerStandardize(hospitalAccess)) %>% 
    mutate(X_physaccess = centerStandardize(physicianAccess)) %>%
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_housdensity = centerStandardize(housDensity)) %>%
    mutate(X_commute = centerStandardize(commutInflows_prep/pop)) %>%
    mutate(X_flight = centerStandardize(pass_prep/pop)) %>%
    select(-adjProviderCoverage, -visitsPerProvider, -insured, -poverty, -income, -mcaidElig, -mcareElig, -hospitalAccess, -physicianAccess, -popDensity, -housDensity, -commutInflows_prep, -pass_prep)
  
  return(full_df)
}


#### heading 2 ################################


#### test the functions here  ################################


