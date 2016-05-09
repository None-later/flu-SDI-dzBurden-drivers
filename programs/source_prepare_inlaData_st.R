
## Name: Elizabeth Lee
## Date: 1/22/16
## Function: functions to create model_version data and adjacency matrix of spatial neighbors for INLA -- state scale
## Filenames: reference_data/USstate_shapefiles/gz_2010_us_040_00_500k
## Data Source: shapefile from US Census 2010 - https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(dplyr); require(tidyr); require(maptools); require(spdep)

#### functions for model data aggregation  ################################

model3a_iliSum_v1 <- function(filepathList){
  # iliSum response only: models 3a, 3c
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  mod_df <- cleanR_iliSum_st(filepathList)
  return(mod_df)
}
################################

model3b_iliSum_v1 <- function(filepathList){
  # iliSum response and sampling effort variables (IMS database cov, IMS viz/phys, % insured): models 3b, 3d
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  mod_df <- cleanR_iliSum_st(filepathList)
  imsCov_df <- cleanO_imsCoverage_st(filepathList)
  cpsasecInsured_df <- cleanO_cpsasecInsured_st()
  
  dummy_df <- left_join(mod_df, imsCov_df, by = c("year", "abbr"))
  full_df <- left_join(dummy_df, cpsasecInsured_df, by = c("year", "fips")) %>%
    group_by(year) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerProvider)) %>%
    mutate(O_insured = centerStandardize(insured)) %>%
    select(-adjProviderCoverage, -visitsPerProvider, -insured) %>%
    ungroup
                        
  return(full_df)
}
################################

model3e_iliSum_v1 <- function(filepathList){
  # iliSum response and driver variables: models 3e, 3g
  # v1 = proportion in poverty
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  mod_df <- cleanR_iliSum_st(filepathList)
  saipePoverty_df <- cleanX_saipePoverty_st()

  full_df <- left_join(mod_df, saipePoverty_df, by = c("year", "fips")) %>%
    group_by(year) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    ungroup
  
  return(full_df)
}
################################

model3f_iliSum_v1 <- function(filepathList){
  # iliSum response, sampling effort, and driver variables: models 3f, 3h
  # v1 = proportion in poverty
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  mod_df <- cleanR_iliSum_st(filepathList)
  imsCov_df <- cleanO_imsCoverage_st(filepathList)
  cpsasecInsured_df <- cleanO_cpsasecInsured_st()
  saipePoverty_df <- cleanX_saipePoverty_st()
  
  
  dummy_df <- left_join(mod_df, imsCov_df, by = c("year", "abbr"))
  dummy_df2 <- left_join(dummy_df, cpsasecInsured_df, by = c("year", "fips")) %>%
    group_by(year) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerProvider)) %>%
    mutate(O_insured = centerStandardize(insured)) %>%
    select(-adjProviderCoverage, -visitsPerProvider, -insured) %>%
    ungroup
  full_df <- left_join(dummy_df2, saipePoverty_df, by = c("year", "fips")) %>%
    group_by(year) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    ungroup 
  
  return(full_df)
}

################################

model4a_iliSum_v1 <- function(filepathList){
  # iliSum response, sampling effort, and driver variables: models 3f, 3h
  # v1 = no vaxcov, variable selection -- one var per proxy
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  mod_df <- cleanR_iliSum_st(filepathList)
  imsCov_df <- cleanO_imsCoverage_st(filepathList)
  cpsasecInsured_df <- cleanO_cpsasecInsured_st()
  saipeIncome_df <- cleanX_saipeIncome_st() 
  censusChildPop_st <- cleanX_censusChildPop_st()
  censusAdultPop_st <- cleanX_censusAdultPop_st()
  ahrfHospAccess_df <- cleanX_ahrfHospitals_st() 
  btsPass_prep <- cleanX_btsPassInflows_st()
  cdcFluPos_df <- cleanX_cdcFluview_fluPos_region()
  cdcH3_df <- cleanX_cdcFluview_H3_region() %>% select(-region)
  narrSpecHum_df <- cleanX_noaanarrSpecHum_st()

  #### join data ####
  dummy_df <- full_join(mod_df, imsCov_df, by = c("year", "abbr"))
  dummy_df2 <- full_join(dummy_df, cpsasecInsured_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipeIncome_df, by = c("year", "fips")) %>%
    full_join(censusChildPop_st, by = c("year", "fips")) %>%
    full_join(censusAdultPop_st, by = c("year", "fips")) %>%
    full_join(ahrfHospAccess_df, by = c("year", "fips")) %>%
    full_join(btsPass_prep, by = c("season", "fips" = "fips_dest")) %>%
    full_join(cdcFluPos_df, by = c("season", "fips")) %>%
    full_join(cdcH3_df, by = c("season", "fips")) %>%
    full_join(narrSpecHum_df, by = c("season", "fips")) %>%
    group_by(season) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerProvider)) %>%
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_income = centerStandardize(income)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>% 
    mutate(X_flight = centerStandardize(pass_prep/pop)) %>%
    mutate(X_fluPos = centerStandardize(fluPos)) %>%
    mutate(X_H3 = centerStandardize(H3)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    ungroup %>%
    select(-adjProviderCoverage, -visitsPerProvider, -insured, -income, -child, -adult, -hospitalAccess, -pass_prep, -fluPos, -H3, -humidity) %>%
    filter(season %in% 2:9)
  
  return(full_df)
}

################################

model4a_iliSum_v2 <- function(filepathList){
  # iliSum response, sampling effort, and driver variables: models 3f, 3h
  # v1 = no vaxcov, variable selection -- one var per proxy
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  mod_df <- cleanR_iliSum_st(filepathList)
  imsCov_df <- cleanO_imsCoverage_st(filepathList)
  cpsasecInsured_df <- cleanO_cpsasecInsured_st()
  saipeIncome_df <- cleanX_saipeIncome_st() 
  censusChildPop_st <- cleanX_censusChildPop_st()
  censusAdultPop_st <- cleanX_censusAdultPop_st()
  ahrfHospAccess_df <- cleanX_ahrfHospitals_st() 
  acsCommut_prep <- cleanX_acsCommutInflows_st()
  btsPass_prep <- cleanX_btsPassInflows_st()
  cdcFluPos_df <- cleanX_cdcFluview_fluPos_region()
  cdcH3_df <- cleanX_cdcFluview_H3_region() %>% select(-region)
  narrSpecHum_df <- cleanX_noaanarrSpecHum_st()
  
  #### join data ####
  dummy_df <- full_join(mod_df, imsCov_df, by = c("year", "abbr"))
  dummy_df2 <- full_join(dummy_df, cpsasecInsured_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipeIncome_df, by = c("year", "fips")) %>%
    full_join(censusChildPop_st, by = c("year", "fips")) %>%
    full_join(censusAdultPop_st, by = c("year", "fips")) %>%
    full_join(ahrfHospAccess_df, by = c("year", "fips")) %>%
    full_join(acsCommut_prep, by = c("year", "fips" = "fips_wrk")) %>%
    full_join(btsPass_prep, by = c("season", "fips" = "fips_dest")) %>%
    full_join(cdcFluPos_df, by = c("season", "fips")) %>%
    full_join(cdcH3_df, by = c("season", "fips")) %>%
    full_join(narrSpecHum_df, by = c("season", "fips")) %>%
    group_by(season) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerProvider)) %>%
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_income = centerStandardize(income)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>% 
    mutate(X_commute = centerStandardize(commutInflows_prep/pop)) %>%
    mutate(X_flight = centerStandardize(pass_prep/pop)) %>%
    mutate(X_fluPos = centerStandardize(fluPos)) %>%
    mutate(X_H3 = centerStandardize(H3)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    ungroup %>%
    select(-adjProviderCoverage, -visitsPerProvider, -insured, -income, -child, -adult, -hospitalAccess, -commutInflows_prep, -pass_prep, -fluPos, -H3, -humidity) %>%
    filter(season %in% 2:9)
  
  return(full_df)
}


#### functions for shapefile manipulation ################################

read_shapefile_st <- function(filepathList){
  # read state shapefile, export state adjacency matrix to file
  print(match.call())
  print(filepathList)

  # read shapefile into R
  st.poly.full <- readShapePoly(filepathList$path_shape_st) 
  # converts polygon data to adjacency matrix
  st.adjM.export <- poly2nb(st.poly.full) 
  # exports state adjacency matrix to file
  nb2INLA(filepathList$path_adjMxExport_st, st.adjM.export) 
  
  return(st.poly.full)
}
################################

combine_shapefile_modelData_st <- function(filepathList, modelData, seasNum){
  # merge model data with poly data for a single season
  print(match.call())

  # grab state shapefile data
  st.poly.full <- read_shapefile_st(filepathList)
  st.poly.only <- attr(st.poly.full, "data") # grab only polygon data

  # merge with model data
  modelData2 <- modelData %>%
    filter(season == seasNum) %>%
    select(-state)
  modelData3 <- tbl_df(st.poly.only) %>%
    mutate(fips = as.character(STATE)) %>%
    mutate(state = tolower(as.character(NAME))) %>%
    select(-STATE, -NAME) %>%
    left_join(modelData2, by = "fips") %>%
    mutate(ID = seq_along(fips)) 
 
  return(modelData3)
}

#### test the functions here  ################################



