
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
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st() %>% rename(st = location)
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() %>% rename(st = location)
  # all region tables
  cdcH3_df <- cleanX_cdcFluview_H3_region()
  
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
    select(-fips_st, -adjProviderCoverage, -visitsPerProvider, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -commutInflows_prep, -pass, -infantAnyVax, -elderlyAnyVax, -H3, -humidity) %>%
    filter(season %in% 2:9)
  
  return(full_df)
}

#### functions for shapefile manipulation ################################
read_shapefile_cty <- function(filepathList){
  # read county shapefile, export county adjacency matrix to file
  print(match.call())
  print(filepathList)
  
  # read shapefile into R
  cty.poly.full <- readShapePoly(filepathList$path_shape_cty) 
  # converts polygon data to adjacency matrix
  cty.adjM.export <- poly2nb(cty.poly.full) 
  # exports state adjacency matrix to file
  nb2INLA(filepathList$path_adjMxExport_cty, cty.adjM.export) 
  
  return(cty.poly.full)
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
    mutate(ID = seq_along(fips)) 
 
  return(modelData3)
}

#### test the functions here  ################################



