
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
    mutate(O_careseek = centerStandardize(visitsPerProvider)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_H3 = centerStandardize(H3)) %>%
    ungroup %>%
    filter(fips_st %in% continentalOnly) %>% # include data for continental states only
    select(-stateID, -adjProviderCoverage, -visitsPerProvider, -poverty, -H3) %>%
    filter(season %in% 2:9)
  
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
    filter(season %in% 2:9)
  
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
    filter(season %in% 2:9)
  
  return(full_df)
}


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

convert_2stageModelData_sharedPredictors <- function(modData_seas){
  # prepare data seasonal model data for 2 stage (hurdle) model in INLA 
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
################################

convert_2stageModelData_separatePredictors <- function(modData_seas){
  # prepare data seasonal model data for 2 stage (hurdle) model in INLA 
  print(match.call())
  
  # create response matrix with 0s and non-zeros
  Y <- modData_seas %>% 
    select(y) %>%
    mutate(y0 = ifelse(y == 0, y, NA)) %>%
    mutate(y1 = ifelse(y > 0, y, NA)) %>%
    select(-y) %>%
    data.matrix
  
  # create matrix for 0s: response, predictors, random effects
  Mx0 <- modData_seas %>%
    select(y, contains("X_"), contains("O_")) %>%
    mutate(intercept = 1) %>%
    mutate_each(funs(zero = ifelse(y == 0, ., NA))) %>%
    select(contains("_zero")) %>%
    select(-y_zero)
  
  # create matrix for >0s: response, predictors, random effects
  Mx1 <- modData_seas %>%
    select(y, contains("X_"), contains("O_")) %>%
    mutate(intercept = 1) %>%
    mutate_each(funs(nonzero = ifelse(y > 0, ., NA))) %>%
    select(contains("_nonzero")) %>%
    select(-y_nonzero)
  
  # create matrix for non-driver predictors (random effects & offset)
  Mx_effects <- modData_seas %>%
    select(fips, fips_st, regionID, logE)
  
  # convert matrix information to a list of lists/matrixes
  Mx <- cbind(Mx_effects, Mx0, Mx1)
  modData_seas_lists <- list()
  for (column in colnames(Mx)){
    modData_seas_lists[[column]] <- Mx[[column]]
  }
  # add Y response matrix as a list
  modData_seas_lists[['Y']] <- Y
  
  return(modData_seas_lists)
}

#### test the functions here  ################################



