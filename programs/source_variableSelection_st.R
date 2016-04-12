
## Name: Elizabeth Lee
## Date: 3/30/16
## Function: functions to perform EDA for variable selection, create model_version data for EDA with linear models
## Filenames: reference_data/USstate_shapefiles/gz_2010_us_040_00_500k
## Data Source: shapefile from US Census 2010 - https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
## Notes: renamed from source_prepare_lmData_st.R on 4/11/16
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(dplyr); require(tidyr); require(GGally) 
require(INLA)

#### functions for data aggregation  ################################

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
################################

prepare_allCov_iliSum <- function(filepathList){
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
  ahrfHospAccess_df <- cleanX_ahrfHospitals_st() 
  ahrfPhysAccess_df <- cleanX_ahrfPhysicians_st() 
  censusPopDens_df <- cleanX_popDensity_st()
  censusHousDens_df <- cleanX_housDensity_st()
  acsCommut_prep <- cleanX_acsCommutInflows_st()
  btsPass_prep <- cleanX_btsPassInflows_st()
  narrSpecHum_df <- cleanX_noaanarrSpecHum_st()
  narrSfcTemp_df <- cleanX_noaanarrSfcTemp_st()
  
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
    left_join(narrSpecHum_df, by = c("season", "fips")) %>%
    left_join(narrSfcTemp_df, by = c("season", "fips")) %>%
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
    mutate(X_humidity = centerStandardize(humidity)) %>%
    mutate(X_temperature = centerStandardize(temperature)) %>%
    select(-adjProviderCoverage, -visitsPerProvider, -insured, -poverty, -income, -mcaidElig, -mcareElig, -hospitalAccess, -physicianAccess, -popDensity, -housDensity, -commutInflows_prep, -pass_prep, -humidity, -temperature) %>%
    ungroup
  
  return(full_df)
}

#### functions for pairwise comparison ################################
pairs_scatterplotMatrix <- function(full_df){
  # return scatterplot matrix of all variables pooled across states & seasons
  print(match.call())
  
  datOnly <- full_df %>%
    select(-fips, -abbr, -state, -lat, -lon, -season, -year, -pop, -E)
  
  return(ggpairs(datOnly))
}
################################

pairs_corrMatrix <- function(full_df){
  # return correlation matrix for all variables pooled across states & seasons
  print(match.call())
  
  datOnly <- full_df %>% 
    select(-fips, -abbr, -state, -lat, -lon, -season, -year, -pop, -E)
  
  return(ggcorr(datOnly, method = c("pairwise", "spearman"), label = TRUE))
}


#### functions for single variable modeling ################################
subset_singleVariable_data <- function(full_df, s, covariate){
  # subset data for single response & covariate modeling
  print(match.call())
  
  # subset data according to season and covariate in function arguments
  mod_df <- full_df %>%
    filter(season == s) %>%
    rename_(varInterest = covariate) %>%
    select(fips, season, logy, logE, varInterest) %>%
    mutate(ID = seq_along(fips))
  
  return(mod_df)
}
################################

model_singleVariable_inla_st <- function(mod_df, respCode, s, covariate){
  # inla model for single response and covariate, output fixed effect coeff
  print(match.call())
  
  formula <- logy ~ varInterest + f(ID, model = "iid")
  
  mod <- inla(formula, family = "gaussian", data = mod_df, 
              control.predictor = list(compute = TRUE), # compute summary statistics on fitted values
              control.compute = list(dic = TRUE),
              # verbose = TRUE,
              offset = logE) # offset of expected cases
  
  names(mod$summary.fixed) <- c("mean", "sd", "q_025", "q_5", "q_975", "mode", "kld")
  modOutput <- tbl_df(mod$summary.fixed) %>%
    mutate(RV = rownames(mod$summary.fixed)) %>%
    filter(RV == "varInterest") %>%
    select(RV, mode, q_025, q_975) 
  
  # data to save
  coefRow <- list(respCode = respCode, singleCov = covariate, season = s, exportDate = as.character(Sys.Date()), coefMode = modOutput$mode, coefQ025 = modOutput$q_025, coefQ975 = modOutput$q_975, DIC = mod$dic$dic)

  return(coefRow)
}


#### test the functions here  ################################

# test <- model_singleVariable_inla_st(allDat2, "iliSum", 2, "X_poverty")
