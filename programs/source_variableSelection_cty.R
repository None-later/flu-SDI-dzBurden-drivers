
## Name: Elizabeth Lee
## Date: 6/2/16
## Function: functions to perform EDA for variable selection at cty level
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(dplyr); require(tidyr)
# source("source_clean_data_functions.R")

#### functions for data aggregation  ################################

prepare_allCov_iliSum_cty <- function(filepathList){
  # iliSum response, all sampling effort, and driver variables
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_iliSum_shift1_cty(filepathList) # 1/5/17
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  imsCareseek_cty_df <- cleanO_imsCareseekTot_cty() # 1/5/17 visitsPerPop from sdi flu data
  imsCareseekChild_cty_df <- cleanO_imsCareseekChild_cty() # 1/5/17 child visitsPerPop from sdi flu data
  imsCareseekAdult_cty_df <- cleanO_imsCareseekAdult_cty() # 1/5/17 adult visitsPerPop from sdi flu data
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  saipeInc_cty_df <- cleanX_saipeIncome_cty()
  ahrfMcaid_cty_df <- cleanX_ahrfMedicaidEligibles_cty()
  ahrfMcare_cty_df <- cleanX_ahrfMedicareEligibles_cty()
  censusInfTodPop_cty_df <- cleanX_censusInfantToddlerPop_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  censusEldPop_cty_df <- cleanX_censusElderlyPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  ahrfPhys_cty_df <- cleanX_ahrfPhysicians_cty()
  brfssMedCost_cty_df <- cleanX_brfssMedCost_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  housDens_cty_df <- cleanX_housDensity_cty()
  acsCommutInflows_cty_prep <- cleanX_acsCommutInflows_cty()
  btsPass_cty_df <- cleanX_btsPassInflows_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  narrSfcTemp_cty_df <- cleanX_noaanarrSfcTemp_cty()
  wonderPollution_cty_df <- cleanX_wonderAirParticulateMatter_cty()
  cbpSocialAssoc_cty_df <- cleanX_cbpSocialAssoc_cty()
  acsOneParentHH_cty_df <- cleanX_acsOneParentFamHH_cty()
  acsOnePersonHH_cty_df <- cleanX_acsOnePersonHH_cty()
  acsAvgHHSize_cty_df <- cleanX_acsAvgHHSize_cty()
  brfssPoorHealth_cty_df <- cleanX_brfssPoorHealth_cty()
  brfssUnhealthyDays_cty_df <- cleanX_brfssUnhealthyDays_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcFluPos_df <- cleanX_cdcFluview_fluPos_region()
  cdcH3_df <- cleanX_cdcFluview_H3_region() %>% select(-region)
  cdcH3A_df <- cleanX_cdcFluview_H3A_region() %>% select(-region)
  cdcB_df <- cleanX_cdcFluview_B_region() %>% select(-region)
  protectedPriorSeas_df <- cleanX_protectedFromPrevSeason_cty(filepathList)
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(imsCareseek_cty_df, by = c("season", "fips")) %>%
    full_join(imsCareseekChild_cty_df, by = c("season", "fips")) %>%
    full_join(imsCareseekAdult_cty_df, by = c("season", "fips")) %>%
    full_join(saipeInc_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfMcaid_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfMcare_cty_df, by = c("year", "fips")) %>%
    full_join(censusInfTodPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusEldPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfPhys_cty_df, by = c("year", "fips")) %>%
    full_join(brfssMedCost_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(housDens_cty_df, by = c("year", "fips")) %>%
    full_join(acsCommutInflows_cty_prep, by = c("year", "fips" = "fips_wrk")) %>%
    full_join(btsPass_cty_df, by = c("season", "fips" = "fips_dest")) %>%
    mutate(pass = ifelse(is.na(pass), 0, pass)) %>% # counties with NA from merge had 0 passengers entering
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcFluPos_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcH3_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcH3A_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcB_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(protectedPriorSeas_df, by = c("season", "fips")) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    full_join(narrSfcTemp_cty_df, by = c("season", "fips")) %>%
    full_join(wonderPollution_cty_df, by = c("season", "fips")) %>%
    full_join(cbpSocialAssoc_cty_df, by = c("fips", "year")) %>%
    full_join(acsOneParentHH_cty_df, by = c("fips", "year")) %>%
    full_join(acsOnePersonHH_cty_df, by = c("fips", "year")) %>%
    full_join(acsAvgHHSize_cty_df, by = c("fips", "year")) %>%
    full_join(brfssPoorHealth_cty_df, by = c("fips", "year")) %>%
    full_join(brfssUnhealthyDays_cty_df, by = c("fips", "year")) %>%
    # group_by(season) %>% # 1/13/17 data fxn for multi-season models
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseekT = centerStandardize(visitsPerPopT)) %>%
    mutate(O_careseekC = centerStandardize(visitsPerPopC)) %>%
    mutate(O_careseekA = centerStandardize(visitsPerPopA)) %>%
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_income = centerStandardize(income)) %>%
    mutate(X_mcaid = centerStandardize(mcaidElig)) %>%
    mutate(X_mcare = centerStandardize(mcareElig)) %>%
    mutate(X_infantToddler = centerStandardize(infantToddler)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_elderly = centerStandardize(elderly)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>%
    mutate(X_physaccess = centerStandardize(physicianAccess)) %>%
    mutate(X_medcost = centerStandardize(medcost)) %>%
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_housdensity = centerStandardize(housDensity)) %>%
    mutate(X_commute = centerStandardize(commutInflows_prep)) %>% # commutInflows_prep/pop and commutInflows_prep raw look similar in EDA choropleths
    mutate(X_flight = centerStandardize(pass)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_fluPos = centerStandardize(fluPos)) %>%
    mutate(X_H3 = centerStandardize(H3)) %>%
    mutate(X_H3A = centerStandardize(prop_H3_a)) %>%
    mutate(X_B = centerStandardize(prop_b_all)) %>%
    mutate(X_priorImmunity = centerStandardize(protectionPrevSeason)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    mutate(X_temperature = centerStandardize(temperature)) %>%
    mutate(X_pollution = centerStandardize(avg_pm)) %>%
    mutate(X_socialOrgs = centerStandardize(socialOrgs)) %>%
    mutate(X_singleParentHH = centerStandardize(prop_oneParentFamHH)) %>%
    mutate(X_singlePersonHH = centerStandardize(perc_hh_1p)) %>%
    mutate(X_avgHHSize = centerStandardize(avgHHSize)) %>%
    mutate(X_poorHealth = centerStandardize(poorhealth)) %>%
    mutate(X_unhealthyDays = centerStandardize(unhealthydays)) %>%
    # ungroup %>% # 1/13/17 data fxn for multi-season models
    filter(fips_st %in% continentalOnly) %>%
    mutate(logE = log(E), logy = log(y1)) %>%
    select(-fips_st, -adjProviderCoverage, -visitsPerPopT, -visitsPerPopC, -visitsPerPopA, -insured, -poverty, -income, -mcaidElig, -mcareElig, -infantToddler, -child, -adult, -elderly, -hospitalAccess, -physicianAccess, -medcost, -popDensity, -housDensity, -commutInflows_prep, -pass, -fluPos, -H3, -prop_H3_a, -prop_b_all, -protectionPrevSeason, -humidity, -temperature, -avg_pm, -socialOrgs, -prop_oneParentFamHH, -perc_hh_1p, -avgHHSize, -poorhealth, -unhealthydays) %>%
    filter(season %in% 3:9) %>%
    mutate(ID = seq_along(fips))

    return(full_df)
}
################################

prepare_allCov_iliSum_cty_raw <- function(filepathList){
  # iliSum response, all sampling effort, and driver variables
  # y = response, E = expected response
  print(match.call())
  print(filepathList)

  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_iliSum_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  imsCareseek_cty_df <- cleanO_imsCareseekTot_cty()
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  saipeInc_cty_df <- cleanX_saipeIncome_cty()
  ahrfMcaid_cty_df <- cleanX_ahrfMedicaidEligibles_cty()
  ahrfMcare_cty_df <- cleanX_ahrfMedicareEligibles_cty()
  censusInfTodPop_cty_df <- cleanX_censusInfantToddlerPop_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  censusEldPop_cty_df <- cleanX_censusElderlyPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  ahrfPhys_cty_df <- cleanX_ahrfPhysicians_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  housDens_cty_df <- cleanX_housDensity_cty()
  acsCommutInflows_cty_df <- cleanX_acsCommutInflows_cty()
  btsPass_cty_df <- cleanX_btsPassInflows_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  narrSfcTemp_cty_df <- cleanX_noaanarrSfcTemp_cty()
  wonderPollution_cty_df <- cleanX_wonderAirParticulateMatter_cty()
  acsOnePersonHH_cty_df <- cleanX_acsOnePersonHH_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcFluPos_df <- cleanX_cdcFluview_fluPos_region()
  cdcH3_df <- cleanX_cdcFluview_H3_region() %>% select(-region)
  cdcH3A_df <- cleanX_cdcFluview_H3A_region()
  cdcB_df <- cleanX_cdcFluview_B_region() %>% select(-region)
  protectedPriorSeas_df <- cleanX_protectedFromPrevSeason_cty(filepathList)
  # graph index IDs
  graphIdx_df <- clean_graphIDx(filepathList, "county")
  graphIdx_st_df <- clean_graphIDx(filepathList, "state")
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df1 <- full_join(dummy_df, imsCareseek_cty_df, by = c("season", "fips"))
  dummy_df2 <- full_join(dummy_df1, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(saipeInc_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfMcaid_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfMcare_cty_df, by = c("year", "fips")) %>%
    full_join(censusInfTodPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusEldPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfPhys_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(housDens_cty_df, by = c("year", "fips")) %>%
    full_join(acsCommutInflows_cty_df, by = c("year", "fips" = "fips_wrk")) %>%
    full_join(btsPass_cty_df, by = c("season", "fips" = "fips_dest")) %>%
    mutate(pass = ifelse(is.na(pass), 0, pass)) %>% # counties with NA from merge had 0 passengers entering
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    full_join(narrSfcTemp_cty_df, by = c("season", "fips")) %>%
    full_join(wonderPollution_cty_df, by = c("season", "fips")) %>%
    full_join(acsOnePersonHH_cty_df, by = c("fips", "year")) %>%
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcFluPos_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcH3_df, by = c("season", "fips_st" = "fips")) %>%
     full_join(cdcH3A_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcB_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(protectedPriorSeas_df, by = c("season", "fips")) %>%
    full_join(graphIdx_df, by = "fips") %>%
    full_join(graphIdx_st_df, by = "fips_st") %>%
    rename(rO_imscoverage = adjProviderCoverage) %>%
    rename(rO_careseek = visitsPerPopT) %>%
    rename(rO_insured = insured) %>%
    rename(rX_poverty = poverty) %>%
    rename(rX_income = income) %>%
    rename(rX_mcaid = mcaidElig) %>%
    rename(rX_mcare = mcareElig) %>%
    rename(rX_infantToddler = infantToddler) %>%
    rename(rX_child = child) %>%
    rename(rX_adult = adult) %>%
    rename(rX_elderly = elderly) %>%
    rename(rX_hospaccess = hospitalAccess) %>% 
    rename(rX_physaccess = physicianAccess) %>%
    rename(rX_popdensity = popDensity) %>%
    rename(rX_housdensity = housDensity) %>%
    rename(rX_commute = commutInflows_prep) %>% # commutInflows_prep/pop and commutInflows_prep raw look similar in EDA choropleths
    rename(rX_flight = pass) %>%
    rename(rX_humidity = humidity) %>%
    rename(rX_temperature = temperature) %>%
    rename(rX_pollution = avg_pm) %>%
    rename(rX_singlePersonHH = perc_hh_1p) %>%
    rename(rX_vaxcovI = infantAnyVax) %>%
    rename(rX_vaxcovE = elderlyAnyVax) %>%
    rename(rX_fluPos = fluPos) %>%
    rename(rX_H3 = H3) %>%
    rename(rX_H3A = prop_H3_a) %>%
    rename(rX_B = prop_b_all) %>%
    filter(fips_st %in% continentalOnly) %>%
    filter(season %in% 3:9) %>%
    mutate(ID = seq_along(fips))
  
  return(full_df)
}
################################

model_8fV7_8aV7_diff_raw <- function(filepathList){
    # 4/8/17 covariates that are only in models 8f_v7 and not 8a_v7, for additional diagnostics in EDA_choro_inlaPredictors.R
    print(match.call())
    print(filepathList)

    # list of continental states
    statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
    continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist

    #### import data ####
    narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_wksToEpi_cty(filepathList)
    wonderPollution_cty_df <- cleanX_wonderAirParticulateMatter_wksToEpi_cty(filepathList)

    full_df <- full_join(narrSpecHum_cty_df, wonderPollution_cty_df, by = c("season", "fips")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>%
    rename(rX_humidity_8f = humidity) %>%
    rename(rX_pollution_8f = avg_pm) %>%
    filter(fips_st %in% continentalOnly) %>%
    filter(season %in% 3:9) 

    return(full_df)
}
################################

model8a_iliSum_v7_raw <- function(filepathList){
  # 3/4/17 model 8a_v7 -- total population
  # raw predictors for exploratory analysis
  # with 2 spatial terms - state flight passenger, county neighbor
  # y1 = log(response+1), E = expected response+1
  print(match.call())
  print(filepathList)
  
  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### import data ####
  # IMS Health based tables
  mod_cty_df <- cleanR_iliSum_shift1_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  imsCareseek_cty_df <- cleanO_imsCareseekTot_cty() # 1/5/17 visitsPerPop from sdi flu data
  # all county tables
  sahieIns_cty_df <- cleanO_sahieInsured_cty()
  saipePov_cty_df <- cleanX_saipePoverty_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  housDens_cty_df <- cleanX_housDensity_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  wonderPollution_cty_df <- cleanX_wonderAirParticulateMatter_cty()
  acsOnePersonHH_cty_df <- cleanX_acsOnePersonHH_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcH3A_df <- cleanX_cdcFluview_H3A_region()
  cdcB_df <- cleanX_cdcFluview_B_region() %>% select(-region)
  protectedPriorSeas_df <- cleanX_protectedFromPrevSeason_cty(filepathList)
 
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(imsCareseek_cty_df, by = c("season", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(housDens_cty_df, by = c("year", "fips")) %>%
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcH3A_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcB_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region) %>%
    full_join(protectedPriorSeas_df, by = c("season", "fips")) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    full_join(wonderPollution_cty_df, by = c("season", "fips")) %>%
    full_join(acsOnePersonHH_cty_df, by = c("fips", "year")) %>%
    mutate(rO_imscoverage = adjProviderCoverage) %>%
    mutate(rO_careseek = visitsPerPopT) %>%
    mutate(rO_insured = insured) %>%
    mutate(rX_poverty = poverty) %>%
    mutate(rX_child = child) %>%
    mutate(rX_adult = adult) %>%
    mutate(rX_hospaccess = hospitalAccess) %>%
    mutate(rX_popdensity = popDensity) %>%
    mutate(rX_housdensity = housDensity) %>%
    mutate(rX_vaxcovI = infantAnyVax) %>%
    mutate(rX_vaxcovE = elderlyAnyVax) %>%
    mutate(rX_H3A = prop_H3_a) %>%
    mutate(rX_B = prop_b_all) %>%
    mutate(rX_priorImmunity = protectionPrevSeason) %>%
    mutate(rX_humidity = humidity) %>%
    mutate(rX_pollution = avg_pm) %>%
    mutate(rX_singlePersonHH = perc_hh_1p) %>%
    filter(fips_st %in% continentalOnly) %>%
    mutate(logE = log(E), y1 = log(y1)) %>% # model response y1 = log(y+1)
    select(-stateID, -adjProviderCoverage, -visitsPerPopT, -insured, -poverty, -child, -adult, -hospitalAccess, -popDensity, -housDensity, -infantAnyVax, -elderlyAnyVax, -prop_H3_a, -prop_b_all, -protectionPrevSeason, -humidity, -avg_pm, -perc_hh_1p) %>%
    filter(season %in% 3:9) %>%
    mutate(ID = seq_along(fips))
  
  return(full_df)
}
################################

prepare_allCov_logIliSum_cty <- function(filepathList){
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
  saipeInc_cty_df <- cleanX_saipeIncome_cty()
  ahrfMcaid_cty_df <- cleanX_ahrfMedicaidEligibles_cty()
  ahrfMcare_cty_df <- cleanX_ahrfMedicareEligibles_cty()
  censusInfTodPop_cty_df <- cleanX_censusInfantToddlerPop_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  censusEldPop_cty_df <- cleanX_censusElderlyPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  ahrfPhys_cty_df <- cleanX_ahrfPhysicians_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  housDens_cty_df <- cleanX_housDensity_cty()
  acsCommutInflows_cty_prep <- cleanX_acsCommutInflows_cty()
  btsPass_cty_df <- cleanX_btsPassInflows_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  narrSfcTemp_cty_df <- cleanX_noaanarrSfcTemp_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcFluPos_df <- cleanX_cdcFluview_fluPos_region()
  cdcH3_df <- cleanX_cdcFluview_H3_region() %>% select(-region)
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(saipeInc_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfMcaid_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfMcare_cty_df, by = c("year", "fips")) %>%
    full_join(censusInfTodPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusEldPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfPhys_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(housDens_cty_df, by = c("year", "fips")) %>%
    full_join(acsCommutInflows_cty_prep, by = c("year", "fips" = "fips_wrk")) %>%
    full_join(btsPass_cty_df, by = c("season", "fips" = "fips_dest")) %>%
    mutate(pass = ifelse(is.na(pass), 0, pass)) %>% # counties with NA from merge had 0 passengers entering
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcFluPos_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcH3_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    full_join(narrSfcTemp_cty_df, by = c("season", "fips")) %>%
    group_by(season) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerProvider)) %>%
    mutate(O_insured = centerStandardize(insured)) %>%
    mutate(X_poverty = centerStandardize(poverty)) %>%
    mutate(X_income = centerStandardize(income)) %>%
    mutate(X_mcaid = centerStandardize(mcaidElig)) %>%
    mutate(X_mcare = centerStandardize(mcareElig)) %>%
    mutate(X_infantToddler = centerStandardize(infantToddler)) %>%
    mutate(X_child = centerStandardize(child)) %>%
    mutate(X_adult = centerStandardize(adult)) %>%
    mutate(X_elderly = centerStandardize(elderly)) %>%
    mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>% 
    mutate(X_physaccess = centerStandardize(physicianAccess)) %>%
    mutate(X_popdensity = centerStandardize(popDensity)) %>%
    mutate(X_housdensity = centerStandardize(housDensity)) %>%
    mutate(X_commute = centerStandardize(commutInflows_prep)) %>% # commutInflows_prep/pop and commutInflows_prep raw look similar in EDA choropleths
    mutate(X_flight = centerStandardize(pass)) %>%
    mutate(X_vaxcovI = centerStandardize(infantAnyVax)) %>%
    mutate(X_vaxcovE = centerStandardize(elderlyAnyVax)) %>%
    mutate(X_fluPos = centerStandardize(fluPos)) %>%
    mutate(X_H3 = centerStandardize(H3)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    mutate(X_temperature = centerStandardize(temperature)) %>%
    ungroup %>%
    select(-fips_st, -adjProviderCoverage, -visitsPerProvider, -insured, -poverty, -income, -mcaidElig, -mcareElig, -infantToddler, -child, -adult, -elderly, -hospitalAccess, -physicianAccess, -popDensity, -housDensity, -commutInflows_prep, -pass, -fluPos, -H3, -humidity, -temperature) %>%
    mutate(y1 = log(y1)) %>% # log response
    filter(season %in% 2:9) %>%
    mutate(ID = seq_along(fips))
  
  return(full_df)
}
################################

prepare_allCov_logIliSum_cty_raw <- function(filepathList){
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
  saipeInc_cty_df <- cleanX_saipeIncome_cty()
  ahrfMcaid_cty_df <- cleanX_ahrfMedicaidEligibles_cty()
  ahrfMcare_cty_df <- cleanX_ahrfMedicareEligibles_cty()
  censusInfTodPop_cty_df <- cleanX_censusInfantToddlerPop_cty()
  censusChPop_cty_df <- cleanX_censusChildPop_cty()
  censusAdPop_cty_df <- cleanX_censusAdultPop_cty()
  censusEldPop_cty_df <- cleanX_censusElderlyPop_cty()
  ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
  ahrfPhys_cty_df <- cleanX_ahrfPhysicians_cty()
  popDens_cty_df <- cleanX_popDensity_cty()
  housDens_cty_df <- cleanX_housDensity_cty()
  acsCommutInflows_cty_df <- cleanX_acsCommutInflows_cty()
  btsPass_cty_df <- cleanX_btsPassInflows_cty()
  narrSpecHum_cty_df <- cleanX_noaanarrSpecHum_cty()
  narrSfcTemp_cty_df <- cleanX_noaanarrSfcTemp_cty()
  # all state tables 
  infantAnyVax_st_df <- cleanX_nisInfantAnyVaxCov_st()
  elderlyAnyVax_st_df <- cleanX_brfssElderlyAnyVaxCov_st() 
  # all region tables
  cdcFluPos_df <- cleanX_cdcFluview_fluPos_region()
  cdcH3_df <- cleanX_cdcFluview_H3_region() %>% select(-region)
  
  #### join data ####
  dummy_df <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips"))
  dummy_df2 <- full_join(dummy_df, sahieIns_cty_df, by = c("year", "fips"))
  
  full_df <- full_join(dummy_df2, saipePov_cty_df, by = c("year", "fips")) %>%
    full_join(saipeInc_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfMcaid_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfMcare_cty_df, by = c("year", "fips")) %>%
    full_join(censusInfTodPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusChPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusAdPop_cty_df, by = c("year", "fips")) %>%
    full_join(censusEldPop_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfHosp_cty_df, by = c("year", "fips")) %>%
    full_join(ahrfPhys_cty_df, by = c("year", "fips")) %>%
    full_join(popDens_cty_df, by = c("year", "fips")) %>%
    full_join(housDens_cty_df, by = c("year", "fips")) %>%
    full_join(acsCommutInflows_cty_df, by = c("year", "fips" = "fips_wrk")) %>%
    full_join(btsPass_cty_df, by = c("season", "fips" = "fips_dest")) %>%
    mutate(pass = ifelse(is.na(pass), 0, pass)) %>% # counties with NA from merge had 0 passengers entering
    full_join(infantAnyVax_st_df, by = c("season", "st")) %>%
    full_join(elderlyAnyVax_st_df, by = c("season", "st")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcFluPos_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(cdcH3_df, by = c("season", "fips_st" = "fips")) %>%
    full_join(narrSpecHum_cty_df, by = c("season", "fips")) %>%
    full_join(narrSfcTemp_cty_df, by = c("season", "fips")) %>%
    group_by(season) %>%
    mutate(rO_imscoverage = adjProviderCoverage) %>%
    mutate(rO_careseek = visitsPerProvider) %>%
    mutate(rO_insured = insured) %>%
    mutate(rX_poverty = poverty) %>%
    mutate(rX_income = income) %>%
    mutate(rX_mcaid = mcaidElig) %>%
    mutate(rX_mcare = mcareElig) %>%
    mutate(rX_infantToddler = infantToddler) %>%
    mutate(rX_child = child) %>%
    mutate(rX_adult = adult) %>%
    mutate(rX_elderly = elderly) %>%
    mutate(rX_hospaccess = hospitalAccess) %>% 
    mutate(rX_physaccess = physicianAccess) %>%
    mutate(rX_popdensity = popDensity) %>%
    mutate(rX_housdensity = housDensity) %>%
    mutate(rX_commute = commutInflows_prep) %>% # commutInflows_prep/pop and commutInflows_prep raw look similar in EDA choropleths
    mutate(rX_flight = pass) %>%
    mutate(rX_vaxcovI = infantAnyVax) %>%
    mutate(rX_vaxcovE = elderlyAnyVax) %>%
    mutate(rX_fluPos = fluPos) %>%
    mutate(rX_H3 = H3) %>%
    mutate(rX_humidity = humidity) %>%
    mutate(rX_temperature = temperature) %>%
    ungroup %>%
    select(-fips_st, -adjProviderCoverage, -visitsPerProvider, -insured, -poverty, -income, -mcaidElig, -mcareElig, -infantToddler, -child, -adult, -elderly, -hospitalAccess, -physicianAccess, -popDensity, -housDensity, -commutInflows_prep, -pass, -fluPos, -H3, -humidity, -temperature) %>%
    mutate(y1 = log(y1)) %>% # log response
    filter(season %in% 2:9) %>%
    mutate(ID = seq_along(fips))
  
  return(full_df)
}
################################

#### test the functions here  ################################

# test <- model_singleVariable_inla_st(allDat2, "iliSum", 2, "X_poverty")
