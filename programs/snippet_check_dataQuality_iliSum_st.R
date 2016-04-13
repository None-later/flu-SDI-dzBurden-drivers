
## Name: Elizabeth Lee
## Date: 4/12/16
## Function: check data availability for each covariate and response
## Filenames: dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: 
## Notes: RESOLVED mutate error -- updated to developer dplyr
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr) # clean_data_functions dependencies

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
rCode <- "iliSum"
seasons <- 2:9


#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_data_functions.R") # functions to clean original data sources
source("source_variableSelection_st.R") # functions for variable selection analyses

#### FILEPATHS #################################
setwd('../reference_data')
path_pop_st <- paste0(getwd(), "/pop_st_Census_00-10.csv")
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")

setwd("../R_export")
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
path_imsCov_st <- paste0(getwd(), "/physicianCoverage_IMSHealth_state.csv")
path_coefDat <- paste0(getwd(), sprintf("/VS_coefDat_%s_st.csv", rCode))

setwd(dirname(sys.frame(1)$ofile))
setwd("../graph_outputs")
path_pltExport <- paste0(getwd(), "/VS_analysisSuite_iliSum_st")

# put all paths in a list to pass them around in functions
path_list <- list(path_pop_st = path_pop_st,
                  path_abbr_st = path_abbr_st,
                  path_latlon_st = path_latlon_st,
                  path_response_st = path_response_st,
                  path_imsCov_st = path_imsCov_st)


##### testing functions ###########################

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
    full_join(ahrfMcaidElig_df, by = c("year", "fips")) %>%
    left_join(ahrfMcareElig_df, by = c("year", "fips")) %>%
    left_join(ahrfHospAccess_df, by = c("year", "fips")) %>%
    left_join(ahrfPhysAccess_df, by = c("year", "fips")) %>%
    left_join(censusPopDens_df, by = c("year", "fips")) %>%
    left_join(censusHousDens_df, by = c("year", "fips")) %>%
    full_join(acsCommut_prep, by = c("year", "fips" = "fips_wrk")) %>%
    left_join(btsPass_prep, by = c("season", "fips" = "fips_dest")) %>%
    left_join(narrSpecHum_df, by = c("season", "fips")) %>%
    left_join(narrSfcTemp_df, by = c("season", "fips")) %>%
    mutate(commutInflows = commutInflows_prep/pop) %>%
    mutate(passengers = pass_prep/pop)
  
  full_df2 <- full_df %>% 
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
    mutate(X_commute = centerStandardize(commutInflows)) %>%
    mutate(X_flight = centerStandardize(passengers)) %>%
    mutate(X_humidity = centerStandardize(humidity)) %>%
    mutate(X_temperature = centerStandardize(temperature)) %>%
    ungroup %>%
    # select(-adjProviderCoverage, -visitsPerProvider, -insured, -poverty, -income, -mcaidElig, -mcareElig, -hospitalAccess, -physicianAccess, -popDensity, -housDensity, -commutInflows_prep, -pass_prep, -humidity, -temperature) %>%
    select(-adjProviderCoverage, -visitsPerProvider, -insured, -poverty, -income, -hospitalAccess, -physicianAccess, -popDensity, -housDensity, -pass_prep, -humidity, -temperature) %>%
    filter(season %in% 2:9)
  
  return(full_df2)
}


##### testing area ###########################
allDat <- prepare_allCov_iliSum(path_list) 
allDat2 <- allDat %>% 
  select(-X_popdensity, -X_housdensity) # these var were available only for Census 2000 and Census 2010
summary(allDat2)

# check counts in each column
View(allDat2 %>% group_by(season) %>% summarise_each(funs(ct = sum(!is.na(.)))))

# components of centerStandardize
View(allDat2 %>% group_by(season) %>% select(fips, season, year, mcaidElig) %>% summarise(mn_mcaid = mean(mcaidElig, na.rm=T), sd_mcaid = sd(mcaidElig, na.rm=T)))
View(allDat2 %>% group_by(season) %>% select(fips, season, year, commutInflows) %>% summarise(mn_comm = mean(commutInflows, na.rm=T), sd_comm = sd(commutInflows, na.rm=T)))
# subset allData2 and recalculate centerStandardize
test <- allDat2 %>% select(fips, season, year, commutInflows, mcaidElig) %>% group_by(season) %>% mutate(xcomm_test = centerStandardize(commutInflows), xmcaid_test = centerStandardize(mcaidElig)) %>% ungroup %>% filter(season %in% 5:8)

allDat2 %>% select(fips, season, commutInflows, X_commute) %>% filter(season %in% 7:9) %>% group_by(season) %>% mutate(test = centerStandardize(commutInflows)) %>% filter(season == 7) # test works with single season

# 4/12/16: THIS SEEMS TO BE A BUG IN DPLYR?
# IF THERE ARE ALL NAS IN PREVIOUS GROUPS, GROUP_BY + MUTATE PRODUCES INAPPROPRIATE NAS
allDat2 %>% select(fips, season, mcareElig, X_elderly) %>% filter(season %in% 5:9) %>% group_by(season) %>% mutate(test = centerStandardize(mcareElig), test2 = mcareElig+10) %>% filter(season == 9) # compare seasons 6:9 vs. 5:9

# check which data are NAs
View(allDat2 %>% group_by(season) %>% summarise(comm = sum(!is.na(commutInflows_prep)), comm2 = sum(!is.na(commutInflows)), xcomm = sum(!is.na(X_commute)), mcaid = sum(!is.na(mcaidElig)), xmcaid = sum(!is.na(X_mcaid)), mcare = sum(!is.na(mcareElig)), xmcare = sum(!is.na(X_elderly))))
# check values for relevant data
View(allDat2 %>% select(fips, season, year, pop, commutInflows_prep, commutInflows, X_commute, mcaidElig, X_mcaid) %>% filter(season %in% 5:8) %>% arrange(fips, season))

# import all tables separately too
mod_df <- cleanR_iliSum_st(path_list)
imsCov_df <- cleanO_imsCoverage_st(path_list)
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

# check data availability
View(narrSpecHum_df %>% group_by(season) %>% summarise(ct = sum(!is.na(humidity))))

# check population data
pop_data <- clean_pop_st(path_list)
View(mod_df %>% filter(year == 2002) %>% select(fips, year, pop) %>% arrange(fips))
View(pop_data %>% group_by(year) %>% summarise(ct = sum(!is.na(pop))))
resp_data <- cleanR_iliSum_st(path_list)
View(resp_data %>% group_by(season) %>% summarise(ct = sum(!is.na(pop)))) # pop data are missing
View(resp_data %>% group_by(season) %>% summarise(ct = sum(!is.na(y)))) # pop data are missing


