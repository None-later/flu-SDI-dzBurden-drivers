
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

testing_module1 <- function(filepathList){
  # iliSum response, all sampling effort, and driver variables
  # y = response, E = expected response
  print(match.call())
  print(filepathList)
  
  #### import data ####
  mod_cty_df <- cleanR_iliSum_cty(filepathList)
  imsCov_cty_df <- cleanO_imsCoverage_cty()
  cdcH3_df <- cleanX_cdcFluview_H3_region()

  # list of continental states
  statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
  continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
  
  #### join data ####
  dummy_df2 <- full_join(mod_cty_df, imsCov_cty_df, by = c("year", "fips")) %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% # region is linked by state fips code
    full_join(cdcH3_df, by = c("season", "fips_st" = "fips")) %>%
    rename(regionID = region)
  
  full_df <- dummy_df2 %>%
    group_by(season) %>%
    mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
    mutate(O_careseek = centerStandardize(visitsPerPop)) %>%
    ungroup %>%
    filter(fips_st %in% continentalOnly) %>% # include data for continental states only
    select(-stateID, -adjProviderCoverage, -visitsPerProvider, -visitsPerPop, -H3) %>%
    filter(season %in% 2:9) %>%
    mutate(logE = log(E))
  
  return(full_df)
}
################################

testing_module2 <- function(filepathList){
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

testing_export_summaryStats_hurdle_wHyperpar <- function(exportPath, modelOutput, rdmFxTxt, modCodeString, dbCodeString, season){
  # export summary statistics of INLA model output for hurdle model variables -- fixed and random effects in the same file
  print(match.call())
  
  ## change variable names output from INLA ##
  names(modelOutput$summary.fixed) <- c("mean", "sd", "q_025", "q_5", "q_975", "mode", "kld")
  names(modelOutput$summary.hyperpar) <- names(modelOutput$summary.fixed)[1:6] # 8/17/16 add hyperpar export

  # clean fixed effects summary statistics output from INLA
  summaryFixed <- tbl_df(modelOutput$summary.fixed) %>%
    mutate(RV = rownames(modelOutput$summary.fixed)) %>%
    mutate(effectType = "fixed") %>%
    mutate(likelihood = ifelse(grepl("_bin", RV, fixed=TRUE), "binomial", ifelse(grepl("_nonzero", RV, fixed=TRUE), "gamma", NA))) %>%
    select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
  # clean hyperpar summary statistics output from INLA
  summaryHyperpar <- tbl_df(modelOutput$summary.hyperpar) %>%
    mutate(RV = rownames(modelOutput$summary.hyperpar)) %>%
    mutate(effectType = "hyperpar", kld = NA) %>%
    mutate(likelihood = ifelse(grepl("_bin", RV, fixed=TRUE), "binomial", ifelse(grepl("_nonzero", RV, fixed=TRUE), "gamma", NA))) %>%
    select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
  
  ## control flow for summary.random ##
  if (length(modelOutput$summary.random)){
    # random effects for binomial model #
    names(modelOutput$summary.random$fips_bin) <- c("ID", names(modelOutput$summary.fixed))
    # random effects for nonzero model (gamma) #
    names(modelOutput$summary.random$fips_nonzero) <- c("ID", names(modelOutput$summary.fixed))
    
    # clean random effects summary statistics output from INLA
    summaryRandomFips <- bind_rows(modelOutput$summary.random$fips_bin %>% mutate(likelihood = "binomial"), 
                                   modelOutput$summary.random$fips_nonzero %>% mutate(likelihood = "gamma")) %>%
      mutate(effectType = "spatial") 
    
    # bind random effects summary statistics
    summaryRandom <- summaryRandomFips %>%
      rename(RV = ID) %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    
    summaryHyperpar <- bind_rows(summaryRandom, summaryHyperpar)
  }
    
    # bind data together
    summaryStats <- bind_rows(summaryFixed, summaryHyperpar) %>%
      mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date())) %>%
      select(modCodeStr, dbCodeStr, season, exportDate, RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
  
  # export data to file
  write_csv(summaryStats, exportPath)
  
}
################################

testing_export_summaryStats_hurdle <- function(exportPath, modelOutput, rdmFxTxt, modCodeString, dbCodeString, season){
  # export summary statistics of INLA model output for hurdle model variables -- fixed and random effects in the same file
  print(match.call())
  
  ## change variable names output from INLA ##
  names(modelOutput$summary.fixed) <- c("mean", "sd", "q_025", "q_5", "q_975", "mode", "kld")

  # clean fixed effects summary statistics output from INLA
  summaryFixed <- tbl_df(modelOutput$summary.fixed) %>%
    mutate(RV = rownames(modelOutput$summary.fixed)) %>%
    mutate(effectType = "fixed") %>%
    mutate(likelihood = ifelse(grepl("_bin", RV, fixed=TRUE), "binomial", ifelse(grepl("_nonzero", RV, fixed=TRUE), "gamma", NA))) %>%
    select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
  
  ## control flow for summary.random ##
  if (length(modelOutput$summary.random)){
    # random effects for binomial model #
    names(modelOutput$summary.random$fips_bin) <- c("ID", names(modelOutput$summary.fixed))
    # random effects for nonzero model (gamma) #
    names(modelOutput$summary.random$fips_nonzero) <- c("ID", names(modelOutput$summary.fixed))
    
    # clean random effects summary statistics output from INLA
    summaryRandomFips <- bind_rows(modelOutput$summary.random$fips_bin %>% mutate(likelihood = "binomial"), 
                                   modelOutput$summary.random$fips_nonzero %>% mutate(likelihood = "gamma")) %>%
      mutate(effectType = "spatial") 
    
    # bind random effects summary statistics
    summaryRandom <- summaryRandomFips %>%
      rename(RV = ID) %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    
    summaryFixed <- bind_rows(summaryRandom, summaryFixed)
  }
  
  # bind data together
  summaryStats <- summaryFixed %>%
    mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date())) %>%
    select(modCodeStr, dbCodeStr, season, exportDate, RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
  
  # export data to file
  write_csv(summaryStats, exportPath)
  
}
################################



################################



#### test the functions here  ################################



