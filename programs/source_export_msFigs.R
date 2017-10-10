
## Name: Elizabeth Lee
## Date: 1/16/17
## Function: general functions to generate MS figures
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(RColorBrewer); require(ggplot2); require(dplyr); require(tidyr); require(readr)
require(data.table)
require(lazyeval)
require(ggthemes)
setwd(dirname(sys.frame(1)$ofile))
source("source_export_inlaDiagnostics.R")
source("source_variableSelection_cty.R")
source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
source("source_clean_response_functions_st.R") # functions to clean response (st)
source("source_clean_data_functions.R") # functions to clean covariate data

#### processing functions ################################
################################
string_fit_fname <- function(modCodeStr){
  searchDir <-  paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_export/", modCodeStr, "/")
  return(grep("summaryStatsFitted_", list.files(path = searchDir, full.names = TRUE), value = TRUE))
}
################################
string_coef_fname <- function(modCodeStr){
    return(paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_export/", modCodeStr, "/summaryStats_", modCodeStr, ".csv"))
}
################################
string_ids_fname <- function(modCodeStr){
    return(paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_export/", modCodeStr, "/ids_", modCodeStr, ".csv"))
}
################################
stringLs_coef_fname <- function(modCodeStr){
  searchDir <- paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_export/", modCodeStr, "/")
  readfile_list <- grep("summaryStats_", list.files(path = searchDir, full.names = TRUE), value = TRUE)
  return(readfile_list)
}
################################
string_msFig_folder <- function(){
    return(paste0(dirname(sys.frame(1)$ofile), "/../graph_outputs/msFigs/"))
}
################################
string_refData_folder <- function(){
    return(paste0(dirname(sys.frame(1)$ofile), "/../reference_data/"))
}
################################
string_cdcData_folder <- function(){
  return(paste0(dirname(sys.frame(1)$ofile), "/../../../CDC_Source/Import_Data/"))
}
################################
import_stAbbr <- function(){
  print(match.call())
  returnDat <- read_csv(paste0(string_refData_folder(), "state_abbreviations_FIPS_region.csv"), col_types = "cc__") %>%
    mutate(State = tolower(State))
  return(returnDat)
}
################################
import_cdcRegILI <- function(){
  print(match.call())
  returnDat <- read_csv(paste0(string_cdcData_folder(), "all_cdc_source_data_HHSRegion.csv"), col_types = cols_only(uqidR = "c", reg = "i", yr = "i", wk = "i", season = "i", ilitot = "i", patients = "i", providers = "i", ili_5.24 = "i", ili_25.64 = "i")) %>%
    filter(season >= 3 & season <= 9) %>%
    group_by(season, reg) %>%
    summarise(ilitot = sum(ilitot, na.rm = TRUE), ili_5.24 = sum(ili_5.24, na.rm = TRUE), ili_25.64 = sum(ili_25.64, na.rm = TRUE), patients = sum(patients, na.rm = TRUE)) %>%
    mutate(tot_unwt_pct = ilitot/patients*100) %>%
    mutate(ch_unwt_totPct = ili_5.24/patients*100) %>%
    mutate(ad_unwt_totPct = ili_25.64/patients*100) %>%
    ungroup
  return(returnDat)
}
################################
import_cdcRegViral <- function(){
  print(match.call())
  returnDat <- read_csv(paste0(string_cdcData_folder(), "all_cdc_source_data_HHSRegion.csv"), col_types = cols_only(uqidR = "c", reg = "i", yr = "i", wk = "i", num_samples = "i", perc_pos = "d", season = "i")) %>%
    filter(season >= 3 & season <= 9) %>%
    mutate(num_pos = (perc_pos * num_samples)/100) %>%
    group_by(season, reg) %>%
    summarise(num_pos = sum(num_pos, na.rm = TRUE), num_samples = sum(num_samples, na.rm = TRUE)) %>%
    mutate(perc_pos = num_pos/num_samples*100) %>%
    ungroup
  return(returnDat)
}
################################
label_ecol_predictors <- function(){
    cleanRV <- c("humidity", "pollution", "popdensity", "housdensity", "child", "adult", "vaxcovI", "vaxcovE", "priorImmunity", "H3A", "B", "adult:H3A", "child:B", "hospaccess", "singlePersonHH", "poverty")
    pltLabels <- c("humidity", "pollution", "popDensity", "householdSize", "child", "adult", "toddlerVacc", "elderlyVacc", "priorImmunity", "fluH3", "fluB", "adult-fluH3", "child-fluB", "hospAccess", "onePersonHH", "poverty")

    dfLabels <- tbl_df(data.frame(RV = cleanRV, pltLabs = pltLabels, stringsAsFactors = FALSE))
    return(dfLabels)
}
################################
label_ecol_predictors_2009p <- function(){
  cleanRV <- c("humidity", "pollution", "popdensity", "housdensity", "child", "adult", "vaxcovC", "vaxcovA", "priorImmunity", "hospaccess", "singlePersonHH", "poverty")
  pltLabels <- c("humidity", "pollution", "popDensity", "householdSize", "child", "adult", "childVacc", "adultVacc", "priorImmunity", "hospAccess", "onePersonHH", "poverty")
  
  dfLabels <- tbl_df(data.frame(RV = cleanRV, pltLabs = pltLabels, stringsAsFactors = FALSE))
  return(dfLabels)
}
################################
label_meas_predictors <- function(){
    cleanRV <- c("insured", "imscoverage", "careseek")
    pltLabels <- c("insured", "claimsCoverage", "careseeking")

    dfLabels <- tbl_df(data.frame(RV = cleanRV, pltLabs = pltLabels, stringsAsFactors = FALSE))
    return(dfLabels)
}
################################
label_tot_predictors <- function(){
    cleanRV <- c("humidity", "pollution", "popdensity", "housdensity", "child", "adult", "vaxcovI", "vaxcovE", "priorImmunity", "H3A", "B", "adult:H3A", "child:B", "hospaccess", "singlePersonHH", "poverty", "insured", "imscoverage", "careseek")
    pltLabels <- c("humidity", "pollution", "popDensity", "householdSize", "child", "adult", "toddlerVacc", "elderlyVacc", "priorImmunity", "fluH3", "fluB", "adult-fluH3", "child-fluB", "hospAccess", "onePersonHH", "poverty", "insured", "claimsCoverage", "careseeking")

    dfLabels <- tbl_df(data.frame(RV = cleanRV, pltLabs = pltLabels, stringsAsFactors = FALSE))
    return(dfLabels)
}

################################
label_seas_predictors <- function(){
  cleanRV <- paste0("S", 3:9)
  pltLabels <- c("2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09")
  
  dfLabels <- tbl_df(data.frame(RV = cleanRV, pltLabs = pltLabels, stringsAsFactors = FALSE))
  return(dfLabels)
}

################################
label_reg_predictors <- function(){
  cleanRV <- paste0("R", 1:10)
  pltLabels <- c("R1 Boston", "R2 New York", "R3 Philadelphia", "R4 Atlanta", "R5 Chicago", "R6 Dallas", "R7 Kansas City", "R8 Denver", "R9 San Francisco", "R10 Seattle")
  
  dfLabels <- tbl_df(data.frame(RV = cleanRV, pltLabs = pltLabels, stringsAsFactors = FALSE))
  return(dfLabels)
}

################################
label_prec_terms <- function(){
  cleanRV <- c("Precision for ID_nonzero",
    "Precision for fips_nonzero",
    "Precision for graphIdx_nonzero",
    "Precision for fips_st_nonzero",
    "Precision for regionID_nonzero",
    "Precision for season_nonzero")
  pltLabels <- c("observation error", "county iid", "county CAR", "state iid", "region iid", "season iid") 

  dfLabels <- tbl_df(data.frame(RV = cleanRV, pltLabs = pltLabels, stringsAsFactors = FALSE))
  return(dfLabels)
}

################################
label_base_modCodeStr <- function(repCodeLs){
  base_modCodeStr <- repCodeLs[which(nchar(repCodeLs) == min(nchar(repCodeLs)))]
  return(base_modCodeStr)
}

################################
calculate_95CI <- function(summDat){
  # primarily for forest plots
  print(match.call())
  
  returnDat <- summDat %>%
    # mutate(LB = mean-(2*sd), UB = mean+(2*sd)) %>% # 5/15/17 change to exported quantiles
    rename(LB = q_025, UB = q_975) %>%
    mutate(signif = ifelse(UB < 0 | LB > 0, TRUE, FALSE))
  return(returnDat)
}

################################
indicate_signif <- function(summDat){
  # primarily for choropleths indicating significance
  print(match.call())
  
  returnDat <- summDat %>%
    # mutate(LB = mean-(2*sd), UB = mean+(2*sd)) %>% # 5/15/17 change to exported quantiles
    rename(LB = q_025, UB = q_975) %>%
    mutate(signif2 = ifelse(UB < 0, "-1", ifelse(LB > 0, "1", NA)))
  return(returnDat)
}

################################
overlapping_intervals <- function(df, intervalA_LB, intervalA_UB, intervalB_LB, intervalB_UB){
  # primarily for choropleths indicating significance

  df %>%
    mutate_(overlap = interp(~ifelse((aLB <= bLB & bLB <= aUB) | (aLB <= bUB & bUB <= aUB) | (bLB <= aLB & aLB <= bUB) | (bLB <= aUB & aUB <= bUB), "1", "0"), aLB = as.name(intervalA_LB), aUB = as.name(intervalA_UB), bLB = as.name(intervalB_LB), bUB = as.name(intervalB_UB)))
                             
}

# mutate_("overlap" = interp(~ifelse((intervalA_LB <= intervalB_LB), "1", "0")), intervalA_LB=as.name(intervalA_LB), intervalA_UB=as.name(intervalA_UB), intervalB_LB=as.name(intervalB_LB))
#  (intervalA_LB <= intervalB_LB & intervalB_LB <= intervalA_UB) | (intervalA_LB <= intervalB_UB & intervalB_UB <= intervalA_UB) | (intervalB_LB <= intervalA_LB & intervalA_LB <= intervalB_UB) | (intervalB_LB <= intervalA_UB & intervalA_UB <= intervalB_UB)
################################
name_intervals <- function(modLabs){
  intervalA_LB <- paste0(modLabs[1], "_LB"); intervalA_UB <- paste0(modLabs[1], "_UB")
  intervalB_LB <- paste0(modLabs[2], "_LB"); intervalB_UB <- paste0(modLabs[2], "_UB")
  return(list(intervalA_LB=intervalA_LB, intervalA_UB=intervalA_UB, intervalB_LB=intervalB_LB, intervalB_UB=intervalB_UB))
}

################################


#### import functions ################################
aggregate_coefReplicates <- function(repCodeLs){
  print(match.call())

  totLabels <- label_tot_predictors()
  # grab coefData from multiple models
  fullDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), effectType = c(), likelihood = c(), mean = c(), sd = c(), q_025 = c(), q_5 = c(), q_975 = c()))
  
  for(repCode in repCodeLs){
    modDat <- read_csv(string_coef_fname(repCode), col_types = "ccd_cccddddd__")
    fullDf <- bind_rows(fullDf, modDat)
  }

  # collapse data from multiple replicates
  numReps <- length(repCodeLs)
  base_modCodeStr <- label_base_modCodeStr(repCodeLs)
  
  prepDat <- fullDf %>%
    filter(effectType == 'fixed') %>%
    filter(!grepl("intercept", RV)) %>%
    indicate_signif(.) %>%
    clean_RVnames(.) %>%
    mutate(RV = factor(RV, levels = totLabels$RV, labels = totLabels$pltLabs)) %>%
    drop_na(signif2) %>%
    group_by(RV) %>%
    summarise(modCodeStr = base_modCodeStr, mn = mean(mean), dotsize = abs(mean(mean)), dotalpha = length(signif2)/numReps) %>%
    ungroup %>%
    mutate(signif2 = ifelse(mn < 0, "-1", ifelse(mn > 0, "1", NA))) %>%
    select(modCodeStr, RV, signif2, dotsize, dotalpha)

  return(prepDat)
}

################################
import_fit_aggBias_seasIntensityRR <- function(modCodeStr_cty, modCodeStr_st, filepathList){
  print(match.call())
  # import fitted values for county and state seasonal intensity models

  # import county data
  ctyDat <- import_obsFit_seasIntensityRR(modCodeStr_cty, filepathList) %>%
  	rename(fit_rr_cty = fit_rr, fit_logy_cty = fit_logy) %>%
  	select(season, fips, fit_rr_cty, fit_logy_cty) %>%
    mutate(fips_st = substring(fips, 1, 2))
  
  # import state data
  stDat <- import_obsFit_seasIntensityRR_st(modCodeStr_st, filepathList) %>%
    rename(fit_rr_st = fit_rr, fit_logy_st = fit_logy) %>%
    select(season, fips_st, fit_rr_st, fit_logy_st)
  
  fullFitDat <- full_join(ctyDat, stDat, by = c("season", "fips_st")) %>%
  	mutate(fit_rrDiff_stCty = fit_rr_st-fit_rr_cty) %>%
  	mutate(fit_logyRatio_stCty = fit_logy_st-fit_logy_cty) %>%
  	select(season, fips, fips_st, fit_rr_cty, fit_logy_cty, fit_rr_st, fit_logy_st, fit_rrDiff_stCty, fit_logyRatio_stCty)
  
  return(fullFitDat) 
}

################################
import_fitReplicates <- function(repCodeLs){
  print(match.call())

  # grab fitData from multiple models
  fullDf <- tbl_df(data.frame(modCodeStr = c(), season = c(), fips = c(), LB = c(), UB = c()))
  
  for(repCode in repCodeLs){
    modDat <- read_csv(string_fit_fname(repCode), col_types = "c_d_c_dd______")
    fullDf <- bind_rows(fullDf, modDat)
  }

  # collapse data from multiple replicates
  numReps <- length(repCodeLs)
  base_modCodeStr <- label_base_modCodeStr(repCodeLs)
  
  prepDat <- fullDf %>%
    mutate(LB = mean-(1*sd), UB = mean+(1*sd)) %>%
    select(modCodeStr, season, fips, LB, UB)

  return(prepDat)
}
################################
import_obsFitReplicates <- function(repCodeLs){
  print(match.call())

  # grab fitData from multiple models
  fullDf <- tbl_df(data.frame(modCodeStr = c(), season = c(), fips = c(), mean = c(), LB = c(), UB = c(), y = c(), y1 = c()))
  
  for(repCode in repCodeLs){
    modDat <- read_csv(string_fit_fname(repCode), col_types = "c_d_c_dd____dd")
    fullDf <- bind_rows(fullDf, modDat)
  }

  # collapse data from multiple replicates
  numReps <- length(repCodeLs)
  base_modCodeStr <- label_base_modCodeStr(repCodeLs)
  
  prepDat <- fullDf %>%
    mutate(LB = mean-(1*sd), UB = mean+(1*sd)) %>%
    select(modCodeStr, season, fips, mean, LB, UB, y, y1) %>%
    rename(y1_inmodel = y1) %>%
    mutate(y1_orig = log(y+1))

  return(prepDat)
}

################################
import_obsFit_seasIntensityRR <- function(modCodeStr, filepathList){
  print(match.call())
  
  # import fitted data (on the scale of log(y))
  outDat <- read_csv(string_fit_fname(modCodeStr), col_types = "c_d_c_dd______") %>%
    rename(fit_logy = mean, fit_sd = sd) %>%
    select(modCodeStr, season, fips, fit_logy, fit_sd)
  
  # import observed and expected log seasIntensity (shift1)
  if (grepl("2009p", modCodeStr)){
    inDat <- cleanR_iliSum_2009p_shift1_cty(filepathList) %>%
        mutate(obs_logy = log(y1), logE = log(E)) %>%
        mutate(season = 10) %>%
        select(season, fips, obs_logy, logE)
  } else{
    inDat <- cleanR_iliSum_shift1_cty(filepathList) %>%
        mutate(obs_logy = log(y1), logE = log(E)) %>%
        select(season, fips, obs_logy, logE)
  }
  
  # prepare data for plotting breaks
  obsFitDat <- left_join(outDat, inDat, by = c("season", "fips")) %>%
    mutate(obs_rr = obs_logy-logE, fit_rr = fit_logy-logE) %>% # exponentiated 10/6/17
    mutate(resid = (obs_logy - fit_logy)/fit_sd)
  
  return(obsFitDat)
}

################################
import_obsFit_seasIntensityRR_st <- function(modCodeStr, filepathList){
	print(match.call())
	# import observed and fitted data for seasonal intensity state models

	# import fitted data
	outDat <- read_csv(string_fit_fname(modCodeStr), col_types = "c_d_c_dd______") %>%
    	rename(fit_logy = mean, fit_sd = sd) %>%
    	select(modCodeStr, season, fips_st, fit_logy, fit_sd)

    # import observed data
    inDat <- cleanR_iliSum_shift1_st(filepathList) %>%
        mutate(obs_logy = log(y1), logE = log(E)) %>%
        select(season, fips_st, obs_logy, logE)
    
    # prepare data for plotting breaks
  	obsFitDat <- left_join(outDat, inDat, by = c("season", "fips_st")) %>%
    	mutate(obs_rr = obs_logy-logE, fit_rr = fit_logy-logE) %>% # exponentiated 10/6/17
    	mutate(resid = (obs_logy - fit_logy)/fit_sd)

    return(obsFitDat)
}

################################
import_obsFit_excessSeasIntensityRR <- function(modCodeStr, filepathList){
  print(match.call())
  
  # import fitted data (on the scale of log(y))
  outDat <- read_csv(string_fit_fname(modCodeStr), col_types = "c_d_c_dd______") %>%
    rename(fit_logy = mean, fit_sd = sd) %>%
    select(modCodeStr, season, fips, fit_logy, fit_sd)
  
  # import expected log seasIntensity (shift1)
  iliSumDat <- cleanR_iliSum_shift1_cty(filepathList) %>%
    mutate(fit_logE = log(E)) %>%
    select(season, fips, fit_logE)

  # import observed and expected log excessSeasIntensity (shift1)
  excessDat <- cleanR_iliExcessBL_shift1_cty(filepathList) %>%
    mutate(obs_logy = log(y1), obs_logE = log(E)) %>%
    select(season, fips, obs_logy, obs_logE)
  
  # prepare data for plotting breaks
  obsFitDat <- left_join(outDat, iliSumDat, by = c("season", "fips")) %>%
    left_join(excessDat, by = c("season", "fips")) %>%
    mutate(obs_rr = obs_logy-obs_logE, fit_rr = fit_logy-fit_logE)
  
  return(obsFitDat)
}

################################
import_obsFit_epiDuration <- function(modCodeStr, filepathList){
  print(match.call())
  
  # import fitted epiDur data 
  outDat <- read_csv(string_fit_fname(modCodeStr), col_types = "c_d_c_dd______") %>%
    rename(fit_y = mean, fit_sd = sd) %>%
    select(modCodeStr, season, fips, fit_y, fit_sd)
  
  # import observed epidemic duration
  inDat <- cleanR_epiDur_cty(filepathList) %>%
    rename(obs_y = y1) %>%
    select(season, fips, obs_y)
  
  # prepare data for plotting breaks
  obsFitDat <- left_join(outDat, inDat, by = c("season", "fips")) %>%
    mutate(resid = (obs_y - fit_y)/fit_sd)
  
  return(obsFitDat)
}

################################
import_county_geomMap <- function(){
  print(match.call())
  
  countyMap <- map_data("county")
  data(county.fips)
  polynameSplit <- tstrsplit(county.fips$polyname, ",")
  county_geomMap <- tbl_df(county.fips) %>%
    mutate(fips = substr.Right(paste0("0", fips), 5)) %>%
    mutate(region = polynameSplit[[1]]) %>%
    mutate(subregion = polynameSplit[[2]]) %>%
    full_join(countyMap, by = c("region", "subregion")) %>%
    filter(!is.na(polyname) & !is.na(long)) %>%
    rename(state = region, county = subregion) %>%
    rename(region = fips) %>%
    select(-polyname)
  
  return(county_geomMap)
}

################################
import_county_oneState_geomMap <- function(stateName){
    print(match.call())

    countyMap <- map_data("county", regions = stateName)
    data(county.fips)
    polynameSplit <- tstrsplit(county.fips$polyname, ",")
    county_geomMap <- tbl_df(county.fips) %>%
        mutate(fips = substr.Right(paste0("0", fips), 5)) %>%
        mutate(region = polynameSplit[[1]]) %>%
        mutate(subregion = polynameSplit[[2]]) %>%
        full_join(countyMap, by = c("region", "subregion")) %>%
        filter(!is.na(polyname) & !is.na(long)) %>%
        rename(state = region, county = subregion) %>%
        rename(region = fips) %>%
        select(-polyname)
  
  return(county_geomMap)
}

################################
import_regionValidationILI <- function(modCodeStr, iFormats){
  print(match.call())
  
  # import cw for fips-region
  cw <- read_csv(string_ids_fname(modCodeStr), col_types = cols_only(fips = "c", regionID = "d")) %>%
    distinct(fips, regionID) %>%
    rename(reg = regionID)
  
  iliDat <- import_cdcRegILI() 
  fitDat <- read_csv(string_fit_fname(modCodeStr), col_types = "c_d_c_ddd_d___") %>%
    rename(LB = q_025, UB = q_975) #5/15/17
  
  fullDat <- left_join(fitDat, cw, by = "fips") %>%
    left_join(iliDat, by = c("reg", "season")) 
  
  if(iFormats$age == "total"){
    returnDat <- fullDat %>%
      select(season, fips, reg, mean, LB, UB, tot_unwt_pct) %>%
      rename(ili_unwt_pct = tot_unwt_pct) %>%
      group_by(season, reg) %>%
      summarise(mean = mean(mean, na.rm = TRUE), ili_unwt_pct = first(ili_unwt_pct)) %>%
      ungroup
  } else if(iFormats$age == "child"){
    returnDat <- fullDat %>%
      select(season, fips, reg, mean, LB, UB, ch_unwt_totPct) %>%
      rename(ili_unwt_pct = ch_unwt_totPct) %>%
      group_by(season, reg) %>%
      summarise(mean = mean(mean, na.rm = TRUE), ili_unwt_pct = first(ili_unwt_pct)) %>%
      ungroup
  } else if(iFormats$age == "adult"){
    returnDat <- fullDat %>%
      select(season, fips, reg, mean, LB, UB, ad_unwt_totPct) %>%
      rename(ili_unwt_pct = ad_unwt_totPct) %>%
      group_by(season, reg) %>%
      summarise(mean = mean(mean, na.rm = TRUE), ili_unwt_pct = first(ili_unwt_pct)) %>%
      ungroup
  }
  
  return(returnDat)
}

################################
import_regionValidationViral <- function(modCodeStr, iFormats){
  print(match.call())
  
  # import cw for fips-region
  cw <- read_csv(string_ids_fname(modCodeStr), col_types = cols_only(fips = "c", regionID = "d")) %>%
    distinct(fips, regionID) %>%
    rename(reg = regionID)
  
  vDat <- import_cdcRegViral()
  fitDat <- read_csv(string_fit_fname(modCodeStr), col_types = "c_d_c_ddd_d___") %>%
    rename(LB = q_025, UB = q_975)
  
  fullDat <- left_join(fitDat, cw, by = "fips") %>%
    left_join(vDat, by = c("reg", "season")) 
  
  returnDat <- fullDat %>%
      select(season, fips, reg, mean, LB, UB, perc_pos) %>%
      group_by(season, reg) %>%
      summarise(mean = mean(mean, na.rm = TRUE), perc_pos = first(perc_pos)) %>%
      ungroup
 
  return(returnDat)
}

#### plot functions ################################
choro_obsFit_seasIntensityRR_oneSeason <- function(modCodeStr, pltFormats, filepathList){
  # plot side-by-side choropleths for the observed and fitted relative risk of seasonal intensity 
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  popCode <- pltFormats$popCode
  if (is.null(pltFormats$legendStep)){
    legendStep <- 1
  } else{
    legendStep <- pltFormats$legendStep
  }
  
  # import and clean observed and fitted seasonal intensity RR 
  prepDat <- import_obsFit_seasIntensityRR(modCodeStr, filepathList)
  
  # set breaks based on distribution of observed data
  breaks <- seq(floor(min(prepDat$obs_rr, na.rm = TRUE)), ceiling(max(prepDat$obs_rr, na.rm = TRUE)), by = legendStep)
  prepDat2 <- prepDat %>%
    # mutate(observed = as.character(findInterval(obs_rr, breaks, rightmost.closed = TRUE))) %>%
    # mutate(fitted = as.character(findInterval(fit_rr, breaks, rightmost.closed = TRUE)))
    mutate(Observed = cut(obs_rr, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) %>%
    mutate(Fitted = cut(fit_rr, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE))
  factorlvls <- levels(prepDat2$Observed)
  
  plotDat <- prepDat2 %>%
    select(season, fips, Observed, Fitted) %>%
    gather(fig, bin, Observed:Fitted) %>%
    mutate(fig = factor(fig, levels = c("Observed", "Fitted"))) %>%
    mutate(bin = factor(bin, levels = factorlvls, labels = factorlvls, ordered = TRUE))
  print(levels(plotDat$bin))
 
  seasLs <- plotDat %>% distinct(season) %>% unlist
  for (s in seasLs){
   
    exportFname <- paste0(string_msFig_folder(), "choro_obsFit_seasIntensityRR", popCode, "_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    # import county mapping info
    ctyMap <- import_county_geomMap()
    
    # plot
    choro <- ggplot() +
      geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
      geom_map(data = pltDat, map = ctyMap, aes(fill = bin, map_id = fips), color = "grey25", size = 0.025) +
      scale_fill_brewer(name = "Log Relative Risk", palette = "OrRd", na.value = "grey60", drop = FALSE) +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 15), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
      facet_wrap(~fig, nrow=1)
    
    ggsave(exportFname, choro, height = h, width = w, dpi = dp)
     
  }
  
}

################################
choro_fit_seasIntensityRR_oneSeason <- function(modCodeStr, pltFormats, filepathList){
  # plot single choropleth for the fitted relative risk of seasonal intensity by season
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  popCode <- pltFormats$popCode
  if (is.null(pltFormats$legendStep)){
    legendStep <- 1
  } else{
    legendStep <- pltFormats$legendStep
  }

  # import and clean observed and fitted seasonal intensity RR 
  prepDat <- import_obsFit_seasIntensityRR(modCodeStr, filepathList)
  
  # set breaks based on distribution of observed data
  breaks <- seq(floor(min(prepDat$obs_rr, na.rm = TRUE)), ceiling(max(prepDat$obs_rr, na.rm = TRUE)), by = legendStep)
  prepDat2 <- prepDat %>%
    # mutate(observed = as.character(findInterval(obs_rr, breaks, rightmost.closed = TRUE))) %>%
    # mutate(fitted = as.character(findInterval(fit_rr, breaks, rightmost.closed = TRUE)))
    mutate(Observed = cut(obs_rr, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) %>%
    mutate(Fitted = cut(fit_rr, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE))
  factorlvls <- levels(prepDat2$Observed)
  
  plotDat <- prepDat2 %>%
    select(season, fips, Observed, Fitted) %>%
    gather(fig, bin, Observed:Fitted) %>%
    filter(fig == "Fitted") %>%
    mutate(fig = factor(fig, levels = c("Fitted"))) %>%
    mutate(bin = factor(bin, levels = factorlvls, labels = factorlvls, ordered = TRUE)) 
  print(levels(plotDat$bin))
 
  seasLs <- plotDat %>% distinct(season) %>% unlist
  for (s in seasLs){
   
    exportFname <- paste0(string_msFig_folder(), "choro_fit_seasIntensityRR", popCode, "_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    # import county mapping info
    ctyMap <- import_county_geomMap()
    
    # plot
    choro <- ggplot() +
      geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
      geom_map(data = pltDat, map = ctyMap, aes(fill = bin, map_id = fips), color = "grey25", size = 0.025) +
      scale_fill_brewer(name = "Log Relative\nRisk", palette = "OrRd", na.value = "grey60", drop = FALSE) +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 10), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
    
    ggsave(exportFname, choro, height = h, width = w, dpi = dp)
     
  }
  
}
################################
choroSt_fit_seasIntensityRR_oneSeason <- function(modCodeStr, pltFormats, filepathList){
  # plot single choropleth for the fitted relative risk of seasonal intensity by season
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  popCode <- pltFormats$popCode
  if (is.null(pltFormats$legendStep)){
    legendStep <- 0.4
  } else{
    legendStep <- pltFormats$legendStep
  }

  # import and clean observed and fitted seasonal intensity RR 
  prepDat <- import_obsFit_seasIntensityRR_st(modCodeStr, filepathList)

  # set breaks based on distribution of observed data
  breaks <- seq(floor(min(prepDat$fit_rr, na.rm = TRUE)), ceiling(max(prepDat$fit_rr, na.rm = TRUE)), by = legendStep)
  prepDat2 <- prepDat %>%
    # mutate(observed = as.character(findInterval(obs_rr, breaks, rightmost.closed = TRUE))) %>%
    # mutate(fitted = as.character(findInterval(fit_rr, breaks, rightmost.closed = TRUE)))
    mutate(Observed = cut(obs_rr, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) %>%
    mutate(Fitted = cut(fit_rr, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE))
  factorlvls <- levels(prepDat2$Observed)
  
  plotDat <- prepDat2 %>%
    select(season, fips_st, Observed, Fitted) %>%
    gather(fig, bin, Observed:Fitted) %>%
    filter(fig == "Fitted") %>%
    mutate(fig = factor(fig, levels = c("Fitted"))) %>%
    mutate(bin = factor(bin, levels = factorlvls, labels = factorlvls, ordered = TRUE)) %>%
    rename(fips = fips_st) %>%
    left_join(import_stAbbr(), by = "fips")
  print(levels(plotDat$bin))
 
  seasLs <- plotDat %>% distinct(season) %>% unlist
  for (s in seasLs){
   
    exportFname <- paste0(string_msFig_folder(), "choroSt_fit_seasIntensityRR", popCode, "_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    # import county mapping info
    stMap <- map_data("state")
    
    # plot
    choro <- ggplot(pltDat, aes(map_id = State)) +
      geom_map(map = stMap, aes(fill = bin, map_id = State), color = "grey25", size = 0.025) +
      scale_fill_brewer(name = "Log Relative Risk", palette = "OrRd", na.value = "grey60", drop = FALSE) +
      expand_limits(x = stMap$long, y = stMap$lat) +
      guides(fill = guide_legend(nrow = 2)) +
      theme_minimal() +
      theme(text = element_text(size = 10), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
    
    ggsave(exportFname, choro, height = h, width = w, dpi = dp)
     
  }
  
}

################################
choroSt_obsFit_seasIntensityRR_multiSeason <- function(modCodeStr, pltFormats, filepathList){
  # plot side-by-side choropleths for the observed and fitted relative risk of seasonal intensity - state-level (multiple years)
  print(match.call())

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasDf <- label_seas_predictors()
  popCode <- pltFormats$popCode
  if (is.null(pltFormats$legendStep)){
    legendStep <- 0.4
  } else{
    legendStep <- pltFormats$legendStep
  }
  
  # import and clean observed and fitted seasonal intensity RR 
  prepDat <- import_obsFit_seasIntensityRR_st(modCodeStr, filepathList)
  
  # set breaks based on distribution of observed data
  if (is.null(pltFormats$manualBreaks)){
      breaks <- seq(floor(min(prepDat$obs_rr, na.rm = TRUE)), ceiling(max(prepDat$obs_rr, na.rm = TRUE)), by = legendStep)
    } else{
      breaks <- pltFormats$manualBreaks
    }
  
  prepDat2 <- prepDat %>%
    # mutate(observed = as.character(findInterval(obs_rr, breaks, rightmost.closed = TRUE))) %>%
    # mutate(fitted = as.character(findInterval(fit_rr, breaks, rightmost.closed = TRUE)))
    mutate(Observed = cut(obs_rr, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) %>%
    mutate(Fitted = cut(fit_rr, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE))
  factorlvls <- levels(prepDat2$Observed)
   
  prepDat3 <- prepDat2 %>%
    select(season, fips_st, Observed, Fitted) %>%
    gather(fig, bin, Observed:Fitted) %>%
    mutate(fig = factor(fig, levels = c("Observed", "Fitted"))) %>%
    mutate(bin = factor(bin, levels = factorlvls, labels = factorlvls, ordered = TRUE)) %>%
    mutate(season = paste0("S", season)) %>%
    mutate(season = factor(season, levels = seasDf$RV, labels = seasDf$pltLabs)) %>%
    rename(fips = fips_st) %>%
    left_join(import_stAbbr(), by = "fips")
  
  # control flow to remove a single season if necessary
  if(is.null(pltFormats$rmSeas)){
    plotDat <- prepDat3
  } else{
    plotDat <- prepDat3 %>%
      filter(season != pltFormats$rmSeas)
  }
  
  print(levels(plotDat$bin))

  exportFname <- paste0(string_msFig_folder(), "choroSt_obsFit_seasIntensityRR_multiSeason", popCode, ".png")
  
  # import county mapping info
  stMap <- map_data("state")
  
  # plot
  choro <- ggplot(plotDat, aes(map_id = State)) +
    geom_map(map = stMap, aes(fill = bin, map_id = State), color = "grey25", size = 0.025) +
    scale_fill_brewer(name = "Log Relative Risk", palette = "OrRd", na.value = "grey60", drop = FALSE) +
    expand_limits(x = stMap$long, y = stMap$lat) +
    theme_minimal() +
    theme(text = element_text(size = 12), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom", legend.margin = margin(), legend.box.margin = margin()) +
    facet_grid(season~fig)
  
  ggsave(exportFname, choro, height = h, width = w, dpi = dp)


}

################################
choro_obsFit_seasIntensityRR_multiSeason <- function(modCodeStr, pltFormats, filepathList){
  # plot side-by-side choropleths for the observed and fitted relative risk of seasonal intensity (multiple years)
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasDf <- label_seas_predictors()
  popCode <- pltFormats$popCode
  if (is.null(pltFormats$legendStep)){
    legendStep <- 1
  } else{
    legendStep <- pltFormats$legendStep
  }
  
  # import and clean observed and fitted seasonal intensity RR 
  prepDat <- import_obsFit_seasIntensityRR(modCodeStr, filepathList)
  
  # set breaks based on distribution of observed data
  breaks <- seq(floor(min(prepDat$obs_rr, na.rm = TRUE)), ceiling(max(prepDat$obs_rr, na.rm = TRUE)), by = legendStep)
  prepDat2 <- prepDat %>%
    # mutate(observed = as.character(findInterval(obs_rr, breaks, rightmost.closed = TRUE))) %>%
    # mutate(fitted = as.character(findInterval(fit_rr, breaks, rightmost.closed = TRUE)))
    mutate(Observed = cut(obs_rr, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) %>%
    mutate(Fitted = cut(fit_rr, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE))
  factorlvls <- levels(prepDat2$Observed)
   
  prepDat3 <- prepDat2 %>%
    select(season, fips, Observed, Fitted) %>%
    gather(fig, bin, Observed:Fitted) %>%
    mutate(fig = factor(fig, levels = c("Observed", "Fitted"))) %>%
    mutate(bin = factor(bin, levels = factorlvls, labels = factorlvls, ordered = TRUE)) %>%
    mutate(season = paste0("S", season)) %>%
    mutate(season = factor(season, levels = seasDf$RV, labels = seasDf$pltLabs)) 
  
  # control flow to remove a single season if necessary
  if(is.null(pltFormats$rmSeas)){
    plotDat <- prepDat3
  } else{
    plotDat <- prepDat3 %>%
      filter(season != pltFormats$rmSeas)
  }
  
  print(levels(plotDat$bin))
  
  exportFname <- paste0(string_msFig_folder(), "choro_obsFit_seasIntensityRR_multiSeason", popCode, ".png")
  
  # import county mapping info
  ctyMap <- import_county_geomMap()
  
  # plot
  choro <- ggplot() +
    geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
    geom_map(data = plotDat, map = ctyMap, aes(fill = bin, map_id = fips), color = "grey25", size = 0.025) +
    scale_fill_brewer(name = "Log Relative\nRisk", palette = "OrRd", na.value = "grey60", drop = FALSE) +
    expand_limits(x = ctyMap$long, y = ctyMap$lat) +
    theme_minimal() +
    theme(text = element_text(size = 14), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom", legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
    facet_grid(season~fig)
  
  ggsave(exportFname, choro, height = h, width = w, dpi = dp)
  
}

################################
choro_fit_aggBias_seasIntensityRR_oneSeason <- function(modCodeStr_cty, modCodeStr_st, pltFormats, filepathList){
  print(match.call())
  # choropleth of error between county and state-level seasonal intensity

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  if (is.null(pltFormats$legendStep)){
    legendStep <- 1
  } else{
    legendStep <- pltFormats$legendStep
  }

  # import error between county and state seasonal intensity RR
  prepDat <- import_fit_aggBias_seasIntensityRR(modCodeStr_cty, modCodeStr_st, filepathList)

  # set breaks based on distribution of log ratio of relative risks from county to state data (aka. error)
  # breaks <- seq(floor(min(prepDat$fit_rrDiff_ctySt, na.rm = TRUE)), ceiling(max(prepDat$fit_rrDiff_ctySt, na.rm = TRUE)), by = legendStep)
  breaks <- seq(-2.5,3.5,by=1)
  manualPalette <- c("#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41", "#09622a")

  plotDat <- prepDat %>%
    mutate(Fit_rrDiff = cut(fit_rrDiff_stCty, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
  factorlvls <- levels(plotDat$fit_rrDiff_stCty)

  seasLs <- plotDat %>% distinct(season) %>% unlist
  for (s in seasLs){
   
    exportFname <- paste0(string_msFig_folder(), "choro_fit_aggBias_seasIntensityRR_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)

    # import county mapping info
    ctyMap <- import_county_geomMap()
    
    # plot
    choro <- ggplot() +
      geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
      geom_map(data = pltDat, map = ctyMap, aes(fill = Fit_rrDiff, map_id = fips), color = "grey25", size = 0.025) +
      scale_fill_manual(name = "Error", values = manualPalette, na.value = "grey60", drop = FALSE) +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 10), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
    
    ggsave(exportFname, choro, height = h, width = w, dpi = dp)
     
  }

}

################################
choro_fit_aggBias_seasIntensityRR_multiSeason <- function(modCodeStr_cty, modCodeStr_st, pltFormats, filepathList){
  print(match.call())
  # choropleth of error between county and state-level seasonal intensity

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasDf <- label_seas_predictors()
  if (is.null(pltFormats$legendStep)){
    legendStep <- 1
  } else{
    legendStep <- pltFormats$legendStep
  }

  # import error between county and state seasonal intensity RR
  prepDat <- import_fit_aggBias_seasIntensityRR(modCodeStr_cty, modCodeStr_st, filepathList)

  # set breaks based on distribution of log ratio of relative risks from county to state data (aka. error)
  # breaks <- seq(floor(min(prepDat$fit_rrDiff_ctySt, na.rm = TRUE)), ceiling(max(prepDat$fit_rrDiff_ctySt, na.rm = TRUE)), by = legendStep)
  breaks <- seq(-2.5,3.5,by=1)
  manualPalette <- c("#1c73b1", "#67add4", "#cacaca", "#69a761", "#2f8e41", "#09622a")
  plotDat <- prepDat %>%
    mutate(Fit_rrDiff = cut(fit_rrDiff_stCty, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) 
  factorlvls <- levels(plotDat$fit_rrDiff_stCty)

  pltDat <- plotDat %>% 
    mutate(season = paste0("S", season)) %>%
    mutate(season = factor(season, levels = seasDf$RV, labels = seasDf$pltLabs))
   
  exportFname <- paste0(string_msFig_folder(), "choro_fit_aggBias_seasIntensityRR_multiSeason.png")
  
  # import county mapping info
  ctyMap <- import_county_geomMap()
  
  # plot
  choro <- ggplot() +
    geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
    geom_map(data = pltDat, map = ctyMap, aes(fill = Fit_rrDiff, map_id = fips), color = "grey25", size = 0.025) +
    scale_fill_manual(name = "Error (State-County)", values = manualPalette, na.value = "grey60", drop = FALSE) +
    expand_limits(x = ctyMap$long, y = ctyMap$lat) +
    theme_minimal() +
    theme(text = element_text(size = 10), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
    facet_wrap(~season, nrow = 2)
  
  ggsave(exportFname, choro, height = h, width = w, dpi = dp)

}
################################
choro_stCty_fit_seasIntensityRR_oneSeason <- function(modCodeStr_cty, modCodeStr_st, pltFormats, filepathList){
    print(match.call())
    # side-by-side choropleths of state and county fitted values for a single state (seasonal intensity RR)

    # plot formatting
    w <- pltFormats$w; h <- pltFormats$h; dp <- 300
    stName <- pltFormats$stName
    stFips <- import_stAbbr()[which(import_stAbbr()$State==stName),]$fips
    if (is.null(pltFormats$legendStep)){
        legendStep <- 0.3
    } else{
        legendStep <- pltFormats$legendStep
    }

    # import county and state seasonal intensity RR
    prepDat <- import_fit_aggBias_seasIntensityRR(modCodeStr_cty, modCodeStr_st, filepathList) %>%
        left_join(import_stAbbr(), by = c("fips_st"="fips")) %>%
        filter(State == stName)

    # import county and state map information
    ctyMap <- import_county_oneState_geomMap(stName)
    stMap <- map_data("state", regions = stName)

    seasLs <- prepDat %>% distinct(season) %>% unlist
    for (s in seasLs){

        exportFname <- paste0(string_msFig_folder(), "choro_stCty_fit_seasIntensityRR_st", stFips, "_S", s, ".png")
        seasDat <- prepDat %>% filter(season == s)

        # set breaks based on distribution of county and state RR
        allValues <- c(seasDat$fit_rr_cty, seasDat$fit_rr_st)
        breaks <- seq(floor(min(allValues, na.rm = TRUE)), ceiling(max(allValues, na.rm = TRUE)), by = legendStep)
        prepDat2 <- seasDat %>%
            mutate(rrCty = cut(fit_rr_cty, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) %>%
            mutate(rrSt = cut(fit_rr_st, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE))

        factorlvls <- levels(prepDat2$rrCty)

        pltDat <- prepDat2 %>%
            select(season, fips, fips_st, State, rrCty, rrSt) %>%
            gather(fig, bin, rrCty:rrSt) %>%
            mutate(fig = factor(fig, levels = c("rrSt", "rrCty"), labels = c("State", "County"))) %>%
            mutate(bin = factor(bin, levels = factorlvls, labels = factorlvls, ordered = TRUE)) #%>%
            # mutate(bin = droplevels(bin))
        print(breaks)
        print(range(allValues))
        print(levels(pltDat$bin))
        View(pltDat)
        # plot
        choro <- ggplot() +
          geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
          geom_map(data = pltDat, map = ctyMap, aes(fill = bin, map_id = fips), color = "grey25", size = 0.025) +
          scale_fill_brewer(name = "Relative Risk", palette = "OrRd", na.value = "grey60", drop = FALSE) +
          expand_limits(x = ctyMap$long, y = ctyMap$lat) +
          theme_minimal() +
          theme(text = element_text(size = 10), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom",legend.margin = margin(), legend.box.margin = margin()) +
          guides(fill = guide_legend(nrow = 2)) +
          facet_wrap(~fig, nrow=1)
        
        ggsave(exportFname, choro, height = h, width = w, dpi = dp)

    }

}

################################
scatter_corr_fitVariance_aggBias <- function(modCodeStr_cty, modCodeStr_st, pltFormats, filepathList){
    print(match.call())
    # state-level scatterplot of variance of fitted values and sum of absolute value in aggregation bias measure
    # Is there more heterogeneity within states that have greater under or overestimation?

    # plot formatting
    w <- pltFormats$w; h <- pltFormats$h; dp <- 300
    seasDf <- label_seas_predictors()

    # import error between county and state seasonal intensity RR
    importDat <- import_fit_aggBias_seasIntensityRR(modCodeStr_cty, modCodeStr_st, filepathList)

    plotDat <- importDat %>%
        group_by(fips_st, season) %>%
        summarise(fitVar = var(fit_rr_cty), magAggBias = sum(abs(fit_rrDiff_stCty), na.rm=TRUE)) %>%
        ungroup %>% 
        mutate(season = paste0("S", season)) %>%
        mutate(season = factor(season, levels = seasDf$RV, labels = seasDf$pltLabs))

    # print correlation 
    print(cor(plotDat$fitVar, plotDat$magAggBias, use = "complete.obs", method = "pearson")) # spearman 0.535, peason 0.429
    print(cor.test(plotDat$fitVar, plotDat$magAggBias, method = "spearman", alternative = "two.sided"))

    exportFname <- paste0(string_msFig_folder(), "scatter_fitVariance_aggBias.png")

    # plot
    plotOutput <- ggplot(plotDat, aes(x = magAggBias, y = fitVar)) +
        geom_point(alpha = 0.7) + 
        scale_x_continuous("Absolute Magnitude of Error") +
        scale_y_continuous("Variance of Fitted Value Means") +
        theme_bw() + 
        theme(text = element_text(size = 13), legend.margin = margin(), legend.position = "bottom") +
        facet_wrap(~season, nrow = 2)
  
    ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)  
    return(plotDat)
}

################################
forest_coefDistr_stateStructuredEffects <- function(modCodeStr){
  print(match.call())
  
  # plot formatting
  stLabels <- read_csv(string_ids_fname(modCodeStr), col_types = "____c__c") %>% distinct(graphIdx_st, st)
  exportFname <- paste0(string_msFig_folder(), "forest_coefStateStructured_", modCodeStr, ".png")
  plotFormats <- list(w=6, h=2)

  # import state structured coef data
  importDat <- read_csv(string_coef_fname(modCodeStr), col_types = "ccd_cccddddd__") 
  coefDat <- importDat %>%
    filter(effectType == 'structured_st') %>%
    clean_RVnames(.) %>%
    mutate(RV = factor(RV, levels = stLabels$graphIdx_st, labels = stLabels$st))
  
  # prepare data for plotting
  plotDat <- calculate_95CI(coefDat) 
  
  # plot 
  plot_coefDistr_RV(plotDat, exportFname, plotFormats)
}

################################
forest_coefDistr_stateEffects <- function(modCodeStr){
  print(match.call())
  
  # plot formatting
  stLabels <- read_csv(string_ids_fname(modCodeStr), col_types = "_c__c___") %>%
    mutate(fips_st = substring(fips, 1, 2)) %>% 
    distinct(fips_st, st)
  exportFname <- paste0(string_msFig_folder(), "forest_coefState_", modCodeStr, ".png")
  plotFormats <- list(w=6, h=2)

  # import state coef data
  importDat <- read_csv(string_coef_fname(modCodeStr), col_types = "ccd_cccddddd__") 
  coefDat <- importDat %>%
    filter(effectType == 'stID') %>%
    clean_RVnames(.) %>%
    mutate(RV = factor(RV, levels = stLabels$fips_st, labels = stLabels$st))
  
  # prepare data for plotting
  plotDat <- calculate_95CI(coefDat) 
  
  # plot 
  plot_coefDistr_RV(plotDat, exportFname, plotFormats)
}

################################
forest_coefDistr_regionEffects <- function(modCodeStr){
  print(match.call())
  
  # plot formatting
  regLabels <- label_reg_predictors()
  exportFname <- paste0(string_msFig_folder(), "forest_coefReg_", modCodeStr, ".png")
  plotFormats <- list(w=6, h=2.5)

  # import region coef data
  importDat <- read_csv(string_coef_fname(modCodeStr), col_types = "ccd_cccddddd__") 

  coefDat <- importDat %>%
    filter(effectType == 'regID') %>%
    mutate(RV = factor(RV, levels = regLabels$RV, labels = regLabels$pltLabs))

  # prepare data for plotting
  plotDat <- calculate_95CI(coefDat) 
  
  # plot 
  plot_coefDistr_RV(plotDat, exportFname, plotFormats)
}

################################
forest_coefDistr_ctyEffects_sample <- function(modCodeStr){
  print(match.call())
  
  set.seed(45761)

  # plot formatting
  exportFname <- paste0(string_msFig_folder(), "forest_coefCty_sample_", modCodeStr, ".png")
  plotFormats <- list(w=4, h=2)
  
  # import season coef data
  importDat <- read_csv(string_coef_fname(modCodeStr), col_types = "ccd_cccddddd__") 
  View(importDat %>% filter(effectType == "spatial"))
  coefDat <- importDat %>%
    filter(effectType == 'spatial') %>%
    sample_n(50) %>%
    clean_RVnames(.) %>%
    mutate(RV = as.factor(RV))
  
  # prepare data for plotting
  plotDat <- calculate_95CI(coefDat) 
  
  # plot 
  plot_coefDistr_RV(plotDat, exportFname, plotFormats)
}

################################
forest_coefDistr_errorEffects_sample <- function(modCodeStr){
  print(match.call())
  
  set.seed(329)

  # plot formatting
  exportFname <- paste0(string_msFig_folder(), "forest_coefErr_sample_", modCodeStr, ".png")
  plotFormats <- list(w=4, h=2)
  
  # import season coef data
  importDat <- read_csv(string_coef_fname(modCodeStr), col_types = "ccd_cccddddd__") 
  coefDat <- importDat %>%
    filter(effectType == 'error') %>%
    sample_n(50) %>%
    clean_RVnames(.) %>%
    mutate(RV = as.factor(RV))
  
  # prepare data for plotting
  plotDat <- calculate_95CI(coefDat) 
  
  # plot 
  plot_coefDistr_RV(plotDat, exportFname, plotFormats)
}

################################
forest_coefDistr_precisionTerms <- function(modCodeStr){
  print(match.call())
  
  # plot formatting
  precLabels <- label_prec_terms()
  exportFname <- paste0(string_msFig_folder(), "forest_coefPrecision_", modCodeStr, ".png")
  plotFormats <- list(w=4, h=2)

  # import state structured coef data
  importDat <- read_csv(string_coef_fname(modCodeStr), col_types = "ccd_cccddddd__") 
  coefDat <- importDat %>%
    filter(effectType == 'hyperpar') %>%
    mutate(RV = factor(RV, levels = precLabels$RV, labels = precLabels$pltLabs)) %>%
    filter(!is.na(RV))
  
  # prepare data for plotting
  plotDat <- calculate_95CI(coefDat) 
  
  # plot 
  plot_coefDistr_RV(plotDat, exportFname, plotFormats)
}

################################
scatter_obsFit_outOfSampleReplicates <- function(baseCodeLs, pltFormats){
  print(match.call())
    
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  modCodeStr <- substring(baseCodeLs[1], 1, nchar(baseCodeLs[1])-4) 

  plotLabels <- data.frame(baseCodeStr = baseCodeLs, pltLabel = pltFormats$labs)
  exportFname <- paste0(string_msFig_folder(), "scatter_obsFit_outOfSampleReplicates_", pltFormats$descrip, "_", modCodeStr, ".png")
  
  # data formatting
  numReplicates <- pltFormats$numReplicates

  ## grab fitData from multiple models ##
  fullDf <- tbl_df(data.frame(modCodeStr = c(), season = c(), fips = c(), mean = c(), LB = c(), UB = c(), y = c(), y1_inmodel = c(), y1_orig = c()))

  # import replicates for each baseCode
  for (baseRepCode in baseCodeLs){
    repCodeLs <- c(paste0(baseRepCode, ""), paste(baseRepCode, 1:(numReplicates-1), sep = "-"))
    modDat <- import_obsFitReplicates(repCodeLs) 
    fullDf <- bind_rows(fullDf, modDat)
    print(paste(baseRepCode, "imported"))
  }

  plotDat <- fullDf %>%
    filter(is.na(y1_inmodel)) %>%
    dplyr::mutate(baseCodeStr = substring(modCodeStr, 1, min(nchar(fullDf$modCodeStr)))) %>%
    left_join(plotLabels, by = c("baseCodeStr")) %>%
    mutate(season = factor(season, levels = 3:9, labels = c("2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09")))

  # plot
  plotOutput <- ggplot(plotDat, aes(x = y1_orig)) +
    geom_pointrange(aes(ymin = LB, y = mean, ymax = UB, colour = season), alpha = 0.5) + 
    geom_abline(aes(intercept = 0, slope = 1), colour = "grey50") +
    scale_x_continuous("Observed") +
    scale_y_continuous("Fitted") +
    scale_colour_tableau() +
    theme_bw() + 
    theme(text = element_text(size = 13), legend.margin = margin(), legend.position = "bottom") +
    facet_wrap(~pltLabel, nrow = 2)
  
  ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)

  return(plotDat)
  
}

################################
scatter_obsFit_seasIntensityRR_multiSeason <- function(modCodeStr, pltFormats, filepathList){
  # scatterplot of observed vs fitted seasonal intensity RR values, by season
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasDf <- label_seas_predictors()
  
  # import and clean observed and fitted seasonal intensity RR 
  plotDat <- import_obsFit_seasIntensityRR(modCodeStr, filepathList) %>%
    rename(Observed = obs_rr, Fitted = fit_rr) %>%
    mutate(season = paste0("S", season)) %>%
    mutate(season = factor(season, levels = seasDf$RV, labels = seasDf$pltLabs)) 

  exportFname <- paste0(string_msFig_folder(), "scatter_obsFit_seasIntensityRR_multiSeason", ".png")
  
  # plot
  plotOutput <- ggplot(plotDat, aes(x = Observed, y = Fitted)) +
    geom_point(alpha = 0.7) + 
    geom_abline(yintercept = 0, slope = 1, colour = "grey50") +
    scale_x_continuous(limits = c(-3,2)) +
    scale_y_continuous(limits = c(-3,2)) +
    theme_bw() + 
    theme(text = element_text(size = 13), legend.margin = margin(), legend.position = "bottom") +
    facet_wrap(~season, nrow = 2)
  
  ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)
}
################################

scatter_obsFit_excessSeasIntensityRR_multiSeason <- function(modCodeStr, pltFormats, filepathList){
  # scatterplot of observed excess seasonal intensity vs fitted seasonal intensity RR values, by season -- validation checks
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasDf <- label_seas_predictors()
  
  # import and clean observed and fitted seasonal intensity RR 
  plotDat <- import_obsFit_excessSeasIntensityRR(modCodeStr, filepathList) %>%
    rename(Observed = obs_rr, Fitted = fit_rr) %>%
    mutate(season = paste0("S", season)) %>%
    mutate(season = factor(season, levels = seasDf$RV, labels = seasDf$pltLabs)) 

  exportFname <- paste0(string_msFig_folder(), "scatter_obsFit_excessSeasIntensityRR_multiSeason", ".png")
  
  # plot
  plotOutput <- ggplot(plotDat, aes(x = Observed, y = Fitted)) +
    geom_point(alpha = 0.7) + 
    geom_abline(yintercept = 0, slope = 1, colour = "grey50") +
    scale_x_continuous("Observed Excess Seasonal Intensity RR", limits = c(-3,3)) +
    scale_y_continuous("Fitted Seasonal Intensity RR", limits = c(-3,3)) +
    theme_bw() + 
    theme(text = element_text(size = 13), legend.margin = margin(), legend.position = "bottom") +
    facet_wrap(~season, nrow = 2)
  
  ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)
}
################################

scatter_residFit_logSeasIntensity_multiSeason <- function(modCodeStr, pltFormats, filepathList){
  # scatterplot of residuals vs fitted log seasonal intensity, by season
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasDf <- label_seas_predictors()
  
  # import and clean observed and fitted seasonal intensity RR 
  plotDat <- import_obsFit_seasIntensityRR(modCodeStr, filepathList) %>%
    rename(Residuals = resid, Fitted = fit_logy) %>%
    mutate(season = paste0("S", season)) %>%
    mutate(season = factor(season, levels = seasDf$RV, labels = seasDf$pltLabs)) 
  print(summary(plotDat))

  exportFname <- paste0(string_msFig_folder(), "scatter_residFit_seasIntensityRR_multiSeason", ".png")
  
  # plot
  plotOutput <- ggplot(plotDat, aes(x = Fitted, y = Residuals)) +
    geom_point(alpha = 0.7) + 
    geom_hline(yintercept = 0, colour = "grey50") +
    theme_bw() + 
    theme(text = element_text(size = 13), legend.margin = margin(), legend.position = "bottom") +
    facet_wrap(~season, nrow = 2, scales = "free")
  
  ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)
}
################################

choro_obsFit_epiDuration_oneSeason <- function(modCodeStr, pltFormats, filepathList){
  # plot side-by-side choropleths for the observed and fitted epidemic duration
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  popCode <- pltFormats$popCode
  if (is.null(pltFormats$legendStep)){
    legendStep <- 1
  } else{
    legendStep <- pltFormats$legendStep
  }
  
  # import and clean observed and fitted epidemic duration
  prepDat <- import_obsFit_epiDuration(modCodeStr, filepathList)
  
  # set breaks based on distribution of observed data
  breaks <- seq(floor(min(prepDat$obs_y, na.rm = TRUE)), ceiling(max(prepDat$obs_y, na.rm = TRUE)), by = 3)
  prepDat2 <- prepDat %>%
    mutate(Observed = cut(obs_y, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) %>%
    mutate(Fitted = cut(fit_y, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE))
  factorlvls <- levels(prepDat2$Observed)
  
  plotDat <- prepDat2 %>%
    select(season, fips, Observed, Fitted) %>%
    gather(fig, bin, Observed:Fitted) %>%
    mutate(fig = factor(fig, levels = c("Observed", "Fitted"))) %>%
    mutate(bin = factor(bin, levels = factorlvls, labels = factorlvls, ordered = TRUE))
  print(levels(plotDat$bin))
  
  seasLs <- plotDat %>% distinct(season) %>% unlist
  for (s in seasLs){
    
    exportFname <- paste0(string_msFig_folder(), "choro_obsFit_epiDuration", popCode, "_S", s, ".png")
    pltDat <- plotDat %>% filter(season == s)
    
    # import county mapping info
    ctyMap <- import_county_geomMap()
    
    # plot
    choro <- ggplot() +
      geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
      geom_map(data = pltDat, map = ctyMap, aes(fill = bin, map_id = fips), color = "grey25", size = 0.025) +
      scale_fill_brewer(name = "Duration\n(Weeks)", palette = "OrRd", na.value = "grey60", drop = FALSE) +
      expand_limits(x = ctyMap$long, y = ctyMap$lat) +
      theme_minimal() +
      theme(text = element_text(size = 15), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
      facet_wrap(~fig, nrow=1)
    
    ggsave(exportFname, choro, height = h, width = w, dpi = dp)
    
  }
  
}
################################

choro_obsFit_epiDuration_multiSeason <- function(modCodeStr, pltFormats, filepathList){
  # plot side-by-side choropleths for the observed and fitted epidemic duration (multiple years)
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasDf <- label_seas_predictors()
  popCode <- pltFormats$popCode
  
  # import and clean observed and fitted seasonal intensity RR 
  prepDat <- import_obsFit_epiDuration(modCodeStr, filepathList)
  
  # set breaks based on distribution of observed data
  breaks <- seq(floor(min(prepDat$obs_y, na.rm = TRUE)), ceiling(max(prepDat$obs_y, na.rm = TRUE)), by = 3)
  prepDat2 <- prepDat %>%
    mutate(Observed = cut(obs_y, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) %>%
    mutate(Fitted = cut(fit_y, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE))
  factorlvls <- levels(prepDat2$Observed)
   
  prepDat3 <- prepDat2 %>%
    select(season, fips, Observed, Fitted) %>%
    gather(fig, bin, Observed:Fitted) %>%
    mutate(fig = factor(fig, levels = c("Observed", "Fitted"))) %>%
    mutate(bin = factor(bin, levels = factorlvls, labels = factorlvls, ordered = TRUE)) %>%
    mutate(season = paste0("S", season)) %>%
    mutate(season = factor(season, levels = seasDf$RV, labels = seasDf$pltLabs)) 
  
  # control flow to remove a single season if necessary
  if(is.null(pltFormats$rmSeas)){
    plotDat <- prepDat3
  } else{
    plotDat <- prepDat3 %>%
      filter(season != pltFormats$rmSeas)
  }
  
  print(levels(plotDat$bin))
  
  exportFname <- paste0(string_msFig_folder(), "choro_obsFit_epiDuration_multiSeason", popCode, ".png")
  
  # import county mapping info
  ctyMap <- import_county_geomMap()
  
  # plot
  choro <- ggplot() +
    geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
    geom_map(data = plotDat, map = ctyMap, aes(fill = bin, map_id = fips), color = "grey25", size = 0.025) +
    scale_fill_brewer(name = "Duration\n(Weeks)", palette = "OrRd", na.value = "grey60", drop = FALSE) +
    expand_limits(x = ctyMap$long, y = ctyMap$lat) +
    theme_minimal() +
    theme(text = element_text(size = 12), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom", legend.margin = margin(), legend.box.margin = margin()) +
    facet_grid(season~fig)
  
  ggsave(exportFname, choro, height = h, width = w, dpi = dp)
  
}
################################

choro_fit_epiDuration_oneSeason <- function(modCodeStr, pltFormats, filepathList){
  # plot choropleths for fitted epidemic duration (one year)
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasDf <- label_seas_predictors()
  popCode <- pltFormats$popCode
  if (is.null(pltFormats$legendStep)){
    legendStep <- 4
  } else{
    legendStep <- pltFormats$legendStep
  }
  
  # import and clean observed and fitted seasonal intensity RR 
  prepDat <- import_obsFit_epiDuration(modCodeStr, filepathList)
  print(summary(prepDat))
  
  # set breaks based on distribution of observed data
  breaks <- seq(floor(min(prepDat$obs_y, na.rm = TRUE)), ceiling(max(prepDat$obs_y, na.rm = TRUE)), by = legendStep)
  prepDat2 <- prepDat %>%
    mutate(Observed = cut(obs_y, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE)) %>%
    mutate(Fitted = cut(fit_y, breaks, right = TRUE, include.lowest = TRUE, ordered_result = TRUE))
  factorlvls <- levels(prepDat2$Observed)
   
  prepDat3 <- prepDat2 %>%
    select(season, fips, Observed, Fitted) %>%
    gather(fig, bin, Observed:Fitted) %>%
    filter(fig == "Fitted") %>%
    mutate(fig = factor(fig, levels = c("Fitted"))) %>%
    mutate(bin = factor(bin, levels = factorlvls, labels = factorlvls, ordered = TRUE)) 
  
  # control flow to remove a single season if necessary
  if(is.null(pltFormats$rmSeas)){
    prepDat4 <- prepDat3
  } else{
    prepDat4 <- prepDat3 %>%
      filter(season != pltFormats$rmSeas)
  } 
  print(levels(prepDat4$bin))

  seasLs <- prepDat4 %>% distinct(season) %>% unlist
  for (s in seasLs){

	exportFname <- paste0(string_msFig_folder(), "choro_fit_epiDuration", popCode, "_S", s, ".png")
    plotDat <- prepDat4 %>% filter(season == s)

	# import county mapping info
	ctyMap <- import_county_geomMap()

	# plot
	choro <- ggplot() +
	geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
	geom_map(data = plotDat, map = ctyMap, aes(fill = bin, map_id = fips), color = "grey25", size = 0.025) +
	scale_fill_brewer(name = "Duration\n(Weeks)", palette = "OrRd", na.value = "grey60", drop = FALSE) +
	expand_limits(x = ctyMap$long, y = ctyMap$lat) +
	theme_minimal() +
	theme(text = element_text(size = 10), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom", legend.margin = margin(), legend.box.margin = margin())

	ggsave(exportFname, choro, height = h, width = w, dpi = dp)

  }
  
  
  
}
################################

scatter_obsFit_epiDuration_multiSeason <- function(modCodeStr, pltFormats, filepathList){
  # scatterplot of observed vs fitted epidemic duration, by season
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasDf <- label_seas_predictors()
  
  # import and clean observed and fitted seasonal intensity RR 
  plotDat <- import_obsFit_epiDuration(modCodeStr, filepathList) %>%
    rename(Observed = obs_y, Fitted = fit_y) %>%
    mutate(season = paste0("S", season)) %>%
    mutate(season = factor(season, levels = seasDf$RV, labels = seasDf$pltLabs)) 

  exportFname <- paste0(string_msFig_folder(), "scatter_obsFit_epiDuration_multiSeason", ".png")
  
  # plot
  plotOutput <- ggplot(plotDat, aes(x = Observed, y = Fitted)) +
    geom_point(alpha = 0.7) + 
    geom_abline(yintercept = 0, slope = 1, colour = "grey50") +
    scale_x_continuous(limits = c(0,25)) +
    scale_y_continuous(limits = c(0,25)) +
    theme_bw() + 
    theme(text = element_text(size = 13), legend.margin = margin(), legend.position = "bottom") +
    facet_wrap(~season, nrow = 2)
  
  ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)
}
################################

scatter_residFit_epiDuration_multiSeason <- function(modCodeStr, pltFormats, filepathList){
  # scatterplot of residuals vs fitted epidemic duration, by season
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasDf <- label_seas_predictors()
  
  # import and clean observed and fitted seasonal intensity RR 
  plotDat <- import_obsFit_epiDuration(modCodeStr, filepathList) %>%
    rename(Residuals = resid, Fitted = fit_y) %>%
    mutate(season = paste0("S", season)) %>%
    mutate(season = factor(season, levels = seasDf$RV, labels = seasDf$pltLabs)) 
  print(summary(plotDat))

  exportFname <- paste0(string_msFig_folder(), "scatter_residFit_epiDuration_multiSeason", ".png")
  
  # plot
  plotOutput <- ggplot(plotDat, aes(x = Fitted, y = Residuals)) +
    geom_point(alpha = 0.7) + 
    geom_hline(yintercept = 0, colour = "grey50") +
    theme_bw() + 
    theme(text = element_text(size = 13), legend.margin = margin(), legend.position = "bottom") +
    facet_wrap(~season, nrow = 2, scales = "free")
  
  ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)
}
################################

scatter_obsFit_seasInt_epiDur_multiSeason <- function(modCodeLs, pltFormats, filepathList){
  # scatterplot of observed seasonal intensity RR vs. observed epidemic duration, by season
  print(match.call())
  
  stopifnot(length(modCodeLs) == 2L)

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasDf <- label_seas_predictors()
  modLabs <- pltFormats$modLabs
  
  # import and clean observed & fitted seasonal intensity RR 
  seasIntDat <- import_obsFit_seasIntensityRR(modCodeLs[1], filepathList) %>%
    rename(Observed = obs_rr, Fitted = fit_rr) %>%
    select(modCodeStr, season, fips, Observed, Fitted)

  # import and clean observed & fitted epidemic duration
  epiDurDat <- import_obsFit_epiDuration(modCodeLs[2], filepathList) %>%
    rename(Observed = obs_y, Fitted = fit_y) %>%
    select(modCodeStr, season, fips, Observed, Fitted) 

  plotDat <- bind_rows(seasIntDat, epiDurDat) %>%
    mutate(modCodeStr = factor(modCodeStr, levels = modCodeLs, labels = modLabs)) %>%
    gather(valueType, value, Observed:Fitted) %>%
    spread(modCodeStr, value) %>%
    mutate(season = paste0("S", season)) %>%
    mutate(season = factor(season, levels = seasDf$RV, labels = seasDf$pltLabs)) %>%
    mutate(valueType = factor(valueType, levels = c("Observed", "Fitted")))

  exportFname <- paste0(string_msFig_folder(), "scatter_obs_seasInt_epiDur_multiSeason", ".png")
  
  # plot
  plotOutput <- ggplot(plotDat, aes(x = seasIntensity, y = epiDuration)) +
    geom_point(alpha = 0.7) + 
    scale_x_continuous("Seasonal Intensity (Relative Risk)") +
    scale_y_continuous("Epidemic Duration (Weeks)") +
    theme_bw() + 
    theme(text = element_text(size = 13), legend.margin = margin(), legend.position = "bottom") +
    facet_grid(season~valueType)
  
  ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)
}
###############################

scatter_obsFit_seasIntensityRR_multiSeason_age <- function(modCodeLs, pltFormats, filepathList){
  # scatterplot of observed vs fitted seasonal intensity RR values, colored by season, panels for age groups
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasDf <- label_seas_predictors()
  ageLabs <- pltFormats$ageLabs
  
  # import and clean observed and fitted seasonal intensity RR for multiple modCodes
  prepDat <- data.frame()
  for (modCodeStr in modCodeLs){
    dummyDat <- import_obsFit_seasIntensityRR(modCodeStr, filepathList) %>%
    mutate(modCodeStr = modCodeStr)
    prepDat <- bind_rows(prepDat, dummyDat)
  }

  # add labels
  plotDat <- prepDat %>%
    rename(Observed = obs_rr, Fitted = fit_rr) %>%
    mutate(season = paste0("S", season)) %>%
    mutate(season = factor(season, levels = seasDf$RV, labels = seasDf$pltLabs)) %>%
    mutate(modCodeStr = factor(modCodeStr, levels = modCodeLs, labels = ageLabs))
  print(summary(plotDat))
  
  exportFname <- paste0(string_msFig_folder(), "scatter_obsFit_seasIntensityRR_multiSeason_age", ".png")
  
  # plot
  plotOutput <- ggplot(plotDat, aes(x = Observed, y = Fitted)) +
    geom_point(aes(colour = season), alpha = 0.7) +
    geom_abline(yintercept = 0, slope = 1, colour = "grey50") +
    scale_colour_tableau('tableau10medium') +
    # scale_x_continuous(limits = c(-5,5)) +
    # scale_y_continuous(limits = c(-5,5)) +
    theme_bw() + 
    theme(text = element_text(size = 13), legend.margin = margin(), legend.position = "bottom") +
    facet_wrap(~modCodeStr, nrow = 1, scales = "free")
  
  ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)
}
################################

choro_stateEffects <- function(modCodeStr){
# draw state choropleth with indicators of group effects
  print(match.call())

  # plot formatting
  states_map <- map_data("state")
  h <- 1.75; w <- 3; dp <- 300
  exportFname <- paste0(string_msFig_folder(), "choro_stGroupEffects_", modCodeStr, ".png")

  # import coef data
  coefDat <- read_csv(string_coef_fname(modCodeStr), col_types = "ccd_cccddddd__") %>%
    filter(effectType == 'stID') %>%
    clean_RVnames(.) %>%
    mutate(fips = substr.Right(paste0("0", RV), 2)) %>%
    left_join(import_stAbbr(), by = "fips") 
  View(coefDat)
  # prepare data for plotting
  plotDat <- indicate_signif(coefDat)
  View(plotDat)
  # plot
  choro <- ggplot(plotDat, aes(map_id = State)) +
    geom_map(aes(fill = signif2), map = states_map, color = "grey50") +
    scale_fill_manual(name = "Signif.", values = c("-1" = "#ca0020", "1" = "#0571b0"), breaks = c("-1", "1"), labels = c("(-)", "(+)"), na.value = "grey75") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    theme_minimal() +
    theme(text = element_text(size = 12), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = "left")
  
  ggsave(exportFname, choro, height = h, width = w, dpi = dp)  
}

################################
forest_coefDistr_seasEffects <- function(modCodeStr){
  print(match.call())
  
  # plot formatting
  seasLabels <- label_seas_predictors()
  exportFname <- paste0(string_msFig_folder(), "forest_coefSeas_", modCodeStr, ".png")
  plotFormats <- list(w=3.5, h=2)
  
  # import season coef data
  importDat <- read_csv(string_coef_fname(modCodeStr), col_types = "ccd_cccddddd__") 
  coefDat <- importDat %>%
    filter(effectType == 'season') %>%
    clean_RVnames(.) %>%
    mutate(RV = factor(RV, levels = seasLabels$RV, labels = seasLabels$pltLabs))
  
  # prepare data for plotting
  plotDat <- calculate_95CI(coefDat) 
  
  # plot 
  plot_coefDistr_RV(plotDat, exportFname, plotFormats)
}

################################
forest_coefDistr_fixedEffects <- function(modCodeStr, is2009p){
  print(match.call())
  
  # plot formatting
  if (is2009p == TRUE){
    ecolLabels <- label_ecol_predictors_2009p()
  }else{
    ecolLabels <- label_ecol_predictors()
  }
  ecolFname <- paste0(string_msFig_folder(), "forest_coefEcol_", modCodeStr, ".png")
  ecolFormats <- list(w=6, h=2.75)
  measLabels <- label_meas_predictors()
  measFname <- paste0(string_msFig_folder(), "forest_coefMeas_", modCodeStr, ".png")
  measFormats <- list(w=2, h=2.75)
  
  # import summary stats data
  fullDf <- read_csv(string_coef_fname(modCodeStr), col_types = "ccd_cccddddd__") 
  
  # prepare data for plotting
  coefDf <- calculate_95CI(fullDf) %>%
    filter(!grepl("intercept", RV)) 

  ## plot fixed effects (ecological) ##
  XDat <- coefDf %>% 
    filter(effectType == 'fixed' & grepl("X_", RV)) %>%
    clean_RVnames(.) %>%
    mutate(RV = factor(RV, levels = ecolLabels$RV, labels = ecolLabels$pltLabs))
  plot_coefDistr_RV(XDat, ecolFname, ecolFormats)

  ## plot fixed effects (measurement) ##
  ODat <- coefDf %>%
    filter(effectType == 'fixed' & grepl("O_", RV)) %>%
    clean_RVnames(.) %>%
    mutate(RV = factor(RV, levels = measLabels$RV, labels = measLabels$pltLabs))
  plot_coefDistr_RV(ODat, measFname, measFormats)
}

################################
plot_coefDistr_RV <- function(plotDat, exportFname, pltFormats){
  # plot all coef mean & 95%CI over RV
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  
  # forest plot
  plotOutput <- ggplot(plotDat, aes(x = RV, y = mean)) +
    geom_pointrange(aes(ymin = LB, ymax = UB, colour = signif)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    scale_y_continuous("posterior mean (95%CI)") +
    scale_colour_manual(limits = c(TRUE, FALSE), values = c("#008837", "grey75")) +
    guides(colour = FALSE) +
    theme_bw() + 
    theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=45, vjust=1, hjust=1), axis.text=element_text(size=10), text = element_text(size = 10))

  ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)
}

################################
forest_coefDistr_fixedEffects_singleSeason <- function(modCodeStr){
  print(match.call())
  
  # plot formatting
  ecolLabels <- label_ecol_predictors()
  ecolFname <- paste0(string_msFig_folder(), "forest_coefEcol_", modCodeStr, ".png")
  ecolFormats <- list(w=6, h=6)
  measLabels <- label_meas_predictors()
  measFname <- paste0(string_msFig_folder(), "forest_coefMeas_", modCodeStr, ".png")
  measFormats <- list(w=6, h=3)
  
  # grab list of season file names
  fnameLs <- stringLs_coef_fname(modCodeStr)
  fullDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), effectType = c(), likelihood = c(), mean = c(), sd = c(), q_025 = c(), q_5 = c(), q_975 = c()))
  
  # import files
  for (infile in fnameLs){
    seasFile <- read_csv(infile, col_types = "cci_cccddddd__")
    fullDf <- bind_rows(fullDf, seasFile)
  }
  
  # prepare data for plotting
  coefDf <- calculate_95CI(fullDf) %>%
    filter(!grepl("intercept", RV)) 
    
  # plot fixed effects (ecological)
  XDat <- coefDf %>% filter(effectType == 'fixed' & grepl("X_", RV)) %>% clean_RVnames(.) %>%
    mutate(RV = factor(RV, levels = ecolLabels$RV, labels = ecolLabels$pltLabs))
  plot_coefDistr_season(XDat, ecolFname, ecolFormats)
  
  # plot fixed effects (measurement)
  ODat <- coefDf %>% filter(effectType == 'fixed' & grepl("O_", RV)) %>% clean_RVnames(.) %>%
    mutate(RV = factor(RV, levels = measLabels$RV, labels = measLabels$pltLabs))
  plot_coefDistr_season(ODat, measFname, measFormats)
}

################################
plot_coefDistr_season <- function(plotDat, exportFname, pltFormats){
  # single season mods
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  skip <- 3

  # plot effects over time
  plotOutput <- ggplot(plotDat, aes(x = season, y = mean, group = RV)) +
    geom_pointrange(aes(ymin = LB, ymax = UB, colour = signif)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    scale_y_continuous("posterior mean (95%CI)") +
    scale_colour_manual(limits = c(TRUE, FALSE), values = c("#008837", "grey75")) +
    facet_wrap(~RV, scales = "free_y") +
    scale_x_continuous("flu season", breaks = seq(3,9,skip), labels = label_seas_predictors()$pltLabs[seq(1, length(label_seas_predictors()$pltLabs), skip)]) +
    guides(colour = FALSE) +
    theme_bw() +
    theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=45, vjust=1, hjust=1), axis.text=element_text(size=12))
  ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)
}

################################
dot_coefCompareReplicates <- function(baseCodeLs, pltFormats){
  # replicate comparison
  print(match.call())

  # plot formatting
  totLabels <- label_tot_predictors()
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  exportFname <- paste0(string_msFig_folder(), "dot_coefCompareReplicates_", pltFormats$descrip, ".png")

  # data formatting
  numReplicates <- pltFormats$numReplicates
  
  ## grab coefData from multiple models ##
  fullDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), effectType = c(), likelihood = c(), mean = c(), sd = c(), q_025 = c(), q_5 = c(), q_975 = c()))
  
  # import complete model coefData
  completeCode <- baseCodeLs[which(nchar(baseCodeLs)==min(nchar(baseCodeLs)))]
  completeDat <- read_csv(string_coef_fname(completeCode), col_types = "ccd_cccddddd__") %>%
    filter(effectType == 'fixed') %>%
    filter(!grepl("intercept", RV)) %>%
    indicate_signif(.) %>%
    clean_RVnames(.) %>%
    mutate(RV = factor(RV, levels = totLabels$RV, labels = totLabels$pltLabs)) %>%
    drop_na(signif2) %>%
    mutate(dotsize = abs(mean), dotalpha = 1) %>%
    select(modCodeStr, RV, signif2, dotsize, dotalpha)

  # import replicates for each baseCode
  baseRepCodeLs <- baseCodeLs[which(nchar(baseCodeLs)!=min(nchar(baseCodeLs)))]
  for(baseRepCode in baseRepCodeLs){
    repCodeLs <- c(paste0(baseRepCode, ""), paste(baseRepCode, 1:(numReplicates-1), sep = "-"))
    modDat <- aggregate_coefReplicates(repCodeLs)
    fullDf <- bind_rows(fullDf, modDat)
    print(paste(baseRepCode, "imported"))
  }
  
  # prepare variables for plotting
  plotDat <- bind_rows(fullDf, completeDat) %>%
    mutate(modCodeStr = factor(modCodeStr, levels = rev(pltFormats$lvls), labels = rev(pltFormats$labs))) %>%
    arrange(modCodeStr, RV)
  
  # plot
  plotOutput <- ggplot(plotDat, aes(x = RV, y = modCodeStr)) +
    geom_point(aes(colour = signif2, size = dotsize, alpha = dotalpha)) +
    geom_hline(yintercept = length(baseCodeLs)-0.5, size = 0.25) +
    scale_colour_manual("Signif.", values = c("-1" = "#ca0020", "1" = "#0571b0"), breaks = c("-1", "1"), labels = c("(-)", "(+)"), na.value = "white") +
    scale_size(trans = "exp") +
    scale_alpha("% Replicates", labels = rev(pltFormats$replabs), breaks = rev(pltFormats$replvls)) +
    scale_x_discrete(position = "top") +
    guides(size = FALSE, colour = guide_legend(order=1)) +
    theme_bw() +
    theme(axis.title=element_blank(), axis.text.x=element_text(angle=45, vjust=1, hjust=0), axis.text=element_text(size=12), panel.grid = element_blank(), legend.position="right", legend.margin = margin(1,1,1,1, unit="pt"))
  ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)
  
}

################################
choro_fitCompareReplicates <- function(baseCodeLs, pltFormats){
  print(match.call())
  
  stopifnot(length(baseCodeLs) >= 2L)
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasLabels <- label_seas_predictors()
  nomatchThresh <- pltFormats$nomatchThresh
  legendposition <- ifelse(length(baseCodeLs)>=4, "bottom", "left")
  exportFname <- paste0(string_msFig_folder(), "choro_fitCompareReplicates_", pltFormats$descrip, "_", pltFormats$nomatchThresh*100, ".png")

  # data formatting
  numReplicates <- pltFormats$numReplicates
  numSeas <- nrow(seasLabels)
  repcodelength <- pltFormats$repcodelength
  
  ## grab fitData from multiple models ##
  fullDf <- tbl_df(data.frame(modCodeStr = c(), season = c(), fips = c(), LB = c(), UB = c()))
  
  # import complete model fitData
  completeCode <- baseCodeLs[which(nchar(baseCodeLs)==min(nchar(baseCodeLs)))]
  completeDat <- read_csv(string_fit_fname(completeCode), col_types = "c_d_c_dd______") %>%
      mutate(LB = mean-(1*sd), UB = mean+(1*sd)) %>% # 5/15/17 okay approximation because we are looking at overlap between posteriors (not 95% CI)
      select(modCodeStr, season, fips, LB, UB)

  # import replicates for each baseCode
  baseRepCodeLs <- baseCodeLs[which(nchar(baseCodeLs)!=min(nchar(baseCodeLs)))]
  for (baseRepCode in baseRepCodeLs){
    repCodeLs <- c(paste0(baseRepCode, ""), paste(baseRepCode, 1:(numReplicates-1), sep = "-"))
    modDat <- import_fitReplicates(repCodeLs) 
    fullDf <- bind_rows(fullDf, modDat)
    print(paste(baseRepCode, "imported"))
  }

  # clean and organize bound data  
  prepDat <- bind_rows(fullDf, completeDat) %>%
    gather(bound, value, LB:UB) %>%
    mutate(bound_spread = paste(modCodeStr, bound, sep = "_")) %>%
    arrange(modCodeStr, bound) 

  # create bound dataframe
  boundLs <- prepDat %>% distinct(bound_spread) %>% unlist
  boundMx <- matrix(boundLs, ncol=2, byrow = TRUE)

  # identify overlaps with complete model
  spreadDat <- prepDat %>%
    select(-bound, -modCodeStr) %>%
    spread(bound_spread, value) 

  # indicate overlap with all replicates
  overlapDat <- spreadDat
  for (i in 2:nrow(boundMx)){
    newcol <- paste0("o_", substring(boundMx[i,1], 16, nchar(boundMx[i,1])-3))
    overlapDat <- do.call(overlapping_intervals, list(df = overlapDat, intervalA_LB = boundMx[1,1], intervalA_UB = boundMx[1,2], intervalB_LB = boundMx[i,1], intervalB_UB = boundMx[i,2])) %>%
      rename_(.dots = setNames("overlap", newcol))
  }
  
  # summarise across replicates and seasons
  onlyOverlapDat <- overlapDat %>%
    select(season, fips, contains("o_")) %>%
    gather(repcode, overlap, contains("o_")) %>%
    mutate(repgroup = substring(repcode, 1, repcodelength)) %>%
    group_by(fips, repgroup) %>%
    summarise(choroalpha = ((numReplicates*numSeas)-sum(as.numeric(overlap)))/(numReplicates*numSeas)) %>% # proportion replicate-seasons without overlaps
    ungroup %>%
    mutate(chorofill = ifelse(choroalpha >= nomatchThresh, "1", "0")) # 0 = overlaps, 1 = proportion of no overlaps is >= nomatchThresh

  # prepare data for plotting
  plotDat <- onlyOverlapDat %>% 
    mutate(modCodeStr = paste0(completeCode, substring(repgroup, 2, nchar(repgroup)))) %>%
    mutate(modCodeStr = factor(modCodeStr, levels = pltFormats$lvls, labels = pltFormats$labs)) %>%
    select(modCodeStr, fips, chorofill, choroalpha)

  # import county mapping info
  ctyMap <- import_county_geomMap()
  
  # plot
  choro <- ggplot() +
    geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
    geom_map(data = plotDat, map = ctyMap, aes(fill = chorofill, map_id = fips), color = "grey50", size = 0.025) +
    scale_fill_manual(name = "", values = c("1" = "#7b3294", "0" = "grey75"), breaks = c("1", "0"), labels = c("failure\nto match", paste0("match"))) +
    expand_limits(x = ctyMap$long, y = ctyMap$lat) +
    theme_minimal() +
    theme(text = element_text(size = 16), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.position = legendposition) +
    facet_wrap(~modCodeStr, nrow=1)
  
  ggsave(exportFname, choro, height = h, width = w, dpi = dp)
  
}

################################
dot_coefCompare <- function(modCodeLs, pltFormats){
  # direct comparison
  print(match.call())

  # plot formatting
  totLabels <- label_tot_predictors()
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  exportFname <- paste0(string_msFig_folder(), "dot_coefCompare_", pltFormats$descrip, ".png")
  
  # grab coefData from multiple models
  fullDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), effectType = c(), likelihood = c(), mean = c(), sd = c(), q_025 = c(), q_5 = c(), q_975 = c()))
  
  for(modCode in modCodeLs){
    modDat <- read_csv(string_coef_fname(modCode), col_types = "ccd_cccddddd__")
    fullDf <- bind_rows(fullDf, modDat)
  }
  
  # prepare variables for plotting
  plotDat <- fullDf %>%
    filter(effectType == 'fixed') %>%
    filter(!grepl("intercept", RV)) %>%
    indicate_signif(.) %>%
    clean_RVnames(.) %>%
    mutate(RV = factor(RV, levels = totLabels$RV, labels = totLabels$pltLabs)) %>%
    mutate(modCodeStr = factor(modCodeStr, levels = rev(pltFormats$lvls), labels = rev(pltFormats$labs))) %>%
    drop_na(signif2) %>%
    mutate(dotsize = abs(mean)) %>%
    select(modCodeStr, RV, dotsize, signif2)
  
  # plot
  plotOutput <- ggplot(plotDat, aes(x = RV, y = modCodeStr)) +
    geom_point(aes(colour = signif2, size = dotsize)) +
    scale_colour_manual("Signif.", values = c("-1" = "#ca0020", "1" = "#0571b0"), breaks = c("-1", "1"), labels = c("(-)", "(+)"), na.value = "white") +
    scale_size(trans = "exp") +
    scale_x_discrete(position = "top") +
    guides(size = FALSE) +
    theme_bw() +
    theme(axis.title=element_blank(), axis.text.x=element_text(angle=45, vjust=1, hjust=0), axis.text=element_text(size=12), panel.grid = element_blank(), legend.position="right", legend.margin = margin(1,1,1,1, unit="pt"))
  ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)
  
}

################################
choro_fitCompare <- function(modCodeLs, pltFormats){
  print(match.call())
  
  stopifnot(length(modCodeLs) == 2L)
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasLabels <- label_seas_predictors()
  exportFname <- paste0(string_msFig_folder(), "choro_fitCompare_", pltFormats$descrip, ".png")
  modLabelsDf <- tbl_df(data.frame(modCodeStr = pltFormats$lvls, modLabs = pltFormats$labs, stringsAsFactors = FALSE))
  
  # grab fitData from multiple models
  fullDf <- tbl_df(data.frame(modCodeStr = c(), season = c(), fips = c(), LB = c(), UB = c()))
  
  for (modCode in modCodeLs){
    modDat <- read_csv(string_fit_fname(modCode), col_types = "c_d_c_dd______") %>%
      mutate(LB = mean-(1*sd), UB = mean+(1*sd)) %>%
      select(modCodeStr, season, fips, LB, UB) 
    fullDf <- bind_rows(fullDf, modDat)
  }

  # prepare data for plotting
  args <- name_intervals(pltFormats$labs)
  pltDat <- fullDf %>%
    gather(bound, value, LB:UB) %>%
    left_join(modLabelsDf, by = "modCodeStr") %>%
    mutate(bound_spread = paste(modLabs, bound, sep = "_")) %>%
    select(-bound, -modLabs, -modCodeStr) %>%
    spread(bound_spread, value) %>%
    overlapping_intervals(args$intervalA_LB, args$intervalA_UB, args$intervalB_LB, args$intervalB_UB) %>%
    mutate(season = factor(paste0("S", season), levels = seasLabels$RV, labels = seasLabels$pltLabs))

  # import county mapping info
  ctyMap <- import_county_geomMap()
  
  # plot
  choro <- ggplot() +
    geom_map(data = ctyMap, map = ctyMap, aes(x = long, y = lat, map_id = region)) +
    geom_map(data = pltDat, map = ctyMap, aes(fill = overlap, map_id = fips), color = "grey50", size = 0.05) +
    scale_fill_manual(name = "", values = c("1" = "grey75", "0" = "#7b3294"), breaks = c("1", "0"), labels = c("match", "no match"), na.value = "grey75") +
    expand_limits(x = ctyMap$long, y = ctyMap$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = c(.9,.3)) +
    facet_wrap(~season, nrow=2)
  
  ggsave(exportFname, choro, height = h, width = w, dpi = dp)
}
################################

scatter_regionValidationILI <- function(modCodeStr, pltFormats){
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasDf <- label_seas_predictors()
  exportFname <- paste0(string_msFig_folder(), "scatter_regionValidationILI_", pltFormats$age, "_byregion.png")
  
  plotDat <- import_regionValidationILI(modCodeStr, pltFormats) %>%
    mutate(season = paste0("S", season)) %>%
    mutate(season = factor(season, levels = seasDf$RV, labels = seasDf$pltLabs)) %>%
    mutate(region = factor(paste("Region", reg), levels = paste("Region", 1:10)))
  # correlation coefficient
  corCoef <- cor.test(plotDat$ili_unwt_pct, plotDat$mean, alternative = "two.sided", method = "pearson", use = "complete.obs")
  print(paste(modCodeStr, "region validation ILI"))
  print(corCoef)
  
  # plot
  plotOutput <- ggplot(plotDat, aes(x = ili_unwt_pct, y = mean)) +
    geom_point(aes(colour = region), alpha = 0.7) +
    scale_x_continuous("ILI % of Patients (CDC)", limits = c(0,pltFormats$xmax)) +
    scale_y_continuous("Model Fitted Mean (Avg for HHS Region)", limits = c(0,4.25)) +
    scale_colour_tableau('', palette = 'tableau10medium') +
    theme_bw() +
    theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) 
  
  ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)
}
################################

scatter_regionValidationViral <- function(modCodeStr, pltFormats){
  print(match.call())
  
  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  seasDf <- label_seas_predictors()
  exportFname <- paste0(string_msFig_folder(), "scatter_regionValidationViral", "_byregion.png")
  
  plotDat <- import_regionValidationViral(modCodeStr, pltFormats) %>%
    mutate(season = paste0("S", season)) %>%
    mutate(season = factor(season, levels = seasDf$RV, labels = seasDf$pltLabs)) %>%
    mutate(region = factor(paste("Region", reg), levels = paste("Region", 1:10)))
  # correlation coefficient
  corCoef <- cor.test(plotDat$perc_pos, plotDat$mean, alternative = "two.sided", method = "pearson", use = "complete.obs")
  print(paste(modCodeStr, "region validation viral"))
  print(corCoef)
  
  # plot
  plotOutput <- ggplot(plotDat, aes(x = perc_pos, y = mean)) +
    geom_point(aes(colour = region), alpha = 0.7) +
    scale_x_continuous("% Positive Lab Confirmation (CDC)", limits = c(0,pltFormats$xmax)) +
    scale_y_continuous("Model Fitted Mean (Avg for HHS Region)", limits = c(0,4.25)) +
    scale_colour_tableau('', palette = 'tableau10medium') +
    theme_bw() +
    theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) 
  
  ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)
}
################################
bxp_rawPredictors_region <- function(filepathList, pltFormats){
  print(match.call())

  # plot formatting
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  pltLabs <- label_tot_predictors()

  plotDat <- model8a_iliSum_v7_raw(filepathList) %>%
    select(fips, season, regionID, contains("rX_"), contains("rO_")) %>%
    gather("RV", "value", -fips, -season, -regionID) %>%
    mutate(RV = gsub("rX_", "", RV)) %>%
    mutate(RV = gsub("rO_", "", RV)) %>%
    left_join(pltLabs, by = "RV") %>%
    select(-RV) %>%
    spread(pltLabs, value) %>%
    mutate(regionID = factor(as.character(regionID), levels = as.character(1:10)))
  varnames <- names(plotDat)[4:ncol(plotDat)]
  # View(plotDat %>% select(fips, season, regionID, fluH3, fluB))

  # # plot
  # for (i in 1:length(varnames)){
  #   exportFname <- paste0(string_msFig_folder(), "rawPredictors_region/bxp_predictorByRegion_", varnames[i], ".png")
  # 
  # plotOutput <- ggplot(plotDat, aes_string(x = "regionID", y = varnames[i])) +
  #   geom_boxplot() +
  #   scale_x_discrete("Region") +
  #   theme_bw() +
  #   theme(text = element_text(size = 12), legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) 
  # 
  # ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)
  # }
}
