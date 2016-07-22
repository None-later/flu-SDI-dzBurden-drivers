
## Name: Elizabeth Lee
## Date: 7/21/16
## Function: general functions to export hurdle model INLA results as data files -- county
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(RColorBrewer); require(ggplot2)


#### functions for results plots by modcode  ################################

################################

importPlot_coefDistr_season_hurdle <- function(path_csvExport, path_plotExport_coefDistr){
  # 7/21/16: import coefficient distributions across seasons, separate figures for each likelihood
  print(match.call())
  
  # grab list of files names
  setwd(path_csvExport)
  readfile_list <- grep("summaryStats_", list.files(), value = TRUE)
  coefDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), effectType = c(), likelihood = c(), mean = c(), sd = c(), q_025 = c(), q_5 = c(), q_975 = c()))
  
  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "cci_cccddddd__")
    coefDf <- bind_rows(coefDf, seasFile)
  }
  
  # separate plots for data from each likelihood
  likelihoods <- coefDf %>% distinct(likelihood) %>% unlist
  for (lik in likelihoods){
    coefDf_lik <- coefDf %>% filter(likelihood == lik)
    
    # plot fixed effects
    fxDat <- coefDf_lik %>% filter(effectType == 'fixed') 
    plot_coefDistr_season(fxDat, path_plotExport_coefDistr, sprintf('fixed_%sLikelihood.png', lik))
    
    # plot random effects
    sampleLs <- coefDf_lik %>% filter(effectType == 'spatial') %>% select(RV) %>% sample_n(9) %>% unlist
    rdmDat <- coefDf_lik %>% filter(effectType == 'spatial' & RV %in% sampleLs) 
    plot_coefDistr_season(rdmDat, path_plotExport_coefDistr, sprintf('random_%sLikelihood.png', lik))
    
    # plot effects of state ID
    stIds <- coefDf_lik %>% filter(effectType == 'stID') %>% distinct(RV) %>% unlist 
    stIdDat <- coefDf_lik %>% filter(effectType == 'stID') %>% mutate(RV = factor(RV, levels = stIds))
    plot_coefDistr_season(stIdDat, path_plotExport_coefDistr, sprintf('stateID_%sLikelihood.png', lik))
    
    # plot effects of state ID
    regIds <- coefDf_lik %>% filter(effectType == 'regID') %>% distinct(RV) %>% unlist 
    regIdDat <- coefDf_lik %>% filter(effectType == 'regID') %>% mutate(RV = factor(RV, levels = regIds))
    plot_coefDistr_season(regIdDat, path_plotExport_coefDistr, sprintf('regionID_%sLikelihood.png', lik))
  } 
}
################################


#### functions for data processing  ################################

################################
logit <- function(probability) {
  return(log(probability)-log(1-probability))
}
################################


#### functions for data export  ################################

################################

export_summaryStats_hurdle <- function(exportPath, modelOutput, rdmFxTxt, modCodeString, dbCodeString, season){
  # export summary statistics of INLA model output for hurdle model variables -- fixed and random effects in the same file
  print(match.call())
  
  ## change variable names output from INLA ##
  names(modelOutput$summary.fixed) <- c("mean", "sd", "q_025", "q_5", "q_975", "mode", "kld")
  # random effects for binomial model #
  names(modelOutput$summary.random$fips_bin) <- c("ID", names(modelOutput$summary.fixed))
  names(modelOutput$summary.random$fips_st_bin) <- names(modelOutput$summary.random$fips_bin)
  names(modelOutput$summary.random$regionID_bin) <- names(modelOutput$summary.random$fips_bin)
  # random effects for nonzero model (gamma) #
  names(modelOutput$summary.random$fips_nonzero) <- names(modelOutput$summary.random$fips_bin)
  names(modelOutput$summary.random$fips_st_nonzero) <- names(modelOutput$summary.random$fips_bin)
  names(modelOutput$summary.random$regionID_nonzero) <- names(modelOutput$summary.random$fips_bin)
  
  # clean fixed effects summary statistics output from INLA
  summaryFixed <- tbl_df(modelOutput$summary.fixed) %>%
    mutate(RV = rownames(modelOutput$summary.fixed)) %>%
    mutate(effectType = "fixed") %>%
    mutate(likelihood = ifelse(grepl("_bin", RV, fixed=TRUE), "binomial", ifelse(grepl("_nonzero", RV, fixed=TRUE), "gamma", NA))) %>%
    select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
  
  # clean random effects summary statistics output from INLA
  summaryRandomFips <- bind_rows(modelOutput$summary.random$fips_bin %>% mutate(likelihood = "binomial"), 
                                 modelOutput$summary.random$fips_nonzero %>% mutate(likelihood = "gamma")) %>%
    mutate(effectType = "spatial") 
  summaryRandomSt <- bind_rows(modelOutput$summary.random$fips_st_bin %>% mutate(likelihood = "binomial"), 
                               modelOutput$summary.random$fips_st_nonzero %>% mutate(likelihood = "gamma")) %>%
    mutate(effectType = "stID") 
  summaryRandomReg <- bind_rows(modelOutput$summary.random$regionID_bin %>% mutate(likelihood = "binomial"), 
                                modelOutput$summary.random$regionID_nonzero %>% mutate(likelihood = "gamma")) %>%
    mutate(ID = as.character(ID)) %>%
    mutate(effectType = "regID") 
  
  # bind random effects summary statistics
  summaryRandom <- bind_rows(summaryRandomFips, summaryRandomSt, summaryRandomReg) %>%
    rename(RV = ID) %>%
    select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
  
  # bind data together
  summaryStats <- bind_rows(summaryFixed, summaryRandom) %>%
    mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date())) %>%
    select(modCodeStr, dbCodeStr, season, exportDate, RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
  
  # export data to file
  write_csv(summaryStats, exportPath)
  
}
################################

export_summaryStats_fitted_hurdle <- function(exportPath, oneLik_fits, modDataFullOutput, modCodeString, dbCodeString, season){
  # process binomial likelihood or gamma likelihood fitted values for diagnostic plotting
  print(match.call())
  
  names(oneLik_fits) <- c("mean", "sd", "q_025", "q_5", "q_975", "mode")
  modOutput_fitted <- bind_cols(modDataFullOutput %>% select(fips, ID, y), oneLik_fits) %>% 
      mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date())) %>%
      select(modCodeStr, dbCodeStr, season, exportDate, fips, ID, mean, sd, q_025, q_5, q_975, mode, y)
  
  # export data to file
  write_csv(modOutput_fitted, exportPath)
  # return modified output
  modOutput_fitted2 <- modOutput_fitted %>%
    select(-modCodeStr, -dbCodeStr, -exportDate)
  
  return(modOutput_fitted2)
}
################################

