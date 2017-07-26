
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
source("source_export_inlaDiagnostics.R")

#### functions for results plots by modcode  ################################

################################

importPlot_coefDistr_season_hurdle <- function(path_csvExport, path_plotExport_coefDistr){
  # 7/21/16: import coefficient distributions across seasons, separate figures for each likelihood
  print(match.call())
  
  # grab list of files names
  setwd(path_csvExport)
  readfile_list <- grep("summaryStats_", list.files(), value = TRUE)
  fullDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), effectType = c(), likelihood = c(), mean = c(), sd = c(), q_025 = c(), q_5 = c(), q_975 = c()))
  
  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "cci_cccddddd__")
    fullDf <- bind_rows(fullDf, seasFile)
  }
  
  coefDf <- fullDf %>%
    mutate(LB = mean-(2*sd), UB = mean+(2*sd)) %>%
    mutate(signif = ifelse(UB < 0 | LB > 0, TRUE, FALSE))
  
  # separate plots for data from each likelihood
  likelihoods <- coefDf %>% filter(!is.na(likelihood)) %>% distinct(likelihood) %>% unlist
  for (lik in likelihoods){
    coefDf_lik <- coefDf %>% filter(likelihood == lik)
    
    # plot fixed effects
    fxDat <- coefDf_lik %>% filter(effectType == 'fixed') %>% clean_RVnames(.)
    plot_coefDistr_season(fxDat, path_plotExport_coefDistr, sprintf('fixed_%sLikelihood.png', lik))
    
    # plot fixed effects (surveillance)
    ODat <- coefDf_lik %>% filter(effectType == 'fixed' & grepl("O_", RV)) %>% clean_RVnames(.)
    plot_coefDistr_season(ODat, path_plotExport_coefDistr, sprintf('fixedSurveil_%sLikelihood.png', lik))
    
    # plot fixed effects (ecological)
    XDat <- coefDf_lik %>% filter(effectType == 'fixed' & grepl("X_", RV)) %>% clean_RVnames(.)
    plot_coefDistr_season(XDat, path_plotExport_coefDistr, sprintf('fixedEcol_%sLikelihood.png', lik))
    
    # plot random effects
    if (nrow(coefDf_lik %>% filter(effectType == 'spatial')) > 0){
      sampleLs <- coefDf_lik %>% filter(effectType == 'spatial') %>% select(RV) %>% sample_n(56) %>% unlist
      rdmDat <- coefDf_lik %>% filter(effectType == 'spatial' & RV %in% sampleLs) %>% clean_RVnames(.)
      plot_coefDistr_season(rdmDat, path_plotExport_coefDistr, sprintf('random_%sLikelihood.png', lik))
    }
    
    # plot effects of state ID
    if (nrow(coefDf_lik %>% filter(effectType == 'stID')) > 0){
      stIds <- coefDf_lik %>% filter(effectType == 'stID') %>% distinct(RV) %>% unlist 
      stIdDat <- coefDf_lik %>% filter(effectType == 'stID') %>% clean_RVnames(.) %>% mutate(RV = factor(RV, levels = stIds))
      plot_coefDistr_season(stIdDat, path_plotExport_coefDistr, sprintf('stateID_%sLikelihood.png', lik))
    }
    
    # plot effects of region ID
    if (nrow(coefDf_lik %>% filter(effectType == 'regID')) > 0){
      regIds <- coefDf_lik %>% filter(effectType == 'regID') %>% distinct(RV) %>% unlist 
      regIdDat <- coefDf_lik %>% filter(effectType == 'regID') %>% clean_RVnames(.) %>% mutate(RV = factor(RV, levels = regIds))
      plot_coefDistr_season(regIdDat, path_plotExport_coefDistr, sprintf('regionID_%sLikelihood.png', lik))
    }

    # plot CAR county effects
    if (nrow(coefDf_lik %>% filter(effectType == 'structured')) > 0){
      sampleStruc <- coefDf_lik %>% filter(effectType == 'structured') %>% clean_RVnames(.) %>% select(RV) %>% sample_n(56) %>% unlist 
      strucDat <- coefDf_lik %>% clean_RVnames(.) %>% filter(effectType == 'structured' & RV %in% sampleStruc) 
      plot_coefDistr_season(strucDat, path_plotExport_coefDistr, sprintf('structured_%sLikelihood.png', lik))
    }
    
    # plot CAR state effects
    if (nrow(coefDf_lik %>% filter(effectType == 'structured_st')) > 0){
      sampleStruc2 <- coefDf_lik %>% filter(effectType == 'structured_st') %>% clean_RVnames(.) %>% select(RV) %>% sample_n(56) %>% unlist 
      strucDat2 <- coefDf_lik %>% clean_RVnames(.) %>% filter(effectType == 'structured_st' & RV %in% sampleStruc2) 
      plot_coefDistr_season(strucDat2, path_plotExport_coefDistr, sprintf('structured_st_%sLikelihood.png', lik))
    }
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

export_summaryStats_hurdle_wHyperpar <- function(exportPath, modelOutput, rdmFxTxt, modCodeString, dbCodeString, season){
  # export summary statistics of INLA model output for hurdle model variables -- fixed and random effects in the same file
  print(match.call())
  
  ## change variable names output from INLA ##
  names(modelOutput$summary.fixed) <- c("mean", "sd", "q_025", "q_5", "q_975", "mode", "kld")
  names(modelOutput$summary.hyperpar) <- names(modelOutput$summary.fixed)[1:6] # 8/17/16 add hyperpar export
  # random effects for binomial model #
  names(modelOutput$summary.random$fips_bin) <- c("ID", names(modelOutput$summary.fixed))
  names(modelOutput$summary.random$fips_st_bin) <- c("ID", names(modelOutput$summary.fixed))
  names(modelOutput$summary.random$regionID_bin) <- c("ID", names(modelOutput$summary.fixed))
  # random effects for nonzero model (gamma) #
  names(modelOutput$summary.random$fips_nonzero) <- c("ID", names(modelOutput$summary.fixed))
  names(modelOutput$summary.random$fips_st_nonzero) <- c("ID", names(modelOutput$summary.fixed))
  names(modelOutput$summary.random$regionID_nonzero) <- c("ID", names(modelOutput$summary.fixed))
  
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
  summaryStats <- bind_rows(summaryFixed, summaryRandom, summaryHyperpar) %>%
    mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date())) %>%
    select(modCodeStr, dbCodeStr, season, exportDate, RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
  
  # export data to file
  write_csv(summaryStats, exportPath)
  
}
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
  names(modelOutput$summary.random$fips_nonzero) <- c("ID", names(modelOutput$summary.fixed))
  names(modelOutput$summary.random$fips_st_nonzero) <- names(modelOutput$summary.random$fips_nonzero)
  names(modelOutput$summary.random$regionID_nonzero) <- names(modelOutput$summary.random$fips_nonzero)
  
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

export_summaryStats_hurdle_binomial <- function(exportPath, modelOutput, rdmFxTxt, modCodeString, dbCodeString, season){
  # export summary statistics of INLA model output for hurdle model variables (binomial component ONLY) -- fixed and random effects in the same file
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
  
  summaryComplete <- bind_rows(summaryFixed, summaryHyperpar)
  # clean random effects summary statistics output from INLA
  if (!is.null(modelOutput$summary.random$fips_bin)){
    names(modelOutput$summary.random$fips_bin) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomFips <- modelOutput$summary.random$fips_bin %>% mutate(likelihood = "binomial") %>%
      mutate(effectType = "spatial") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomFips)
  }
  if (!is.null(modelOutput$summary.random$fips_st_bin)){
    names(modelOutput$summary.random$fips_st_bin) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomSt <- modelOutput$summary.random$fips_st_bin %>% mutate(likelihood = "binomial") %>%
      mutate(effectType = "stID") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomSt)
  }
  if (!is.null(modelOutput$summary.random$regionID_bin)){
    names(modelOutput$summary.random$regionID_bin) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomReg <- modelOutput$summary.random$regionID_bin %>% mutate(likelihood = "binomial") %>%
      mutate(RV = as.character(RV)) %>%
      mutate(effectType = "regID") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomReg)
  }
  
  # select columns
  summaryStats <- summaryComplete %>%
    mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date())) %>%
    select(modCodeStr, dbCodeStr, season, exportDate, RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
  
  # export data to file
  write_csv(summaryStats, exportPath)
  
}
################################

export_summaryStats_hurdle_gamma <- function(exportPath, modelOutput, rdmFxTxt, modCodeString, dbCodeString, season){
  # export summary statistics of INLA model output for hurdle model variables (gamma component ONLY) -- fixed and random effects in the same file
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
  
  summaryComplete <- bind_rows(summaryFixed, summaryHyperpar)
  # clean random effects summary statistics output from INLA
  if (!is.null(modelOutput$summary.random$fips_nonzero)){
    names(modelOutput$summary.random$fips_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomFips <- modelOutput$summary.random$fips_nonzero %>% mutate(likelihood = "gamma") %>%
      mutate(effectType = "spatial") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomFips)
  }
  # clean structure spatial effects  summary statistics output from INLA
  if (!is.null(modelOutput$summary.random$graphIdx_nonzero)){
    names(modelOutput$summary.random$graphIdx_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomGraphid <- modelOutput$summary.random$graphIdx_nonzero %>% mutate(likelihood = "gamma") %>%
      mutate(RV = as.character(paste0("phi", RV))) %>%
      mutate(effectType = "structured") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomGraphid)
  }
  if (!is.null(modelOutput$summary.random$fips_st_nonzero)){
    names(modelOutput$summary.random$fips_st_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomSt <- modelOutput$summary.random$fips_st_nonzero %>% mutate(likelihood = "gamma") %>%
      mutate(effectType = "stID") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomSt)
  }
  if (!is.null(modelOutput$summary.random$regionID_nonzero)){
    names(modelOutput$summary.random$regionID_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomReg <- modelOutput$summary.random$regionID_nonzero %>% mutate(likelihood = "gamma") %>%
      mutate(RV = as.character(paste0("R", RV))) %>% ## 10/26/16: paste "R"
      mutate(effectType = "regID") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomReg)
  }
  if (!is.null(modelOutput$summary.random$season_nonzero)){
    names(modelOutput$summary.random$season_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomSeas <- modelOutput$summary.random$season_nonzero %>% mutate(likelihood = "gamma") %>%
      mutate(RV = paste0("S", RV)) %>%
      mutate(effectType = "season") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomSeas)
  }
  # 10/26/16: added error term for each observation
  if (!is.null(modelOutput$summary.random$ID_nonzero)){
    names(modelOutput$summary.random$ID_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomErr <- modelOutput$summary.random$ID_nonzero %>% mutate(likelihood = "gamma") %>%
      mutate(RV = as.character(RV)) %>%
      mutate(effectType = "error") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomErr)
  }
 
  # bind data together
  summaryStats <- summaryComplete %>%
    mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date())) %>%
    select(modCodeStr, dbCodeStr, season, exportDate, RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
  
  # export data to file
  write_csv(summaryStats, exportPath)
  
}
################################

export_summaryStats_hurdle_likString <- function(exportPath, modelOutput, rdmFxTxt, modCodeString, dbCodeString, season, likString){
  # 12/15/16 export summary statistics of INLA model output for hurdle model variables (flex non-zero likelihood string) -- fixed and random effects in the same file
  print(match.call())
  
  ## change variable names output from INLA ##
  names(modelOutput$summary.fixed) <- c("mean", "sd", "q_025", "q_5", "q_975", "mode", "kld")
  names(modelOutput$summary.hyperpar) <- names(modelOutput$summary.fixed)[1:6] # 8/17/16 add hyperpar export
  
  # clean fixed effects summary statistics output from INLA
  summaryFixed <- tbl_df(modelOutput$summary.fixed) %>%
    mutate(RV = rownames(modelOutput$summary.fixed)) %>%
    mutate(effectType = "fixed") %>%
    mutate(likelihood = ifelse(grepl("_bin", RV, fixed=TRUE), "binomial", ifelse(grepl("_nonzero", RV, fixed=TRUE), likString, NA))) %>%
    select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
  # clean hyperpar summary statistics output from INLA
  summaryHyperpar <- tbl_df(modelOutput$summary.hyperpar) %>%
    mutate(RV = rownames(modelOutput$summary.hyperpar)) %>%
    mutate(effectType = "hyperpar", kld = NA) %>%
    mutate(likelihood = ifelse(grepl("_bin", RV, fixed=TRUE), "binomial", ifelse(grepl("_nonzero", RV, fixed=TRUE), likString, NA))) %>%
    select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
  
  summaryComplete <- bind_rows(summaryFixed, summaryHyperpar)
  # clean random effects summary statistics output from INLA
  if (!is.null(modelOutput$summary.random$fips_nonzero)){
    names(modelOutput$summary.random$fips_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomFips <- modelOutput$summary.random$fips_nonzero %>% mutate(likelihood = likString) %>%
      mutate(effectType = "spatial") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomFips)
  }
  # clean structure spatial effects  summary statistics output from INLA
  if (!is.null(modelOutput$summary.random$graphIdx_nonzero)){
    names(modelOutput$summary.random$graphIdx_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomGraphid <- modelOutput$summary.random$graphIdx_nonzero %>% mutate(likelihood = likString) %>%
      mutate(RV = as.character(paste0("phi", RV))) %>%
      mutate(effectType = "structured") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomGraphid)
  }
  if (!is.null(modelOutput$summary.random$graphIdx_st_nonzero)){
    names(modelOutput$summary.random$graphIdx_st_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomGraphid2 <- modelOutput$summary.random$graphIdx_st_nonzero %>% mutate(likelihood = likString) %>%
      mutate(RV = as.character(paste0("phi", RV))) %>%
      mutate(effectType = "structured_st") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomGraphid2)
  }
  if (!is.null(modelOutput$summary.random$fips_st_nonzero)){
    names(modelOutput$summary.random$fips_st_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomSt <- modelOutput$summary.random$fips_st_nonzero %>% mutate(likelihood = likString) %>%
      mutate(effectType = "stID") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomSt)
  }
  if (!is.null(modelOutput$summary.random$regionID_nonzero)){
    names(modelOutput$summary.random$regionID_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomReg <- modelOutput$summary.random$regionID_nonzero %>% mutate(likelihood = likString) %>%
      mutate(RV = as.character(paste0("R", RV))) %>% ## 10/26/16: paste "R"
      mutate(effectType = "regID") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomReg)
  }
  if (!is.null(modelOutput$summary.random$season_nonzero)){
    names(modelOutput$summary.random$season_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomSeas <- modelOutput$summary.random$season_nonzero %>% mutate(likelihood = likString) %>%
      mutate(RV = paste0("S", RV)) %>%
      mutate(effectType = "season") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomSeas)
  }
  # 7/25/17: add seasonID AR1 term
  if (!is.null(modelOutput$summary.random$seasonID_nonzero)){
    names(modelOutput$summary.random$seasonID_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomSeas <- modelOutput$summary.random$seasonID_nonzero %>% mutate(likelihood = likString) %>%
      mutate(RV = paste0("S", RV)) %>%
      mutate(effectType = "seasonAR1") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomSeas)
  }
  # 10/26/16: added error term for each observation
  if (!is.null(modelOutput$summary.random$ID_nonzero)){
    names(modelOutput$summary.random$ID_nonzero) <- c("RV", names(modelOutput$summary.fixed))
    summaryRandomErr <- modelOutput$summary.random$ID_nonzero %>% mutate(likelihood = likString) %>%
      mutate(RV = as.character(RV)) %>%
      mutate(effectType = "error") %>%
      select(RV, effectType, likelihood, mean, sd, q_025, q_5, q_975, mode, kld)
    summaryComplete <- bind_rows(summaryComplete, summaryRandomErr)
  }
  
  # bind data together
  summaryStats <- summaryComplete %>%
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
  modOutput_fitted <- bind_cols(modDataFullOutput %>% select(fips, ID, y, y1, season), oneLik_fits) %>% 
      mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, exportDate = as.character(Sys.Date())) %>% # 10/11/16: grab season from modDataFullOutput instead of function argument
      select(modCodeStr, dbCodeStr, season, exportDate, fips, ID, mean, sd, q_025, q_5, q_975, mode, y, y1)
  
  # export data to file
  write_csv(modOutput_fitted, exportPath)
  # return modified output
  modOutput_fitted2 <- modOutput_fitted %>%
    select(-modCodeStr, -dbCodeStr, -exportDate)
  
  return(modOutput_fitted2)
}
################################

export_summaryStats_fitted_hurdle_st <- function(exportPath, oneLik_fits, modDataFullOutput, modCodeString, dbCodeString, season){
  # process binomial likelihood or gamma likelihood fitted values for diagnostic plotting
  print(match.call())
  
  names(oneLik_fits) <- c("mean", "sd", "q_025", "q_5", "q_975", "mode")
  modOutput_fitted <- bind_cols(modDataFullOutput %>% select(fips_st, ID, y, y1, season), oneLik_fits) %>% 
      mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, exportDate = as.character(Sys.Date())) %>% # 10/11/16: grab season from modDataFullOutput instead of function argument
      select(modCodeStr, dbCodeStr, season, exportDate, fips_st, ID, mean, sd, q_025, q_5, q_975, mode, y, y1)
  
  # export data to file
  write_csv(modOutput_fitted, exportPath)
  # return modified output
  modOutput_fitted2 <- modOutput_fitted %>%
    select(-modCodeStr, -dbCodeStr, -exportDate)
  
  return(modOutput_fitted2)
}
################################