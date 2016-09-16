
## Name: Elizabeth Lee
## Date: 6/6/16
## Function: general functions to export INLA results as data files -- should apply to cty and st scales, but not implemented for state scale models
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(RColorBrewer); require(ggplot2)

#### functions for results plots by model  ################################

################################
plot_rdmFx_marginalsSample<- function(path_plotExport_rdmFxSample, marginalsRandomID, rdmFx_RV){
  # plot a sample of marginal posteriors for random effects
  print(match.call())
  
  w <- 6; h <- 6; dp <- 200
  
  png(path_plotExport_rdmFxSample, width = w, height = h, units = "in", res = dp)
  par(mfrow = c(3, 2))
  for (i in 1:6){
    plot(marginalsRandomID[[i]], xlab = sprintf("%s%s", rdmFx_RV, i))
  }
  dev.off()
}
################################

plot_fixedFx_marginals <- function(exportPath, marginalsFixed, modCodeStr, s){
  # plot marginal posteriors for all fixed effect coefficients (pass entire INLA model output to function)
  print(match.call())
  
  w <- 4; h <- 4; dp <- 300
  
  names_fixedFx <- names(marginalsFixed)  # standard naming system "O_samplingeffort" or "X_driver"
  
  for (i in 1:length(names_fixedFx)){
    exportPath_full <- paste0(exportPath, sprintf("/inla_%s_%s_marg_S%s.png", modCodeStr, names_fixedFx[i], s))
    png(exportPath_full, width = w, height = h, units = "in", res = dp)
    par(mfrow = c(1, 1))
    plot(marginalsFixed[[i]], xlab = paste0(names_fixedFx[i], sprintf(", S%s", s)), 
         xlim = c(-4, 4), ylab = "density")
    dev.off()
  }
  
}
################################

plot_fit_diagnostics <- function(path_plotExport_diagnostics, fitOutput, modelOutput){
  # plot pooled residuals and PIT, model fit diagnostics
  print(match.call())
  
  w <- 6; h <- 3; dp <- 200
  
  png(path_plotExport_diagnostics, width = w, height = h, units = "in", res = dp)
  par(mfrow = c(1, 2))
  plot(fitOutput$yhat_mn, fitOutput$yhat_resid, xlim = c(floor(min(fitOutput$yhat_mn[which(!is.na(fitOutput$yhat_resid))])), ceiling(max(fitOutput$yhat_mn[which(!is.na(fitOutput$yhat_resid))]))), xlab = "Fitted value mean (yhat)", ylab = "Residuals")
  abline(h = 0)
  hist(modelOutput$cpo$pit, breaks = 10, main = "", xlab = "Probability integral transform (PIT)")
  dev.off()
  
}


#### functions for results plots by modcode  ################################

################################

importPlot_coefDistr_season <- function(path_csvExport, path_plotExport_coefDistr){
  # import coefficient distributions across seasons
  print(match.call())
  
  # grab list of files names
  setwd(path_csvExport)
  readfile_list <- grep("summaryStats_", list.files(), value = TRUE)
  coefDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), effectType = c(), mean = c(), sd = c(), q_025 = c(), q_5 = c(), q_975 = c()))
  
  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "cci_ccddddd__")
    coefDf <- bind_rows(coefDf, seasFile)
  }
  
  # plot fixed effects
  fxDat <- coefDf %>% filter(effectType == 'fixed') 
  plot_coefDistr_season(fxDat, path_plotExport_coefDistr, 'fixed.png')
  
  # plot random effects
  sampleLs <- coefDf %>% filter(effectType == 'spatial') %>% select(RV) %>% sample_n(9) %>% unlist
  rdmDat <- coefDf %>% filter(effectType == 'spatial' & RV %in% sampleLs) 
  plot_coefDistr_season(rdmDat, path_plotExport_coefDistr, 'random.png')
  
  # plot effects of state ID
  stIds <- coefDf %>% filter(effectType == 'stID') %>% distinct(RV) %>% unlist 
  stIdDat <- coefDf %>% filter(effectType == 'stID') %>% mutate(RV = factor(RV, levels = stIds))
  plot_coefDistr_season(stIdDat, path_plotExport_coefDistr, 'stateID.png')
  
  # plot effects of state ID
  regIds <- coefDf %>% filter(effectType == 'regID') %>% distinct(RV) %>% unlist 
  regIdDat <- coefDf %>% filter(effectType == 'regID') %>% mutate(RV = factor(RV, levels = regIds))
  plot_coefDistr_season(regIdDat, path_plotExport_coefDistr, 'regionID.png')
}
################################

importPlot_coefDistr_season_transformed <- function(path_csvExport, path_plotExport_coefDistr){
  # import coefficient distributions across seasons, transformed back to original scale
  print(match.call())
  
  # grab list of files names
  setwd(path_csvExport)
  readfile_list <- grep("summaryStats_", list.files(), value = TRUE)
  fullDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), effectType = c(), mean = c(), sd = c(), q_025 = c(), q_25 = c(), q_5 = c(), q_75 = c(), q_975 = c()))

  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "cci_ccddddddd")
    fullDf <- bind_rows(coefDf, seasFile)
  }

  coefDf <- fullDf %>%
  mutate(LB = mean-(2*sd), UB = mean+(2*sd)) %>%
    mutate(signif = ifelse(UB < 0 | LB > 0, TRUE, FALSE))

  # plot fixed effects
  fxDat <- coefDf %>% filter(effectType == 'fixed') 
  plot_coefDistr_season(fxDat, path_plotExport_coefDistr, 'fixed.png')
  
  # plot random effects
  sampleLs <- coefDf %>% filter(effectType == 'spatial') %>% select(RV) %>% sample_n(9) %>% unlist
  rdmDat <- coefDf %>% filter(effectType == 'spatial' & RV %in% sampleLs) 
  plot_coefDistr_season(rdmDat, path_plotExport_coefDistr, 'random.png')
  
  # plot effects of state ID
  stIds <- coefDf %>% filter(effectType == 'stID') %>% distinct(RV) %>% unlist 
  stIdDat <- coefDf %>% filter(effectType == 'stID') %>% mutate(RV = factor(RV, levels = stIds))
  plot_coefDistr_season(stIdDat, path_plotExport_coefDistr, 'stateID.png')
  
  # plot effects of state ID
  regIds <- coefDf %>% filter(effectType == 'regID') %>% distinct(RV) %>% unlist 
  regIdDat <- coefDf %>% filter(effectType == 'regID') %>% mutate(RV = factor(RV, levels = regIds))
  plot_coefDistr_season(regIdDat, path_plotExport_coefDistr, 'regionID.png')
}
################################

plot_coefDistr_season <- function(plotDat, path_plotExport_coefDistr, plotFilename){
  # plot all coef mean & 95%CI over time
  print(match.call())
  
  # plot formatting
  w <- 8; h <- 8; dp <- 250

  # plot fixed effects
  plotOutput <- ggplot(plotDat, aes(x = season, y = mean, group = RV)) +
    geom_pointrange(aes(ymin = LB, ymax = UB, colour = signif)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~RV, scales = "free_y") +
    scale_y_continuous("coefMean (95%CI)") +
    xlim(c(1, 10)) +
    guides(colour = FALSE) +
    theme_bw()
  ggsave(paste0(path_plotExport_coefDistr, plotFilename), plotOutput, height = h, width = w, dpi = dp)
  
}
################################



#### functions for data export  ################################

################################
export_DIC <- function(exportPath, dicDataframe){
  # 6/7/16: export DIC & CPO values for one season (instead of all seasons)
  # if cpoFail > 0, CPO calculation is invalid
  print(match.call())
  
  # parse modCodeStr
  parsed <- strsplit(modCodeStr, "_")[[1]]
  
  # clean data frame
  dicOutput <- tbl_df(dicDataframe) %>%
    mutate(modCode = parsed[1], dbMetric = parsed[2], version = parsed[3]) %>%
    select(modCodeStr, modCode, dbMetric, version, season, exportDate, DIC, CPO, cpoFail)
  
  write_csv(dicOutput, exportPath)
  
}
################################

export_ids <- function(exportPath, modDataFullOutput){
  # export random and state/region group effect ids with true identities, as a key
  print(match.call())
  
  ids <- modDataFullOutput %>% 
    select(season, fips, county, ID, st, regionID)
  
  # export data to file
  write_csv(ids, exportPath)
  
}
################################

export_summaryStats_fitted <- function(exportPath, modelOutput, modDataFullOutput, modCodeString, dbCodeString, season){
  # export summary statistics of INLA fitted values
  print(match.call())
  
  # variable name output from INLA
  names(modelOutput$summary.fitted.values) <- c("mean", "sd", "q_025", "q_5", "q_975", "mode")
  modOutput_fitted <- bind_cols(modDataFullOutput %>% select(fips, ID, y), modelOutput$summary.fitted.values) %>%
    mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date())) %>%
    select(modCodeStr, dbCodeStr, season, exportDate, fips, ID, mean, sd, q_025, q_5, q_975, mode, y)

  # export data to file
  write_csv(modOutput_fitted, exportPath)
  # return modified output
  modOutput_fitted2 <- modOutput_fitted %>%
    select(-modCodeStr, -dbCodeStr, -exportDate)
  
  return(modOutput_fitted2)
}
# ################################ # 7/20/16 rm??
# 
# export_summaryStats_rdmOnly <- function(exportPath, modelOutput, rdmFxTxt, modCodeString, dbCodeString, season){
#   # export summary statistics of INLA model output -- only random effects in the same file
#   print(match.call())
#   
#   # variable name output from INLA
#   names(modelOutput$summary.random$ID) <- c("ID", "mean", "sd", "q_025", "q_5", "q_975", "mode", "kld")
#   
#   # clean random effects summary statistics output from INLA
#   summaryRandom <- tbl_df(modelOutput$summary.random$ID) %>%
#     mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date()), RV = paste0(rdmFxTxt, ID)) %>%
#     select(modCodeStr, dbCodeStr, season, exportDate, RV, mean, sd, q_025, q_5, q_975, mode, kld)
#   
#   # export data to file
#   write_csv(summaryRandom, exportPath)
#   
# }
################################

export_summaryStats_transformed <- function(exportPath, summListOutput, fxNames, rdmFxTxt, modCodeString, dbCodeString, season){
  # export summary statistics of INLA model output -- fixed and random effects in the same file
  print(match.call())
  
  # table formatting: assuming fixed, spatial, state ID, and region ID exist
  summ.fx.transf <- as.data.frame(do.call(rbind, summListOutput))
  names(summ.fx.transf) <- c("mean", "sd", "q_025", "q_25", "q_5", "q_75", "q_975")
  RV <- c(fxNames, paste0(rdmFxTxt, 1:nrow(summListOutput[[2]])), paste0('stID', 1:nrow(summListOutput[[3]])), paste0('regID', 1:nrow(summListOutput[[4]]))) # number of random effects (spatial, state, region)
  effectType <- c(rep("fixed", length(fxNames)), rep("spatial", nrow(summListOutput[[2]])), rep("stID", nrow(summListOutput[[3]])), rep("regID", nrow(summListOutput[[4]])))
  
  # bind data together
  summaryStats <- summ.fx.transf %>%
    mutate(RV = RV, effectType = effectType, modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date())) %>%
    select(modCodeStr, dbCodeStr, season, exportDate, RV, effectType, mean, sd, q_025, q_25, q_5, q_75, q_975)
  
  # export data to file
  write_csv(summaryStats, exportPath)
  
}
################################

export_summaryStats <- function(exportPath, modelOutput, rdmFxTxt, modCodeString, dbCodeString, season){
  # export summary statistics of INLA model output -- fixed and random effects in the same file
  print(match.call())
  
  # variable name output from INLA
  names(modelOutput$summary.fixed) <- c("mean", "sd", "q_025", "q_5", "q_975", "mode", "kld")
  names(modelOutput$summary.random$fips) <- c("ID", names(modelOutput$summary.fixed))
  names(modelOutput$summary.random$fips_st) <- names(modelOutput$summary.random$fips)
  names(modelOutput$summary.random$regionID) <- names(modelOutput$summary.random$fips)
  names(modelOutput$summary.hyperpar) <- names(modelOutput$summary.fixed)[1:6] # 8/17/16 add hyperpar export
  
  # clean fixed effects summary statistics output from INLA
  summaryFixed <- tbl_df(modelOutput$summary.fixed) %>%
    mutate(RV = rownames(modelOutput$summary.fixed)) %>%
    mutate(effectType = "fixed") %>%
    select(RV, effectType, mean, sd, q_025, q_5, q_975, mode, kld)
  # clean hyperpar summary statistics output from INLA
  summaryHyperpar <- tbl_df(modelOutput$summary.hyperpar) %>%
    mutate(RV = rownames(modelOutput$summary.hyperpar)) %>%
    mutate(effectType = "hyperpar", kld = NA) %>%
    select(RV, effectType, mean, sd, q_025, q_5, q_975, mode, kld)
  
  # clean random effects summary statistics output from INLA
  summaryRandomFips <- tbl_df(modelOutput$summary.random$fips) %>%
    mutate(effectType = "spatial") 
  summaryRandomSt <- tbl_df(modelOutput$summary.random$fips_st) %>%
    mutate(effectType = "stID") 
  summaryRandomReg <- tbl_df(modelOutput$summary.random$regionID) %>%
    mutate(ID = as.character(ID)) %>%
    mutate(effectType = "regID") 
  summaryRandom <- bind_rows(summaryRandomFips, summaryRandomSt, summaryRandomReg) %>%
    rename(RV = ID) %>%
    select(RV, effectType, mean, sd, q_025, q_5, q_975, mode, kld)
  
  # bind data together
  summaryStats <- bind_rows(summaryFixed, summaryRandom, summaryHyperpar) %>%
    mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date())) %>%
    select(modCodeStr, dbCodeStr, season, exportDate, RV, effectType, mean, sd, q_025, q_5, q_975, mode, kld)
  
  # export data to file
  write_csv(summaryStats, exportPath)
  
}
################################

