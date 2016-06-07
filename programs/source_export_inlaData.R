
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

#### functions for diagnostic plots  ################################

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
  
  names_fixedFx <- names(marg.fx.transf)  # standard naming system "O_samplingeffort" or "X_driver"
  
  for (i in 1:length(names_fixedFx)){
    exportPath_full <- paste0(exportPath, sprintf("/inla_%s_%s_marg_S%s.png", modCodeStr, names_fixedFx[i], s))
    png(exportPath_full, width = w, height = h, units = "in", res = dp)
    par(mfrow = c(1, 1))
    plot(marginalsFixed[[i]], xlab = paste0(names_fixedFx[i], sprintf(", S%s", s)), 
         xlim = c(-3, 3), ylab = "density")
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
  plot(fitOutput$mn, fitOutput$yhat_resid, xlim = c(floor(min(fitOutput$mn[which(!is.na(fitOutput$yhat_resid))])), ceiling(max(fitOutput$mn[which(!is.na(fitOutput$yhat_resid))]))), xlab = "Fitted value mean (yhat)", ylab = "Residuals")
  abline(h = 0)
  hist(modelOutput$cpo$pit, breaks = 10, main = "", xlab = "Probability integral transform (PIT)")
  dev.off()
  
}
################################

plot_coefDistr_season <- function(path_csvExport, path_plotExport_coefDistr){
  # plot all coef modes, Q.025 - Q0.975 over time
  print(match.call())
  
  # plot formatting
  w <- 8; h <- 8; dp <- 250
  
  # grab list of files names
  setwd(path_csvExport)
  readfile_list <- grep("summaryStats_", list.files(), value = TRUE)
  coefDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), mean = c(), sd = c(), q_025 = c(), q_5 = c(), q_975 = c(), mode = c()))

  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "cci_ccdddddd_")
    coefDf <- bind_rows(coefDf, seasFile)
  }
  
  # plot fixed effects
  fixedFig <- ggplot(coefDf %>% filter(effectType == 'fixed'), aes(x = season, y = mode, group = RV)) +
    geom_pointrange(aes(ymin = q_025, ymax = q_975)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~RV, scales = "free_y") +
    ylab("coefMode (95%CI)") +
    xlim(c(1, 10)) +
    ylim(c(-2, 2))
  ggsave(paste0(path_plotExport_coefDistr, "fixed.png"), fixedFig, height = h, width = w, dpi = dp)
  
  # plot random effects
  rdmSample <- paste0('spatial', 1:6)
  rdmFig <- ggplot(coefDf %>% filter(effectType == 'spatial' & RV %in% rdmSample), aes(x = season, y = mode, group = RV)) +
    geom_pointrange(aes(ymin = q_025, ymax = q_975)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~RV, scales = "free_y") +
    ylab("coefMode (95%CI)") +
    xlim(c(1, 10))
  ggsave(paste0(path_plotExport_coefDistr, "random.png"), rdmFig, height = h, width = w, dpi = dp)
  
  # plot effects of state ID
  stFig <- ggplot(coefDf %>% filter(effectType == 'stID'), aes(x = season, y = mode, group = RV)) +
    geom_pointrange(aes(ymin = q_025, ymax = q_975)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~RV, scales = "free_y") +
    ylab("coefMode (95%CI)") +
    xlim(c(1, 10))
  ggsave(paste0(path_plotExport_coefDistr, "stateID.png"), stFig, height = h, width = w, dpi = dp)
  
  # plot effects of state ID
  regFig <- ggplot(coefDf %>% filter(effectType == 'regID'), aes(x = season, y = mode, group = RV)) +
    geom_pointrange(aes(ymin = q_025, ymax = q_975)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~RV, scales = "free_y") +
    ylab("coefMode (95%CI)") +
    xlim(c(1, 10))
  ggsave(paste0(path_plotExport_coefDistr, "regID.png"), regFig, height = h, width = w, dpi = dp)
 
}


#### functions for data export  ################################

export_summaryStats_transformed <- function(exportPath, summListOutput, fxNames, rdmFxTxt, modCodeString, dbCodeString, season){
  # export summary statistics of INLA model output -- fixed and random effects in the same file
  print(match.call())
  
  # table formatting: assuming fixed, spatial, state ID, and region ID exist
  summ.fx.transf <- as.data.frame(matrix(unlist(summListOutput), byrow = T, ncol = 7))
  names(summ.fx.transf) <- c("mean", "sd", "q_025", "q_5", "q_975", "mode", "kld")
  RV <- c(fxNames, paste0(rdmFxTxt, 1:nrow(summListOutput[[2]])), paste0('stID', 1:nrow(summListOutput[[3]])), paste0('regID', 1:nrow(summListOutput[[4]]))) # number of random effects (spatial, state, region)
  effectType <- c(rep("fixed", length(fxNames)), rep("spatial", nrow(summListOutput[[2]])), rep("stID", nrow(summListOutput[[3]])), rep("regID", nrow(summListOutput[[4]])))

  # bind data together
  summaryStats <- summ.fx.transf %>%
    mutate(RV = RV, effectType = effectType, modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date())) %>%
    select(modCodeStr, dbCodeStr, season, exportDate, RV, effectType, mean, sd, q_025, q_5, q_975, mode, kld)
  
  # export data to file
  write_csv(summaryStats, exportPath)
  
}
################################

export_summaryStats <- function(exportPath, modelOutput, rdmFxTxt, modCodeString, dbCodeString, season){
  # export summary statistics of INLA model output -- fixed and random effects in the same file
  print(match.call())

  # variable name output from INLA
  names(modelOutput$summary.fixed) <- c("mean", "sd", "q_025", "q_5", "q_975", "mode", "kld")
  names(modelOutput$summary.random$ID) <- c("ID", names(modelOutput$summary.fixed))
  
  # clean fixed and random effects summary statistics output from INLA
  summaryFixed <- tbl_df(modelOutput$summary.fixed) %>%
    mutate(RV = rownames(mod$summary.fixed)) %>%
    select(RV, mean, sd, q_025, q_5, q_975, mode, kld)
  summaryRandomID <- tbl_df(modelOutput$summary.random$ID) %>%
    mutate(RV = paste0(rdmFxTxt, ID)) %>%
    select(RV, mean, sd, q_025, q_5, q_975, mode, kld)
  
  # bind data together
  summaryStats <- bind_rows(summaryFixed, summaryRandom) %>%
    mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date())) %>%
    select(modCodeStr, dbCodeStr, season, exportDate, RV, mean, sd, q_025, q_5, q_975, mode, kld)
  
  # export data to file
  write_csv(summaryStats, exportPath)
    
}
################################

export_summaryStats_rdmOnly <- function(exportPath, modelOutput, rdmFxTxt, modCodeString, dbCodeString, season){
  # export summary statistics of INLA model output -- only random effects in the same file
  print(match.call())
  
  # variable name output from INLA
  names(modelOutput$summary.random$ID) <- c("ID", "mean", "sd", "q_025", "q_5", "q_975", "mode", "kld")
  
  # clean random effects summary statistics output from INLA
  summaryRandom <- tbl_df(modelOutput$summary.random$ID) %>%
    mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date()), RV = paste0(rdmFxTxt, ID)) %>%
    select(modCodeStr, dbCodeStr, season, exportDate, RV, mean, sd, q_025, q_5, q_975, mode, kld)
  
  # export data to file
  write_csv(summaryRandom, exportPath)
  
}
################################

export_summaryStats_fitted <- function(exportPath, modelOutput, residualsDf, modCodeString, dbCodeString, season, digits){
  # export summary statistics of INLA fitted values
  print(match.call())
  
  # variable name output from INLA
  names(modelOutput$summary.fitted.values) <- c("mn", "sd", "q_025", "q_5", "q_975", "mode")
  # county scale: substr.Right by 4; state scale: substr.Right by 2
  idvar <- paste0("yhat", as.character(as.numeric(substr.Right(rownames(modelOutput$summary.fitted.values), digits))))
  
  # clean summary statistics output for fitted values (yhat)
  summaryFitted_save <- tbl_df(modelOutput$summary.fitted.values) %>%
    mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date()), ID = idvar) %>%
    select(modCodeStr, dbCodeStr, season, exportDate, ID, mn, sd, q_025, q_5, q_975, mode) %>%
    mutate(y = residualsDf$y, yhat_resid = residualsDf$residVec) %>%
    mutate(yhat_mn = mn, yhat_sd = sd, yhat_mode = mode) 
  
  summaryFitted_obj <- summaryFitted_save %>%
    mutate(ID = as.numeric(substring(ID, 5, nchar(ID))))
    
  # export data to file
  write_csv(summaryFitted_save, exportPath)
  return(summaryFitted_obj)
}
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


