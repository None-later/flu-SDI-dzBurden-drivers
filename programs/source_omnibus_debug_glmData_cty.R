## Name: Elizabeth Lee
## Date: 9/26/16
## Function: functions for glm version of model
## Filenames: reference_data/USstate_shapefiles/gz_2010_us_040_00_500k
## Data Source: shapefile from US Census 2010 - https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(dplyr); require(tidyr)

#### functions for model data preparation  ################################

convert_glmModel_gamma <- function(modData_seas){
  # prepare inla data output for glm with gamma model - remove zero values from dataset
  print(match.call())
  
  modData_glm <- modData_seas %>% 
    mutate(intercept = 1) %>%
    mutate(ID = seq_along(fips)) %>% 
    mutate(regionID = as.character(regionID)) %>%
    filter(!is.na(y) & y > 0) 
  
  return(modData_glm)
}

#### functions for model data export  ################################

export_AIC <- function(exportPath, aicDataframe){
  # export AIC 
  print(match.call())
  
  # parse modCodeStr
  parsed <- strsplit(as.character(aicDataframe$modCodeStr)[1], "_")[[1]]

  # clean data frame
  aicOutput <- tbl_df(aicDataframe) %>%
    mutate(modCode = parsed[1], dbMetric = parsed[2], version = parsed[3]) %>%
    select(modCodeStr, modCode, dbMetric, version, season, exportDate, AIC, BIC, logLik, deviance, df.residual, null.deviance, df.null)

  write_csv(aicOutput, exportPath)
  
}
################################

export_summaryStats_glm <- function(exportPath, modelOutput, modCodeString, dbCodeString, season){
  # export summary statistics of GLM model output (gamma likelihood) -- fixed and random effects in the same file
  print(match.call())
  
  # clean coefficient data
  summaryStats <- tidy(modelOutput) %>%
    rename(RV = term) %>%
    mutate(effectType = ifelse((RV == "intercept"|grepl("[XO]{1}_", RV)), "fixed", ifelse(grepl("fips_st", RV), "stID", "regID"))) %>%
    mutate(likelihood = "gamma") %>%
    mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date())) %>%
    rename(mean = estimate, sd = std.error) %>%
    select(modCodeStr, dbCodeStr, season, exportDate, RV, effectType, likelihood, mean, sd, statistic, p.value)
  
  # export data to file
  write_csv(summaryStats, exportPath)
  
}
################################

export_summaryStats_fitted_glm <- function(exportPath, modelPredictions, modData_seas, modCodeString, dbCodeString, season){
  # process binomial likelihood or gamma likelihood fitted values for diagnostic plotting
  print(match.call())
  
  fitData <- tbl_df(data.frame(mean = modelPredictions$fit, sd = modelPredictions$se.fit))
  modOutput_fitted <- bind_cols(modData_seas %>% select(fips, ID, y), fitData) %>% 
    mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date())) %>%
    select(modCodeStr, dbCodeStr, season, exportDate, fips, ID, mean, sd, y)
  
  # export data to file
  write_csv(modOutput_fitted, exportPath)
  # return modified output
  modOutput_fitted2 <- modOutput_fitted %>%
    select(-modCodeStr, -dbCodeStr, -exportDate)
  
  return(modOutput_fitted2)
}
################################

importPlot_coefDistr_season_glm <- function(path_csvExport, path_plotExport_coefDistr){
  # import coefficient distributions across seasons
  print(match.call())
  
  # grab list of files names
  setwd(path_csvExport)
  readfile_list <- grep("summaryStats_", list.files(), value = TRUE)
  fullDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), effectType = c(), mean = c(), sd = c(), statistic = c(), p.value = c()))

  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "cci_cccdddd")
    fullDf <- bind_rows(fullDf, seasFile)
  }
  
  coefDf <- fullDf %>%
    mutate(LB = mean-(2*sd), UB = mean+(2*sd)) %>%
    mutate(signif = ifelse(UB < 0 | LB > 0, TRUE, FALSE))
  
  # plot fixed effects
  fxDat <- coefDf %>% filter(effectType == 'fixed') 
  plot_coefDistr_season(fxDat, path_plotExport_coefDistr, 'fixed.png')
  
  # plot effects of state ID
  if ('stID' %in% coefDf$effectType){
    stIds <- coefDf %>% filter(effectType == 'stID') %>% distinct(RV) %>% unlist 
    stIdDat <- coefDf %>% filter(effectType == 'stID') %>% mutate(RV = factor(RV, levels = stIds))
    plot_coefDistr_season(stIdDat, path_plotExport_coefDistr, 'stateID.png')
  }
  
  # plot effects of region number
  if ('regID' %in% coefDf$effectType){
    regIds <- coefDf %>% filter(effectType == 'regID') %>% distinct(RV) %>% unlist 
    regIdDat <- coefDf %>% filter(effectType == 'regID') %>% mutate(RV = factor(RV, levels = regIds))
    plot_coefDistr_season(regIdDat, path_plotExport_coefDistr, 'regionID.png')
  }
  
}
################################

plot_diag_scatter_glm <- function(path_csvExport, path_plotExport_predVsObs, likelihoodString, xaxisVariable, yaxisVariable, errorbar){
  # plot scatterplot with errorbars & calculate corr coef for each season
  print(match.call())
  
  # grab list of files names
  setwd(path_csvExport)
  readfile_list <- grep("summaryStatsFitted_", list.files(), value = TRUE)
  plotDat <- data.frame()

  # import yhat and y data
  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "cci_ciddd")
    plotDat <- bind_rows(plotDat, seasFile)
  }

  names(plotDat) <- c("modCodeStr", "dbCodeStr", "season", "fips", "ID", "mean", "sd", "y")
  
  # calculate yhat residuals for gamma model only
  if (likelihoodString == "gamma"){
    plotDat <- plotDat %>%
      mutate(y_nonzero = ifelse(y > 0, y, NA)) %>%
      mutate(yhat_resid = (y_nonzero-mean)/sd) %>%
      mutate(yhat_rawresid = (y_nonzero-mean)) %>%
      mutate(LB = mean-(sd*2), UB = mean+(sd*2))
  }
  
  # calculate spearman's rho correlations
  corrDat <- plotDat %>% 
    rename_(pltVar = yaxisVariable, xVar = xaxisVariable) %>%
    group_by(season) %>%
    summarise(rho = cor(xVar, pltVar, method = "spearman", use = 'complete.obs')) %>%
    mutate(facetlabel = paste(sprintf("S%s rho", season), round(rho, 3))) %>%
    select(season, facetlabel)
  
  # create new dataset with corr coef in label
  plotDat2 <- left_join(plotDat, corrDat, by = 'season') %>% 
    rename_(pltVar = yaxisVariable, xVar = xaxisVariable)
  
  # plot formatting
  w <- 8; h <- 8; dp <- 250
  
  # scatterplot: predicted vs observed with errorbars
  if (errorbar){
    plotOutput <- ggplot(plotDat2, aes(x = xVar, y = pltVar, group = facetlabel)) +
      geom_pointrange(aes(ymin = LB, ymax = UB)) +
      facet_wrap(~facetlabel, scales = "free") +
      scale_y_continuous(paste(yaxisVariable, "(95%CI)")) +
      xlab(xaxisVariable)
  } else{
    plotOutput <- ggplot(plotDat2, aes(x = xVar, y = pltVar, group = facetlabel)) +
      geom_point() +
      facet_wrap(~facetlabel, scales = "free") +
      ylab(yaxisVariable) +
      xlab(xaxisVariable)
  }
  
  ggsave(path_plotExport_predVsObs, plotOutput, height = h, width = w, dpi = dp)
  
}
