
## Name: Elizabeth Lee
## Date: 6/17/16
## Function: general functions to generate INLA diagnostic plots
## Filenames: 
## Data Source: 
## Notes: need to SSH into snow server
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(RColorBrewer); require(ggplot2); require(dplyr); require(tidyr)
require(DBI); require(RMySQL) # read tables from mysql database
require(ggthemes)


#### functions for diagnostic plots by modcode  ################################

################################

plot_diag_scatter_hurdle <- function(path_csvExport, path_plotExport_scatter, likelihoodString, xaxisVariable, yaxisVariable, errorbar){
  # plot scatterplot with errorbars & calculate corr coef for each season
  print(match.call())
  
  #### import fitted values ####
  setwd(path_csvExport)
  readfile_list <- grep(sprintf("summaryStatsFitted_%s", likelihoodString), list.files(), value = TRUE)
  fitDat <- tbl_df(data.frame())

  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "ccd_ccdddddddd")
    fitDat <- bind_rows(fitDat, seasFile)
  }
  names(fitDat) <- c("modCodeStr", "dbCodeStr", "season", "fips", "ID", "mean", "sd", "q_025", "q_5", "q_975", "mode", "y", "y1")
  
  #### import id crosswalk ####
  readfile_list2 <- grep("ids_", list.files(), value = TRUE)
  idDat <- tbl_df(data.frame())
  
  for (infile2 in readfile_list2){
    seasFile2 <- read_csv(infile2, col_types = cols_only(season = "d", fips = "c", st = "c", regionID = "d"))
    idDat <- bind_rows(idDat, seasFile2)
  }
  
  #### merge data ####
  plotDat <- left_join(fitDat, idDat, by = c("season", "fips")) %>%
    mutate(season = as.factor(as.integer(season))) %>%
    mutate(regionID = as.factor(as.integer(regionID)))

  #### clean data ####
  # calculate yhat residuals for gamma model only
  if (likelihoodString %in% c("gamma", "poisson")){
    plotDat <- calculate_residuals(plotDat, TRUE)
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
    plotDat3 <- plotDat2 %>% filter(!is.na(pltVar) & !is.na(xVar))
    plotOutput <- ggplot(plotDat3, aes(x = xVar, y = pltVar, group = facetlabel)) +
      geom_pointrange(aes(ymin = q_025, ymax = q_975, colour = regionID), alpha = 0.3) +
      facet_wrap(~facetlabel, scales = "free") +
      scale_y_continuous(paste(yaxisVariable, "(95%CI)")) +
      xlab(xaxisVariable) +
      theme(legend.position = "bottom")
  } else{
    plotDat3 <- plotDat2 %>% filter(!is.na(pltVar) & !is.na(xVar))
    plotOutput <- ggplot(plotDat3, aes(x = xVar, y = pltVar, group = facetlabel)) +
      geom_point(aes(colour = regionID), alpha = 0.3) +
      facet_wrap(~facetlabel, scales = "free") +
      ylab(yaxisVariable) +
      xlab(xaxisVariable) +
      theme(legend.position = "bottom")
  }
  
  ggsave(path_plotExport_scatter, plotOutput, height = h, width = w, dpi = dp)
  
}
################################

plot_diag_scatter_hurdle_spatiotemporal <- function(path_csvExport, path_plotExport_scatter, likelihoodString, xaxisVariable, yaxisVariable, errorbar){
  # plot scatterplot with errorbars & calculate corr coef for each season
  print(match.call())
  
  #### import fitted values ####
  setwd(path_csvExport)
  readfile_list <- grep(sprintf("summaryStatsFitted_%s", likelihoodString), list.files(), value = TRUE)
  fitDat <- tbl_df(data.frame())

  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "ccd_ccdddddddd")
    fitDat <- bind_rows(fitDat, seasFile)
  }
  names(fitDat) <- c("modCodeStr", "dbCodeStr", "season", "fips", "ID", "mean", "sd", "q_025", "q_5", "q_975", "mode", "y", "y1")
  
  #### import id crosswalk ####
  readfile_list2 <- grep("ids_", list.files(), value = TRUE)
  idDat <- tbl_df(data.frame())
  
  for (infile2 in readfile_list2){
    seasFile2 <- read_csv(infile2, col_types = cols_only(season = "d", fips = "c", st = "c", regionID = "d"))
    idDat <- bind_rows(idDat, seasFile2)
  }
  
  #### merge data ####
  plotDat <- left_join(fitDat, idDat, by = c("season", "fips")) %>%
    mutate(season = as.factor(as.integer(season))) %>%
    mutate(regionID = as.factor(as.integer(regionID)))
  
  #### clean data ####
  # calculate yhat residuals for nonzero model only
  if (likelihoodString %in% c("gamma", "normal", "poisson")){
    plotDat <- calculate_residuals(plotDat, TRUE)
  }
  
  # calculate spearman's rho correlations
  corrLab <- plotDat %>% 
    rename_(pltVar = yaxisVariable, xVar = xaxisVariable) %>%
    summarise(rho = cor(xVar, pltVar, method = "spearman", use = 'complete.obs')) %>%
    mutate(facetlabel = paste("spatiotemporal rho", round(rho, 3))) %>%
    select(facetlabel) 

  # create new dataset with new varnames
  plotDat2 <- plotDat %>% 
    rename_(pltVar = yaxisVariable, xVar = xaxisVariable)
  
  # plot formatting
  w <- 7; h <- 5; dp <- 250
  
  # scatterplot: predicted vs observed with errorbars
  if (errorbar){
    plotDat3 <- plotDat2 %>% filter(!is.na(pltVar) & !is.na(xVar))
    plotOutput <- ggplot(plotDat3, aes(x = xVar, y = pltVar)) +
      geom_pointrange(aes(ymin = q_025, ymax = q_975, colour = season), alpha = 0.3) +
      scale_y_continuous(paste(yaxisVariable, "(95%CI)")) +
      xlab(xaxisVariable) +
      theme(legend.position = "bottom") +
      facet_wrap(~season, nrow = 2, scales = "free") +
      ggtitle(as.character(corrLab$facetlabel))
    
  } else{
    plotDat3 <- plotDat2 %>% filter(!is.na(pltVar) & !is.na(xVar))
    plotOutput <- ggplot(plotDat3, aes(x = xVar, y = pltVar)) +
      geom_point(aes(colour = season), alpha = 0.3) +
      ylab(yaxisVariable) +
      xlab(xaxisVariable) +
      theme(legend.position = "bottom") +
      facet_wrap(~season, nrow = 2, scales = "free") +
      ggtitle(as.character(corrLab$facetlabel))
  }
  
  ggsave(path_plotExport_scatter, plotOutput, height = h, width = w, dpi = dp)
  
}
################################

importPlot_correlogram <- function(path_csvExport, path_plotExport_correlogram, path_list, likelihoodString){
  # plot correlogram of residual spatial autocorrelation vs. distance
  print(match.call())

  #### import fitted values ####
  setwd(path_csvExport)
  readfile_list <- grep(sprintf("summaryStatsFitted_%s", likelihoodString), list.files(), value = TRUE)
  fitDat <- tbl_df(data.frame())

  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "ccd_ccdddddddd")
    fitDat <- bind_rows(fitDat, seasFile)
  }
  names(fitDat) <- c("modCodeStr", "dbCodeStr", "season", "fips", "ID", "mean", "sd", "q_025", "q_5", "q_975", "mode", "y", "y1")

  seasons <- fitDat %>% distinct(season) %>% unlist

  #### import lat/lon by fips ####
  respDat <- cleanR_iliSum_cty(path_list) %>%
    distinct(fips, lat, lon)

  fullDat <- calculate_residuals(fitDat, TRUE) %>%
    left_join(respDat, by = "fips") %>%
    mutate(season = paste0("S", season)) %>%
    select(season, fips, lat, lon, yhat_resid)

  #### plot correlogram ####
  # plot formatting
  w <- 4; h <- 4; dp <- 250

  #### clean data ####
  # plot by season because of memory limitations
  residDf <- spread(fullDat, season, yhat_resid)
  resids <- data.matrix(residDf %>% select(-fips, -lat, -lon))
  
  for (i in 1:length(seasons)){
    correlogram <- correlog(x=residDf$lon, y=residDf$lat, z=resids[,i], increment=2, resamp = 1, latlon = TRUE, na.rm = TRUE, quiet = FALSE)
    exportPath <- paste0(path_plotExport_correlogram, "_S", seasons[i], ".png")
    png(exportPath, width = w, height = h, units = "in", res = dp)
    plot(correlogram$mean.of.class, correlogram$correlation, xlim = c(0,200), ylim = c(-3,3))
    dev.off()
  }
}
################################

importPlot_diag_scatter_predictors_spatiotemporal <- function(path_csvExport, path_plotExport_scatter, likelihoodString, yaxisVariable, modelDat){
  # import data and call scatterplot with fitted values/residuals vs. predictors
  print(match.call())
  
  #### import fitted values ####
  setwd(path_csvExport)
  readfile_list <- grep(sprintf("summaryStatsFitted_%s", likelihoodString), list.files(), value = TRUE)
  fitDat <- tbl_df(data.frame())
  
  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "ccd_ccdddddddd")
    fitDat <- bind_rows(fitDat, seasFile)
  }
  names(fitDat) <- c("modCodeStr", "dbCodeStr", "season", "fips", "ID", "mean", "sd", "q_025", "q_5", "q_975", "mode", "y", "y1")
  
  #### import id crosswalk ####
  readfile_list2 <- grep("ids_", list.files(), value = TRUE)
  idDat <- tbl_df(data.frame())
  
  for (infile2 in readfile_list2){
    seasFile2 <- read_csv(infile2, col_types = cols_only(season = "d", fips = "c", st = "c", regionID = "d"))
    idDat <- bind_rows(idDat, seasFile2)
  }
  
  #### grab only predictor data ####
  modDat_clean <- modelDat %>%
    select(fips, season, pop, matches("[XO]_"))
  
  #### merge data ####
  plotDat <- left_join(fitDat, idDat, by = c("season", "fips")) %>%
    left_join(modDat_clean, by = c("season", "fips")) %>%
    mutate(season = as.factor(as.integer(season))) %>%
    mutate(regionID = as.factor(as.integer(regionID)))
  
  #### clean data ####
  # calculate yhat residuals for gamma model only
  if (grepl("gamma", likelihoodString) | grepl("poisson", likelihoodString)){
    plotDat <- calculate_residuals(plotDat, TRUE)
  }
  # list of varnames
  varnames <- c(grep("[XO]_", names(plotDat), value = TRUE), "pop")

  for (i in 1:length(varnames)){
    var <- gsub("r?[XO]_", "", varnames[i])
    path_plotExport_predictor <- paste0(path_plotExport_scatter, var, "_", likelihoodString, ".png")
    plot_diag_scatter_predictors_spatiotemporal(plotDat, varnames[i], yaxisVariable, path_plotExport_predictor)
    print(paste("exported", path_plotExport_predictor))
  }

}
################################

plot_diag_scatter_predictors_spatiotemporal <- function(plotDat, xaxisVariable, yaxisVariable, path_plotExport_predictor){
  
  # formatting for scatterplot with fitted values/residuals vs. predictors
  print(match.call())
  
  # calculate spearman's rho correlations
  corrLab <- plotDat %>% 
    rename_(yVar = yaxisVariable, xVar = xaxisVariable) %>%
    summarise(rho = cor(xVar, yVar, method = "spearman", use = 'complete.obs')) %>%
    mutate(facetlabel = paste("spearman rho", round(rho, 3))) %>%
    select(facetlabel) 

  # create new dataset with new varnames
  plotDat2 <- plotDat %>% 
    rename_(yVar = yaxisVariable, xVar = xaxisVariable)
  
  # plot formatting
  w <- 6; h <- 6; dp <- 250
  
  # scatterplot: fitted/residual vs predictor data 
  plotOutput <- ggplot(plotDat2, aes(x = xVar, y = yVar)) +
    geom_point() +
    ylab(yaxisVariable) +
    xlab(xaxisVariable) +
    facet_wrap(~season) +
    theme_bw() +
    ggtitle(as.character(corrLab$facetlabel))
  ggsave(path_plotExport_predictor, plotOutput, height = h, width = w, dpi = dp)

  }
################################

importPlot_diag_scatter_ctyerrors_spatiotemporal <- function(path_csvExport, path_plotExport_scatter, likelihoodString){
  # import data and call scatterplot with fitted values/residuals vs. county iid errors
  print(match.call())
  
  #### import fitted values ####
  setwd(path_csvExport)
  fitfile_list <- grep(sprintf("summaryStatsFitted_%s", likelihoodString), list.files(), value = TRUE)
  fitDat <- tbl_df(data.frame())
  
  for (infile in fitfile_list){
    seasFile <- read_csv(infile, col_types = "ccd_ccdddddddd")
    fitDat <- bind_rows(fitDat, seasFile)
  }
  names(fitDat) <- c("modCodeStr", "dbCodeStr", "season", "fips", "ID", "mean", "sd", "q_025", "q_5", "q_975", "mode", "y", "y1")
  
  #### import error values ####
  coeffile_list <- grep("summaryStats_", list.files(), value = TRUE)
  coefDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), effectType = c(), likelihood = c(), mean = c(), sd = c(), q_025 = c(), q_5 = c(), q_975 = c()))
  
  for (infile in coeffile_list){
    seasFile <- read_csv(infile, col_types = "ccd_cccddddd__")
    coefDf <- bind_rows(coefDf, seasFile)
  }
  
  #### import id crosswalk ####
  readfile_list2 <- grep("ids_", list.files(), value = TRUE)
  idDat <- tbl_df(data.frame())
  
  for (infile2 in readfile_list2){
    seasFile2 <- read_csv(infile2, col_types = cols_only(season = "d", fips = "c", st = "c", regionID = "d"))
    idDat <- bind_rows(idDat, seasFile2)
  }

  #### clean fitted data ####
  fitDat2 <- calculate_residuals(fitDat, TRUE)
  fitDat_clean <- fitDat2 %>%
    rename(fit_mn = mean, fit_sd = sd, fit_LB = LB, fit_UB = UB) %>%
    select(season, fips, fit_mn, fit_sd, fit_LB, fit_UB, y, y1, yhat_resid, yhat_rawresid)
  
  #### clean coefficient data ####
  coefDf_clean <- coefDf %>%
    filter(likelihood == likelihoodString & effectType == "spatial") %>%
    rename(fips = RV) %>%
    mutate(error_mn = exp(mean), error_sd = exp(sd)) %>%
    select(fips, error_mn, error_sd) %>%
    mutate(error_LB = error_mn - (2*error_sd), error_UB = error_mn + (2*error_sd))

  #### merge data ####
  plotDat <- left_join(fitDat_clean, coefDf_clean, by = "fips") %>%
    left_join(idDat, by = c("season", "fips")) %>%
    mutate(season = as.factor(as.integer(season))) %>%
    mutate(regionID = as.factor(as.integer(regionID)))

  modCodeStr <- coefDf$modCodeStr[1]
  #### plot data ####
  
  # pred vs. error, color by season
  path_plotExport_fitted <- paste0(path_plotExport_scatter, "ctyerrorVsPred_seas", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_scatter_errors_spatiotemporal(plotDat, "fit_mn", "season", c(TRUE, "fit_LB", "fit_UB"), path_plotExport_fitted)
  print(paste("exported", path_plotExport_fitted))
  # pred vs. error, color by region
  path_plotExport_fitted2 <- paste0(path_plotExport_scatter, "ctyerrorVsPred_reg", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_scatter_errors_spatiotemporal(plotDat, "fit_mn", "regionID", c(TRUE, "fit_LB", "fit_UB"), path_plotExport_fitted2)
  print(paste("exported", path_plotExport_fitted2))
  
  # y_nonzero vs. error, color by season
  path_plotExport_obs <- paste0(path_plotExport_scatter, "ctyerrorVsObs_seas", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_scatter_errors_spatiotemporal(plotDat, "y1", "season", c(FALSE), path_plotExport_obs)
  print(paste("exported", path_plotExport_obs))
  # y_nonzero vs. error, color by region
  path_plotExport_obs2 <- paste0(path_plotExport_scatter, "ctyerrorVsObs_reg", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_scatter_errors_spatiotemporal(plotDat, "y1", "regionID", c(FALSE), path_plotExport_obs2)
  print(paste("exported", path_plotExport_obs2))
  
  # raw resid vs. error, color by season
  path_plotExport_rawresid <- paste0(path_plotExport_scatter, "ctyerrorVsRawresid_seas", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_scatter_errors_spatiotemporal(plotDat, "yhat_rawresid", "season", c(FALSE), path_plotExport_rawresid)
  print(paste("exported", path_plotExport_rawresid))
  # raw resid vs. error, color by region
  path_plotExport_rawresid2 <- paste0(path_plotExport_scatter, "ctyerrorVsRawresid_reg", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_scatter_errors_spatiotemporal(plotDat, "yhat_rawresid", "regionID", c(FALSE), path_plotExport_rawresid2)
  print(paste("exported", path_plotExport_rawresid2))
  
  # std resid vs. error, color by season
  path_plotExport_resid <- paste0(path_plotExport_scatter, "ctyerrorVsResid_seas", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_scatter_errors_spatiotemporal(plotDat, "yhat_resid", "season", c(FALSE), path_plotExport_resid)
  print(paste("exported", path_plotExport_resid))
  # std resid vs. error, color by region
  path_plotExport_resid2 <- paste0(path_plotExport_scatter, "ctyerrorVsResid_reg", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_scatter_errors_spatiotemporal(plotDat, "yhat_resid", "regionID", c(FALSE), path_plotExport_resid2)
  print(paste("exported", path_plotExport_resid2))
  
}
################################

importPlot_diag_scatter_errors_spatiotemporal <- function(path_csvExport, path_plotExport_scatter, likelihoodString){
  # import data and call scatterplot with fitted values/residuals vs. observation level iid error terms
  print(match.call())
  
  #### import fitted values ####
  setwd(path_csvExport)
  fitfile_list <- grep(sprintf("summaryStatsFitted_%s", likelihoodString), list.files(), value = TRUE)
  fitDat <- tbl_df(data.frame())
  
  for (infile in fitfile_list){
    seasFile <- read_csv(infile, col_types = "ccd_ccdddddddd")
    fitDat <- bind_rows(fitDat, seasFile)
  }
  names(fitDat) <- c("modCodeStr", "dbCodeStr", "season", "fips", "ID", "mean", "sd", "q_025", "q_5", "q_975", "mode", "y", "y1")
  
  #### import error values ####
  coeffile_list <- grep("summaryStats_", list.files(), value = TRUE)
  coefDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), effectType = c(), likelihood = c(), mean = c(), sd = c(), q_025 = c(), q_5 = c(), q_975 = c()))
  
  for (infile in coeffile_list){
    seasFile <- read_csv(infile, col_types = "ccd_cccddddd__")
    coefDf <- bind_rows(coefDf, seasFile)
  }
  
  #### import id crosswalk ####
  readfile_list2 <- grep("ids_", list.files(), value = TRUE)
  idDat <- tbl_df(data.frame())
  
  for (infile2 in readfile_list2){
    seasFile2 <- read_csv(infile2, col_types = cols_only(season = "d", fips = "c", st = "c", regionID = "d", ID = "c"))
    idDat <- bind_rows(idDat, seasFile2)
  }

  #### clean fitted data ####
  fitDat2 <- calculate_residuals(fitDat, TRUE)
  fitDat_clean <- fitDat2 %>%
    rename(fit_mn = mean, fit_sd = sd, fit_LB = LB, fit_UB = UB) %>%
    select(season, fips, fit_mn, fit_sd, fit_LB, fit_UB, y, y1, yhat_resid, yhat_rawresid)
  
  #### clean coefficient data ####
  coefDf_clean <- coefDf %>%
    filter(likelihood == likelihoodString & effectType == "error") %>%
    rename(ID = RV) %>%
    select(-season) %>%
    left_join(idDat, by = c("ID")) %>%
    mutate(error_mn = exp(mean), error_sd = exp(sd)) %>%
    select(fips, season, st, regionID, error_mn, error_sd) %>%
    mutate(error_LB = error_mn - (2*error_sd), error_UB = error_mn + (2*error_sd))

  #### merge data ####
  plotDat <- left_join(fitDat_clean, coefDf_clean, by = c("fips", "season")) %>%
    mutate(season = as.factor(as.integer(season))) %>%
    mutate(regionID = as.factor(as.integer(regionID)))

  modCodeStr <- coefDf$modCodeStr[1]
  #### plot data ####
  
  # pred vs. error, color by season
  path_plotExport_fitted <- paste0(path_plotExport_scatter, "errorVsPred_seas", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_scatter_errors_spatiotemporal(plotDat, "fit_mn", "season", c(TRUE, "fit_LB", "fit_UB"), path_plotExport_fitted)
  print(paste("exported", path_plotExport_fitted))
  # pred vs. error, color by region
  path_plotExport_fitted2 <- paste0(path_plotExport_scatter, "errorVsPred_reg", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_scatter_errors_spatiotemporal(plotDat, "fit_mn", "regionID", c(TRUE, "fit_LB", "fit_UB"), path_plotExport_fitted2)
  print(paste("exported", path_plotExport_fitted2))
  
  # y_nonzero vs. error, color by season
  path_plotExport_obs <- paste0(path_plotExport_scatter, "errorVsObs_seas", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_scatter_errors_spatiotemporal(plotDat, "y1", "season", c(FALSE), path_plotExport_obs)
  print(paste("exported", path_plotExport_obs))
  # y_nonzero vs. error, color by region
  path_plotExport_obs2 <- paste0(path_plotExport_scatter, "errorVsObs_reg", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_scatter_errors_spatiotemporal(plotDat, "y1", "regionID", c(FALSE), path_plotExport_obs2)
  print(paste("exported", path_plotExport_obs2))
  
  # raw resid vs. error, color by season
  path_plotExport_rawresid <- paste0(path_plotExport_scatter, "errorVsRawresid_seas", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_scatter_errors_spatiotemporal(plotDat, "yhat_rawresid", "season", c(FALSE), path_plotExport_rawresid)
  print(paste("exported", path_plotExport_rawresid))
  # raw resid vs. error, color by region
  path_plotExport_rawresid2 <- paste0(path_plotExport_scatter, "errorVsRawresid_reg", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_scatter_errors_spatiotemporal(plotDat, "yhat_rawresid", "regionID", c(FALSE), path_plotExport_rawresid2)
  print(paste("exported", path_plotExport_rawresid2))
  
  # std resid vs. error, color by season
  path_plotExport_resid <- paste0(path_plotExport_scatter, "errorVsResid_seas", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_scatter_errors_spatiotemporal(plotDat, "yhat_resid", "season", c(FALSE), path_plotExport_resid)
  print(paste("exported", path_plotExport_resid))
  # std resid vs. error, color by region
  path_plotExport_resid2 <- paste0(path_plotExport_scatter, "errorVsResid_reg", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_scatter_errors_spatiotemporal(plotDat, "yhat_resid", "regionID", c(FALSE), path_plotExport_resid2)
  print(paste("exported", path_plotExport_resid2))
  
}
################################

plot_diag_scatter_errors_spatiotemporal <- function(plotDat, xaxisVariable, colorVariable, xErrorbar, path_plotExport_errors){
  
  # formatting for scatterplot with fitted values/residuals vs. error terms
  print(match.call())
  
  # calculate spearman's rho correlations
  corrLab <- plotDat %>% 
    rename_(xVar = xaxisVariable) %>%
    summarise(rho = cor(error_mn, xVar, method = "spearman", use = 'complete.obs')) %>%
    mutate(facetlabel = paste("spearman rho", round(rho, 3))) %>%
    select(facetlabel) 
  
  # plot formatting
  w <- 6; h <- 6; dp <- 250
  
  # scatterplot: fitted/residual vs predictor data
  if (xErrorbar[1]==TRUE){
    # create new dataset with new varnames
    plotDat2 <- plotDat %>% 
      rename_(xVar = xaxisVariable, cVar = colorVariable, xLB = xErrorbar[2], xUB = xErrorbar[3])
    # plot
    plotOutput <- ggplot(plotDat2, aes(x = xVar, y = error_mn)) +
      geom_point(aes(colour = cVar), alpha = 0.3) +
      geom_errorbarh(aes(xmin = xLB, xmax = xUB, colour = cVar), alpha = 0.3) +
      geom_errorbar(aes(ymin = error_LB, ymax = error_UB, colour = cVar), alpha = 0.3) +
      xlab(xaxisVariable) +
      ylab("error mean (exponentiated)") +
      theme_bw() +
      ggtitle(as.character(corrLab$facetlabel))
  } else{
    # create new dataset with new varnames
    plotDat2 <- plotDat %>% 
      rename_(xVar = xaxisVariable, cVar = colorVariable)
    # plot
    plotOutput <- ggplot(plotDat2, aes(x = xVar, y = error_mn)) +
      geom_point(aes(colour = cVar), alpha = 0.3) +
      geom_errorbar(aes(ymin = error_LB, ymax = error_UB, colour = cVar), alpha = 0.3) +
      xlab(xaxisVariable) +
      ylab("error mean (exponentiated)") +
      theme_bw() +
      ggtitle(as.character(corrLab$facetlabel))
  }
  
  ggsave(path_plotExport_errors, plotOutput, height = h, width = w, dpi = dp)
  
}
################################

importPlot_diag_index_spatiotemporal <- function(path_csvExport, path_plotExport_scatter, likelihoodString){
  # import data and call scatterplot with fitted values/residuals vs. predictors
  print(match.call())
  
  #### import fitted values ####
  setwd(path_csvExport)
  fitfile_list <- grep(sprintf("summaryStatsFitted_%s", likelihoodString), list.files(), value = TRUE)
  fitDat <- tbl_df(data.frame())
  
  for (infile in fitfile_list){
    seasFile <- read_csv(infile, col_types = "ccd_ccdddddddd")
    fitDat <- bind_rows(fitDat, seasFile)
  }
  names(fitDat) <- c("modCodeStr", "dbCodeStr", "season", "fips", "ID", "mean", "sd", "q_025", "q_5", "q_975", "mode", "y", "y1")
  
  #### import error values ####
  coeffile_list <- grep("summaryStats_", list.files(), value = TRUE)
  coefDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), effectType = c(), likelihood = c(), mean = c(), sd = c(), q_025 = c(), q_5 = c(), q_975 = c()))
  
  for (infile in coeffile_list){
    seasFile <- read_csv(infile, col_types = "ccd_cccddddd__")
    coefDf <- bind_rows(coefDf, seasFile)
  }
  
  #### import id crosswalk ####
  readfile_list2 <- grep("ids_", list.files(), value = TRUE)
  idDat <- tbl_df(data.frame())
  
  for (infile2 in readfile_list2){
    seasFile2 <- read_csv(infile2, col_types = cols_only(season = "d", fips = "c", st = "c", regionID = "d", ID = "c"))
    idDat <- bind_rows(idDat, seasFile2)
  }
  
#### clean fitted data ####
  fitDat2 <- calculate_residuals(fitDat, TRUE)
  fitDat_clean <- fitDat2 %>%
    rename(fit_mn = mean, fit_sd = sd, fit_LB = LB, fit_UB = UB) %>%
    select(season, fips, fit_mn, fit_sd, fit_LB, fit_UB, y, y1, yhat_resid, yhat_rawresid)
  
  #### clean coefficient data ####
  coefDf_clean <- coefDf %>%
    filter(likelihood == likelihoodString & effectType == "error") %>%
    select(-season) %>%
    rename(ID = RV) %>%
    left_join(idDat, by = c("ID")) %>%
    mutate(error_mn = exp(mean), error_sd = exp(sd)) %>%
    select(fips, season, st, regionID, error_mn, error_sd) %>%
    mutate(error_LB = error_mn - (2*error_sd), error_UB = error_mn + (2*error_sd))

  #### merge data ####
  plotDat <- left_join(fitDat_clean, coefDf_clean, by = c("fips", "season")) %>%
    mutate(season = as.factor(as.integer(season))) %>%
    mutate(regionID = as.factor(as.integer(regionID)))
  
  modCodeStr <- coefDf$modCodeStr[1]
  #### plot data ####
  
  # county error term vs. index, color by region
  path_plotExport_error <- paste0(path_plotExport_scatter, "errorVsIndex_reg", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_index_spatiotemporal(plotDat, "error_mn", "regionID", c(TRUE, "error_LB", "error_UB"), FALSE, path_plotExport_error)
  print(paste("exported", path_plotExport_error))
  # county error term vs. index, color by region -- randomize order
  path_plotExport_errorR <- paste0(path_plotExport_scatter, "errorVsIndex_rdm", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_index_spatiotemporal(plotDat, "error_mn", "regionID", c(TRUE, "error_LB", "error_UB"), TRUE, path_plotExport_errorR)
  print(paste("exported", path_plotExport_errorR))
  
  # raw resid vs. index, color by region
  path_plotExport_rawresid <- paste0(path_plotExport_scatter, "rawresidVsIndex_reg", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_index_spatiotemporal(plotDat, "yhat_rawresid", "regionID", c(FALSE), FALSE, path_plotExport_rawresid)
  print(paste("exported", path_plotExport_rawresid))
  # raw resid vs. index, color by region -- randomize order
  path_plotExport_rawresidR <- paste0(path_plotExport_scatter, "rawresidVsIndex_rdm", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_index_spatiotemporal(plotDat, "yhat_rawresid", "regionID", c(FALSE), TRUE, path_plotExport_rawresidR)
  print(paste("exported", path_plotExport_rawresidR))
  
  # std resid vs. index, color by region
  path_plotExport_resid2 <- paste0(path_plotExport_scatter, "residVsIndex_reg", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_index_spatiotemporal(plotDat, "yhat_resid", "regionID", c(FALSE), FALSE, path_plotExport_resid2)
  print(paste("exported", path_plotExport_resid2))
  # std resid vs. index, color by region -- randomize order
  path_plotExport_resid2R <- paste0(path_plotExport_scatter, "residVsIndex_rdm", "_", likelihoodString, "_", modCodeStr, ".png")
  plot_diag_index_spatiotemporal(plotDat, "yhat_resid", "regionID", c(FALSE), TRUE, path_plotExport_resid2R)
  print(paste("exported", path_plotExport_resid2R))
  
}
################################

plot_diag_index_spatiotemporal <- function(plotDat, yaxisVariable, colorVariable, yErrorbar, randomOrder, path_plotExport_index){
  
  # formatting for scatterplot with fitted values/residuals vs. error terms
  print(match.call())
  
  # plot formatting
  w <- 9; h <- 4; dp <- 250
  
  # randomize order
  if(randomOrder==TRUE){
    plotDat <- plotDat %>% mutate(fips = as.factor(runif(nrow(plotDat))))
  }
  
  # scatterplot: fitted/residual vs index to see if there are structures
  if (yErrorbar[1]==TRUE){
    # create new dataset with new varnames
    plotDat2 <- plotDat %>% 
      rename_(yVar = yaxisVariable, cVar = colorVariable, yLB = yErrorbar[2], yUB = yErrorbar[3])
    # plot
    plotOutput <- ggplot(plotDat2, aes(x = fips, y = yVar)) +
      geom_point(aes(colour = cVar), alpha = 0.3) +
      geom_errorbar(aes(ymin = yLB, ymax = yUB, colour = cVar), alpha = 0.3) +
      xlab("county") +
      ylab(yaxisVariable) +
      scale_colour_tableau() +
      theme_bw() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom")
  } else{
    # create new dataset with new varnames
    plotDat2 <- plotDat %>% 
      rename_(yVar = yaxisVariable, cVar = colorVariable)
    # plot
    plotOutput <- ggplot(plotDat2, aes(x = fips, y = yVar)) +
      geom_point(aes(colour = cVar), alpha = 0.3) +
      xlab("county") +
      ylab(yaxisVariable) +
      scale_colour_tableau() +
      theme_bw() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom")
  }
  
  ggsave(path_plotExport_index, plotOutput, height = h, width = w, dpi = dp)
  
}
################################

importPlot_diag_data_distribution <- function(path_csvExport, path_plotExport_distribution, likelihoodString, modelDat){
  # import data and call histogram of distributions of fitted values, residuals, and predictors
  print(match.call())
  
  #### import fitted values ####
  setwd(path_csvExport)
  readfile_list <- grep(sprintf("summaryStatsFitted_%s", likelihoodString), list.files(), value = TRUE)
  fitDat <- tbl_df(data.frame())
  
  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "ccd_ccdddddddd")
    fitDat <- bind_rows(fitDat, seasFile)
  }
  names(fitDat) <- c("modCodeStr", "dbCodeStr", "season", "fips", "ID", "mean", "sd", "q_025", "q_5", "q_975", "mode", "y", "y1")
  
  #### grab only predictor data ####
  modDat_clean <- modelDat %>%
    select(fips, season, matches("[XO]_"))
  
  #### merge data ####
  plotDat <- left_join(fitDat, modDat_clean, by = c("season", "fips")) %>%
    mutate(season = as.factor(as.integer(season))) 
  
  #### clean data ####
  # calculate yhat residuals for gamma model only
  if (grepl("gamma", likelihoodString) | grepl("poisson", likelihoodString)){
    plotDat <- calculate_residuals(plotDat, TRUE)
  }
  # list of varnames
  varnames <- grep("[XO]_", names(plotDat), value = TRUE)
  resnames <- c("y1", "yhat_resid", "yhat_rawresid")
  allnames <- c(varnames, resnames)
  
  for (i in 1:length(allnames)){
    var <- gsub("r?[XO]_", "", allnames[i])
    path_plotExport_distribution_full <- paste0(path_plotExport_distribution, var, "_", likelihoodString, ".png")
    plot_diag_data_distribution(plotDat, allnames[i], path_plotExport_distribution_full)
    print(paste("exported", path_plotExport_distribution_full))
  }
  
}
################################

plot_diag_data_distribution <- function(plotDat, xaxisVariable, path_plotExport_distribution){
  
  # formatting for data distributions
  print(match.call())
  
  # create new dataset with new varnames
  plotDat2 <- plotDat %>% 
    rename_(xVar = xaxisVariable)
  
  # plot formatting
  w <- 4; h <- 4; dp <- 250
  
  # scatterplot: fitted/residual vs predictor data 
  plotOutput <- ggplot(plotDat2, aes(x = xVar)) +
    geom_histogram(bins=nrow(plotDat2)/10) +
    xlab(xaxisVariable) +
    theme_bw() 
  ggsave(path_plotExport_distribution, plotOutput, height = h, width = w, dpi = dp)
  
}
################################

plot_diag_scatter <- function(path_csvExport, path_plotExport_predVsObs, xaxisVariable, yaxisVariable, errorbar){
  # plot scatterplot & calculate corr coef for each season
  print(match.call())
  
  # grab list of files names
  setwd(path_csvExport)
  readfile_list <- grep("summaryStatsFitted_", list.files(), value = TRUE)
  plotDat <- tbl_df(data.frame())

  # import yhat and y data
  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "cci_ccddddddd")
    plotDat <- bind_rows(plotDat, seasFile)
  }
  # rename columns
  names(plotDat) <- c("modCodeStr", "dbCodeStr", "season", "fips", "ID", "mean", "sd", "q_025", "q_5", "q_975", "mode", "y")
  
  # calculate yhat residuals 
  plotDat <- calculate_residuals(plotDat, FALSE)

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
################################

plot_diag_predVsObs <- function(path_csvExport, path_plotExport_predVsObs){
  # plot yhat vs y & calculate corr coef for each season
  print(match.call())
  
  # grab list of files names
  setwd(path_csvExport)
  readfile_list <- grep("summaryStatsFitted_", list.files(), value = TRUE)
  plotDat <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), ID = c(), q_025 = c(), q_5 = c(), q_975 = c(), y = c()))
  
  # import yhat and y data
  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "cci_c__ddd_d_")
    plotDat <- bind_rows(plotDat, seasFile)
  }
  
  # calculate spearman's rho correlations
  corrDat <- plotDat %>% 
    group_by(season) %>%
    summarise(rho = cor(y, q_5, method = "spearman", use = 'complete.obs')) %>%
    mutate(facetlabel = paste(sprintf("S%s rho", season), round(rho, 3))) %>%
    select(season, facetlabel)
  
  # create new dataset with corr coef in label
  plotDat2 <- left_join(plotDat, corrDat, by = 'season') %>%
    rename(yobs = y)
  
  # plot formatting
  w <- 8; h <- 8; dp <- 250
  
  # scatterplot: predicted vs observed
  plotOutput <- ggplot(plotDat2, aes(x = yobs, y = q_5, group = facetlabel)) +
    geom_pointrange(aes(ymin = q_025, ymax = q_975)) +
    facet_wrap(~facetlabel, scales = "free") +
    ylab("yhatMedian (95%CI)") +
    xlab("yObs") 
  ggsave(path_plotExport_predVsObs, plotOutput, height = h, width = w, dpi = dp)
  
}
################################

plot_diag_predVsObs_transformed <- function(path_csvExport, path_plotExport_predVsObs){
  # plot yhat vs y & calculate corr coef for each season, transformed data back to original scale
  print(match.call())
  
  # grab list of files names
  setwd(path_csvExport)
  readfile_list <- grep("summaryStatsFitted_", list.files(), value = TRUE)
  plotDat <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), ID = c(), q_5 = c(), q_025 = c(), q_975 = c(), y = c()))
  
  # import yhat and y data
  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "cci_cd_ddd_d_")
    plotDat <- bind_rows(plotDat, seasFile)
  }
  
  # calculate spearman's rho correlations
  corrDat <- plotDat %>% 
    group_by(season) %>%
    summarise(rho = cor(y, q_5, method = "spearman", use = 'complete.obs')) %>%
    mutate(facetlabel = paste(sprintf("S%s rho", season), round(rho, 3))) %>%
    select(season, facetlabel)
  
  # create new dataset with corr coef in label
  plotDat2 <- left_join(plotDat, corrDat, by = 'season') %>%
    rename(yobs = y)
  
  # plot formatting
  w <- 8; h <- 8; dp <- 250
  
  # scatterplot: predicted vs observed
  plotOutput <- ggplot(plotDat2, aes(x = yobs, y = q_5, group = facetlabel)) +
    geom_pointrange(aes(ymin = q_025, ymax = q_975)) +
    facet_wrap(~facetlabel, scales = "free") +
    ylab("yhatMedian (95%CI)") +
    xlab("yObs") 
  ggsave(path_plotExport_predVsObs, plotOutput, height = h, width = w, dpi = dp)
  
}
################################ 

importPlot_diag_predVsRaw <- function(path_csvExport, path_plotExport_predVsRaw, filepathList){
  # import data and plot yhat vs raw ili counts & calculate corr coef for each season, when no transformations have been performed after modeling
  print(match.call())
  
  # grab list of files names for yhat & yhatID-fips crosswalk
  setwd(path_csvExport)
  readfile_list <- grep("summaryStatsFitted_", list.files(), value = TRUE)
  readfile_list2 <- grep("ids_", list.files(), value = TRUE)
  yhatDat <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), fips = c(), ID = c(), mean = c(), sd = c(), y = c()))

  # import yhat data
  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "cci_cidd____d")
    yhatDat <- bind_rows(yhatDat, seasFile)
  }
  
  # clean yhat data 
  yhatDat2 <- yhatDat %>%
    mutate(LB = mean-(sd*2), UB = mean+(sd*2))
  
  # import county ili case data & create ili rate
  plotDat <- clean_rawILI_cty(filepathList) %>%
    full_join(yhatDat2, by = c("fips", "season")) %>%
    mutate(iliPer10K = ili/pop*10000)
  
  plot_diag_predVsRaw(plotDat, path_plotExport_predVsRaw, "ili")
  plot_diag_predVsRaw(plotDat, path_plotExport_predVsRaw, "iliPer10K")
  
}
################################ 

importPlot_diag_predVsRaw_transformed <- function(path_csvExport, path_plotExport_predVsRaw, filepathList){
  # import data and plot yhat vs raw ili counts & calculate corr coef for each season, when data has been transformed back to original scale
  print(match.call())
  
  # grab list of files names for yhat & yhatID-fips crosswalk
  setwd(path_csvExport)
  readfile_list <- grep("summaryStatsFitted_", list.files(), value = TRUE)
  readfile_list2 <- grep("ids_", list.files(), value = TRUE)
  yhatDat <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), ID = c(), q_5 = c(), q_025 = c(), q_975 = c()))
  idDat <- tbl_df(data.frame(season = c(), fips = c(), ID = c()))
  
  # import yhat data
  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "cci_cd_ddd___")
    yhatDat <- bind_rows(yhatDat, seasFile)
  }
  
  # import crosswalk between yhat IDs and fips
  for (infile in readfile_list2){
    seasFile <- read_csv(infile, col_types = "ic__i___")
    idDat <- bind_rows(idDat, seasFile)
  }
  
  # clean yhat data & merge IDs
  yhatDat2 <- yhatDat %>%
    mutate(ID = as.numeric(substring(ID, 5, nchar(ID)))) %>%
    left_join(idDat, by = c("season", "ID"))
  
  # import county ili case data & create ili rate
  plotDat <- clean_rawILI_cty(filepathList) %>%
    full_join(yhatDat2, by = c("fips", "season")) %>%
    mutate(iliPer10K = ili/pop*10000)
  
  plot_diag_predVsRaw(plotDat, path_plotExport_predVsRaw, "ili")
  plot_diag_predVsRaw(plotDat, path_plotExport_predVsRaw, "iliPer10K")
  
}
################################ 

plot_diag_predVsRaw <- function(plotDat, path_plotExport_predVsRaw, var){
  # plot yhat vs raw ili counts or ili per pop & calculate corr coef for each season
  print(match.call())
  
  # calculate spearman's rho correlations
  corrDat <- plotDat %>% 
    rename_(varInterest = eval(var)) %>%
    group_by(season) %>%
    summarise(rho = cor(varInterest, mean, method = "spearman", use = 'complete.obs')) %>%
    mutate(facetlabel = paste(sprintf("S%s rho", season), round(rho, 3))) %>%
    select(season, facetlabel)
  
  # create new dataset with corr coef in label
  plotDat2 <- left_join(plotDat, corrDat, by = 'season') %>%
    rename_(varInterest = eval(var))

  # plot formatting
  w <- 8; h <- 8; dp <- 250
  
  # scatterplot: predicted vs ILI counts (covnerted to county)
  plotOutput <- ggplot(plotDat2, aes(x = varInterest, y = mean, group = facetlabel)) +
    geom_pointrange(aes(ymin = LB, ymax = UB)) +
    facet_wrap(~facetlabel, scales = "free") +
    ylab("yhatMean (95%CI)") +
    xlab(var) 
  ggsave(paste0(path_plotExport_predVsRaw, var, ".png"), plotOutput, height = h, width = w, dpi = dp)
  
}
################################

importPlot_coefDistr_RV_spatiotemporal <- function(path_csvExport, path_plotExport_coefDistr){
  # 10/11/16: import coefficient distributions across random variables
  print(match.call())
  
  # grab list of files names
  setwd(path_csvExport)
  readfile_list <- grep("summaryStats_", list.files(), value = TRUE)
  fullDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), effectType = c(), likelihood = c(), mean = c(), sd = c(), q_025 = c(), q_5 = c(), q_975 = c()))
  
  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "ccd_cccddddd__")
    fullDf <- bind_rows(fullDf, seasFile)
  }
  
  coefDf <- fullDf %>%
    mutate(LB = mean-(2*sd), UB = mean+(2*sd)) %>%
    mutate(signif = ifelse(UB < 0 | LB > 0, TRUE, FALSE)) %>%
    filter(!grepl("intercept", RV)) 
  
  # separate plots for data from each likelihood
  likelihoods <- coefDf %>% filter(!is.na(likelihood)) %>% distinct(likelihood) %>% unlist
  for (lik in likelihoods){
    coefDf_lik <- coefDf %>% filter(likelihood == lik)

    # plot fixed effects
    fxDat <- coefDf_lik %>% filter(effectType == 'fixed') %>% clean_RVnames(.)
    plot_coefDistr_RV(fxDat, path_plotExport_coefDistr, sprintf('fixed_%sLikelihood.png', lik))
    
    # plot fixed effects (surveillance)
    ODat <- coefDf_lik %>% filter(effectType == 'fixed' & grepl("O_", RV)) %>% clean_RVnames(.)
    plot_coefDistr_RV(ODat, path_plotExport_coefDistr, sprintf('fixedSurveil_%sLikelihood.png', lik))
    
    # plot fixed effects (ecological)
    XDat <- coefDf_lik %>% filter(effectType == 'fixed' & grepl("X_", RV)) %>% clean_RVnames(.)
    plot_coefDistr_RV(XDat, path_plotExport_coefDistr, sprintf('fixedEcol_%sLikelihood.png', lik))
    
    # plot random effects
    if (nrow(coefDf_lik %>% filter(effectType == 'spatial')) > 0){
      sampleLs <- coefDf_lik %>% filter(effectType == 'spatial') %>% select(RV) %>% sample_n(56) %>% unlist
      rdmDat <- coefDf_lik %>% filter(effectType == 'spatial' & RV %in% sampleLs) %>% clean_RVnames(.)
      plot_coefDistr_RV(rdmDat, path_plotExport_coefDistr, sprintf('random_%sLikelihood.png', lik))
    }
    
    # plot effects of state ID
    if (nrow(coefDf_lik %>% filter(effectType == 'stID')) > 0){
      stIds <- coefDf_lik %>% filter(effectType == 'stID') %>% distinct(RV) %>% unlist 
      stIdDat <- coefDf_lik %>% filter(effectType == 'stID') %>% clean_RVnames(.) %>% mutate(RV = factor(RV, levels = stIds)) 
      plot_coefDistr_RV(stIdDat, path_plotExport_coefDistr, sprintf('stateID_%sLikelihood.png', lik))
    }
    
    # plot effects of region ID
    if (nrow(coefDf_lik %>% filter(effectType == 'regID')) > 0){
      regIds <- coefDf_lik %>% filter(effectType == 'regID') %>% distinct(RV) %>% unlist 
      regIdDat <- coefDf_lik %>% filter(effectType == 'regID') %>% clean_RVnames(.) %>% mutate(RV = factor(RV, levels = regIds))
      plot_coefDistr_RV(regIdDat, path_plotExport_coefDistr, sprintf('regionID_%sLikelihood.png', lik))
    }

    # plot effects of season
    if (nrow(coefDf_lik %>% filter(effectType == 'season')) > 0){
      seasIds <- coefDf_lik %>% filter(effectType == 'season') %>% distinct(RV) %>% unlist 
      seasDat <- coefDf_lik %>% filter(effectType == 'season') %>% clean_RVnames(.) %>% mutate(RV = factor(RV, levels = seasIds))
      plot_coefDistr_RV(seasDat, path_plotExport_coefDistr, sprintf('season_%sLikelihood.png', lik))
    }
    
    # plot effects of county spatial structure
    if (nrow(coefDf_lik %>% filter(effectType == 'structured')) > 0){
      sampleStruc <- coefDf_lik %>% filter(effectType == 'structured') %>% clean_RVnames(.) %>% select(RV) %>% sample_n(56) %>% unlist 
      strucDat <- coefDf_lik %>% clean_RVnames(.) %>% filter(effectType == 'structured' & RV %in% sampleStruc) 
      plot_coefDistr_RV(strucDat, path_plotExport_coefDistr, sprintf('structured_%sLikelihood.png', lik))
    }

    # plot effects of state spatial structure
    if (nrow(coefDf_lik %>% filter(effectType == 'structured_st')) > 0){
      sampleStruc2 <- coefDf_lik %>% filter(effectType == 'structured_st') %>% clean_RVnames(.) %>% select(RV) %>% unlist 
      strucDat2 <- coefDf_lik %>% clean_RVnames(.) %>% filter(effectType == 'structured_st' & RV %in% sampleStruc2) 
      plot_coefDistr_RV(strucDat2, path_plotExport_coefDistr, sprintf('structured_st_%sLikelihood.png', lik))
    }
  } 
}
################################


#### functions for data processing  ################################

################################ 

clean_rawILI_zip3 <- function(){
  # clean raw ILI counts at zip3 level, exported from mysql
  print(match.call())
  
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-sdi")
  dbListTables(con)
  
  dbListFields(con, "flu")
  # sel.statement <- "Select * from flu limit 5"
  sel.statement <- "SELECT WEEK AS week, PATIENT_ZIP3 AS zip3, ILI_m AS ili FROM flu WHERE AGEGROUP = 'TOTAL' AND SERVICE_PLACE = 'TOTAL' AND (MONTH(WEEK) >= 11 OR MONTH(WEEK) <= 4) AND WEEK >= '2001-11-01' AND WEEK <= '2009-04-30'"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)
  
  output <- tbl_df(dummy) %>%
    mutate(season = ifelse(as.numeric(substring(week, 6, 7)) <= 4, as.integer(substring(week, 3, 4)), as.integer(substring(week, 3, 4)) + 1)) %>%
    group_by(season, zip3) %>%
    summarise(ili = sum(ili, na.rm=TRUE))
  
  return(output)
  
}
################################ 

clean_rawILI_cty <- function(filepathList){
  # clean raw ILI counts to county level
  print(match.call())
  
  # spatial crosswalk: fips, zip3, proportion (of overlap in zip3 & fips population)
  cw <- cw_zip3_cty()
  # pop data: fips, county, st, season, year, pop, lat lon
  pop_data <- clean_pop_cty(filepathList)
  
  # import raw ILI data: season, zip3, ili
  zip3Dat <- clean_rawILI_zip3() %>%
    full_join(cw, by = "zip3")
  
  # convert to county level data
  return_data <- left_join(zip3Dat, pop_data, by = c("season", "fips")) %>%
    group_by(fips, season) %>%
    summarise(ili = weighted.mean(ili, proportion, na.rm = TRUE), pop = first(pop)) %>%
    ungroup %>%
    filter(season != 1) 
  
  return(return_data)
  
}
################################

clean_RVnames <- function(dat){
  # remove RV addenda for plotting
  print(match.call())
  
  return(dat %>% 
           mutate(RV = gsub("_nonzero", "", RV)) %>%
           mutate(RV = gsub("_bin", "", RV)) %>%
           mutate(RV = gsub("X_", "", RV)) %>%
           mutate(RV = gsub("O_", "", RV)) %>%
           mutate(RV = gsub("phi", "", RV))
         )
}

