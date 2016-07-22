
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


#### functions for diagnostic plots by modcode  ################################

################################

plot_diag_scatter_hurdle <- function(path_csvExport, path_plotExport_predVsObs, likelihoodString, xaxisVariable, yaxisVariable, errorbar){
  # plot scatterplot with errorbars & calculate corr coef for each season
  print(match.call())
  
  # grab list of files names
  setwd(path_csvExport)
  readfile_list <- grep(sprintf("summaryStatsFitted_%s", likelihoodString), list.files(), value = TRUE)
  plotDat <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), fips = c(), ID = c(), mean = c(), sd = c(), q_025 = c(), q_5 = c(), q_975 = c(), mode = c(), y = c()))

  # import yhat and y data
  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "cci_ccddddddd")
    plotDat <- bind_rows(plotDat, seasFile)
  }
  
  # calculate yhat residuals for gamma model only
  if (likelihoodString == "gamma"){
    plotDat <- plotDat %>%
      mutate(yhat_resid = (y-mean)/sd)
  }
  
  # calculate spearman's rho correlations
  corrDat <- plotDat %>% 
    rename_(pltVar = yaxisVariable, xVar = xaxisVariable) %>%
    group_by(season) %>%
    summarise(rho = cor(xaxisVariable, pltVar, method = "spearman", use = 'complete.obs')) %>%
    mutate(facetlabel = paste(sprintf("S%s rho", season), round(rho, 3))) %>%
    select(season, facetlabel)
  
  # create new dataset with corr coef in label
  plotDat2 <- left_join(plotDat, corrDat, by = 'season') 
  
  # plot formatting
  w <- 8; h <- 8; dp <- 250
  
  # scatterplot: predicted vs observed with errorbars
  if (errorbar){
    plotOutput <- ggplot(plotDat2, aes(x = xVar, y = pltVar, group = facetlabel)) +
      geom_pointrange(aes(ymin = q_025, ymax = q_975)) +
      facet_wrap(~facetlabel, scales = "free") +
      ylab(sprintf("%s (95% CI)", yaxisVariable)) +
      xlab(sprintf("%s", xaxisVariable)) 
  } else{
    plotOutput <- ggplot(plotDat2, aes(x = xVar, y = pltVar, group = facetlabel)) +
      facet_wrap(~facetlabel, scales = "free") +
      ylab(sprintf("%s", yaxisVariable)) +
      xlab(sprintf("%s", xaxisVariable)) 
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
  yhatDat <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), ID = c(), q_025 = c(), q_5 = c(), q_975 = c()))
  idDat <- tbl_df(data.frame(season = c(), fips = c(), ID = c()))
  
  # import yhat data
  for (infile in readfile_list){
    seasFile <- read_csv(infile, col_types = "cci_c__ddd___")
    yhatDat <- bind_rows(yhatDat, seasFile)
  }
  
  # import crosswalk between yhat IDs and fips
  for (infile in readfile_list2){
    seasFile <- read_csv(infile, col_types = "ic_i__")
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
    summarise(rho = cor(varInterest, q_5, method = "spearman", use = 'complete.obs')) %>%
    mutate(facetlabel = paste(sprintf("S%s rho", season), round(rho, 3))) %>%
    select(season, facetlabel)
  
  # create new dataset with corr coef in label
  plotDat2 <- left_join(plotDat, corrDat, by = 'season') %>%
    rename_(varInterest = eval(var))

  # plot formatting
  w <- 8; h <- 8; dp <- 250
  
  # scatterplot: predicted vs ILI counts (covnerted to county)
  plotOutput <- ggplot(plotDat2, aes(x = varInterest, y = q_5, group = facetlabel)) +
    geom_pointrange(aes(ymin = q_025, ymax = q_975)) +
    facet_wrap(~facetlabel, scales = "free") +
    ylab("yhatMedian (95%CI)") +
    xlab(var) 
  ggsave(paste0(path_plotExport_predVsRaw, var, ".png"), plotOutput, height = h, width = w, dpi = dp)
  
}

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