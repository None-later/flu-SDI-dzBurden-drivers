
## Name: Elizabeth Lee
## Date: 2/8/16
## Function: functions to export INLA results as data files and diagnostic figures -- state scale 
## Filenames: reference_data/USstate_shapefiles/gz_2010_us_040_00_500k
## Data Source: shapefile from US Census 2010 - https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(RColorBrewer); require(ggplot2)

#### functions for diagnostic plots  ################################

plot_state_choropleth <- function(exportPath, pltDat, pltVarTxt, code){
# draw state choropleth with tiers or gradient colors and export to file
  print(match.call())

  states_map <- map_data("state")
  h <- 5; w <- 8; dp <- 300
  pltDat <- pltDat %>% rename_(pltVar = pltVarTxt)

  if (code == 'tier'){
    choro <- ggplot(pltDat, aes(map_id = state)) +
      geom_map(aes(fill = pltVar), map = states_map, color = "black") +
      scale_fill_brewer(palette = "RdYlGn") +
      expand_limits(x = states_map$long, y = states_map$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
  }

  else if (code == 'gradient'){
    choro <- ggplot(pltDat, aes(map_id = state)) +
      geom_map(aes(fill = pltVar), map = states_map, color = "black") +
      scale_fill_continuous(low = "green", high = "red") +
      expand_limits(x = states_map$long, y = states_map$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") 
  }
  
  ggsave(exportPath, choro, height = h, width = w, dpi = dp)  
  
}
################################

plot_fixedFx_marginals <- function(exportPath, modelOutput, modCodeStr, s){
  # plot marginal posteriors for all fixed effect coefficients (pass entire INLA model output to function)
  print(match.call())
  
  w <- 4; h <- 4; dp <- 300
  
  names_fixedFx <- names(modelOutput$marginals.fixed)
  # standard naming system "O_samplingeffort" or "X_driver"
  names_fixedFx_cl <- grep("[OX]{1}[_]{1}", unlist(strsplit(names_fixedFx, split = "[()]")), value = TRUE)
  
  for (i in 1:length(names_fixedFx)){
    exportPath_full <- paste0(exportPath, sprintf("/inla_%s_%s_marg_S%s.png", modCodeStr, names_fixedFx_cl[i], s))
    png(exportPath_full, width = w, height = h, units = "in", res = dp)
    par(mfrow = c(1, 1))
    plot(modelOutput$marginals.fixed[[i]], xlab = paste0(names_fixedFx_cl[i], sprintf(", S%s", s)), 
         xlim = c(-1, 1), ylab = "density")
    dev.off()
  }
  
}
################################

plot_rdmFx_summary_map <- function(exportPath, pltDat, rdmFxTxt){
  # plot random effect mode as the color and precision of the distribution (1/variance) as the size of the dot on a map
  print(match.call())
  
  # plot formatting
  states_map <- map_data("state")
  h <- 5; w <- 8; dp <- 300

  # rename variables to standard names for plotting
  colorVar <- sprintf("Pred%s_mode", rdmFxTxt)
  pltDat <- pltDat %>% rename_(sizeVar_prep = sprintf("Pred%s_sd", rdmFxTxt), colorVar = colorVar)
  
  # calculate nu precision and drop unneeded variables
  pltDat2 <- pltDat %>%
    mutate(sizeVar = 1/((sizeVar_prep))) %>%
    select(fips, state, abbr, lat, lon, season, year, sizeVar, colorVar)
  # View(pltDat2)
  
  # draw map
  summary.map <- ggplot(pltDat2, aes(x = lon, y = lat)) +
    geom_point(aes(color = colorVar, size = sizeVar)) +
    scale_color_continuous(name = sprintf("Predicted %s mode", rdmFxTxt), low = "green", high = "red") +
    scale_size_continuous(name = "1/SD") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") 
  
  # save figure
  ggsave(exportPath, summary.map, height = h, width = w, dpi = dp) 
  
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
    seasFile <- read_csv(infile, col_types = "cci_cdddddd_")
    coefDf <- bind_rows(coefDf, seasFile)
  }
 
  # separate plots with random and fixed effects 
  names_fixedFx <- coefDf %>% select(RV) %>% unique %>% unlist
  names_fixedFx_cl <- grep("[OX]{1}[_]{1}", unlist(strsplit(names_fixedFx, split = "[()]")), value = TRUE)
  coefDf_fixed <- coefDf %>% filter(RV %in% names_fixedFx_cl)
  coefDf_random <- coefDf %>% filter(!(RV %in% names_fixedFx_cl))
  
  # plot fixed effects
  fixedFig <- ggplot(coefDf_fixed, aes(x = season, y = mode, group = RV)) +
    geom_pointrange(aes(ymin = q_025, ymax = q_975)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~RV, scales = "free_y") +
    ylab("coefMode (95%CI)") +
    xlim(c(1, 10))
  ggsave(paste0(path_plotExport_coefDistr, "fixed.png"), fixedFig, height = h, width = w, dpi = dp)
  
  # plot random effects
  rdmFig <- ggplot(coefDf_random, aes(x = season, y = mode, group = RV)) +
    geom_pointrange(aes(ymin = q_025, ymax = q_975)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~RV, scales = "free_y") +
    ylab("coefMode (95%CI)") +
    xlim(c(1, 10))
  ggsave(paste0(path_plotExport_coefDistr, "random.png"), rdmFig, height = h, width = w, dpi = dp)
 
}

#### functions for data processing prior to export  ################################

# process_binVariable <- function(dataset, variable, varname){
#   # for the passed variable, bin to quintiles and add color strings
#   print(match.call())
#   
#   #### plot formatting ####
#   labVec <- paste("Tier", 1:5)
#   colVec <- brewer.pal(length(labVec), 'RdYlGn')
#   
#   #### variable name formatting ####
#   bin_name <- paste0(varname, "_bin")
#   bincolor_name <- sprintf("%s_bin_color", varname)
#   colstring_name <- sprintf("%s_col_string", varname)
#   
#   #### processing ####
#   dataset2 <- dataset %>%
#     rename_(variable = variable) %>%
#     mutate_(binName = cut(variable, breaks = quantile(variable = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE, include.lowest = TRUE)) %>%
#     rename_(bin_name = binName)
#            
#   return(dataset2)
# }


#### functions for data export  ################################

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
  summaryRandom <- tbl_df(modelOutput$summary.random$ID) %>%
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
export_summaryStats_fitted <- function(exportPath, modelOutput, residualsDf, modCodeString, dbCodeString, season){
  # export summary statistics of INLA fitted values
  print(match.call())
  
  # variable name output from INLA
  names(modelOutput$summary.fitted.values) <- c("mn", "sd", "q_025", "q_5", "q_975", "mode")
  idvar <- paste0("yhat", as.character(as.numeric(substr.Right(rownames(modelOutput$summary.fitted.values), 2))))
  
  # clean summary statistics output for fitted values (yhat)
  summaryFitted_save <- tbl_df(modelOutput$summary.fitted.values) %>%
    mutate(modCodeStr = modCodeString, dbCodeStr = dbCodeString, season = season, exportDate = as.character(Sys.Date()), ID = idvar) %>%
    select(modCodeStr, dbCodeStr, season, exportDate, ID, mn, sd, q_025, q_5, q_975, mode) %>%
    mutate(logy = residualsDf$logy, logyhat_resid = residualsDf$residVec) %>%
    mutate(yhat_mn = exp(mn), yhat_sd = exp(sd), yhat_mode = exp(mode)) 
  
  summaryFitted_obj <- summaryFitted_save %>%
    mutate(ID = as.numeric(substring(ID, 5, nchar(ID))))
    
  # export data to file
  write_csv(summaryFitted_save, exportPath)
  return(summaryFitted_obj)
}

export_DIC <- function(exportPath, dicDataframe){
  # export DIC & CPO values across all seasons for a given model set
  # if cpoFail > 0, CPO calculation is invalid
  print(match.call())
  
  # parse modCodeStr
  parsed <- strsplit(modCodeStr, "_")[[1]]
  
  # clean data frame
  dicOutput <- tbl_df(dicDataframe) %>%
    mutate(modCode = parsed[1], dbMetric = parsed[2], version = parsed[3]) %>%
    select(modCodeStr, modCode, dbMetric, version, season, exportDate, DIC, CPO, cpoFail)
  
  if(dim(dicOutput)[1] == 8){
    # export data to file
    write_csv(dicOutput, exportPath)
  } else{
    # print message
    print("DICs not written to file. Fewer than 8 seasons present in DIC output dataframe.")
  }
  
}


