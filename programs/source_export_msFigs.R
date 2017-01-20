
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
import_stAbbr <- function(){
  print(match.call())
  returnDat <- read_csv(paste0(string_refData_folder(), "state_abbreviations_FIPS_region.csv"), col_types = "cc__") %>%
    mutate(State = tolower(State))
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
calculate_95CI <- function(summDat){
  # primarily for forest plots
  print(match.call())
  
  returnDat <- summDat %>%
    mutate(LB = mean-(2*sd), UB = mean+(2*sd)) %>%
    mutate(signif = ifelse(UB < 0 | LB > 0, TRUE, FALSE))
  return(returnDat)
}

################################
indicate_signif <- function(summDat){
  # primarily for choropleths indicating significance
  print(match.call())
  
  returnDat <- summDat %>%
    mutate(LB = mean-(2*sd), UB = mean+(2*sd)) %>%
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

#### plot functions ################################
################################
choro_stateEffects <- function(modCodeStr){
# draw state choropleth with indicators of group effects
  print(match.call())

  # plot formatting
  states_map <- map_data("state")
  h <- 5; w <- 8; dp <- 300
  exportFname <- paste0(string_msFig_folder(), "choro_stGroupEffects_", modCodeStr, ".png")

  # import coef data
  coefDat <- read_csv(string_coef_fname(modCodeStr), col_types = "ccd_cccddddd__") %>%
    filter(effectType == 'stID') %>%
    clean_RVnames(.) %>%
    mutate(fips = substr.Right(paste0("0", RV), 2)) %>%
    left_join(import_stAbbr(), by = "fips") 

  # prepare data for plotting
  plotDat <- indicate_signif(coefDat)

  # plot
  choro <- ggplot(plotDat, aes(map_id = State)) +
    geom_map(aes(fill = signif2), map = states_map, color = "grey50") +
    scale_fill_manual(name = "Significance", values = c("-1" = "#ca0020", "1" = "#0571b0"), breaks = c("-1", "1"), labels = c("(-)", "(+)"), na.value = "grey75") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
  
  ggsave(exportFname, choro, height = h, width = w, dpi = dp)  
}

################################
forest_coefDistr_seasEffects <- function(modCodeStr){
  print(match.call())
  
  # plot formatting
  seasLabels <- label_seas_predictors()
  exportFname <- paste0(string_msFig_folder(), "forest_coefSeas_", modCodeStr, ".png")
  plotFormats <- list(w=6, h=3)
  
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
forest_coefDistr_fixedEffects <- function(modCodeStr){
  print(match.call())
  
  # plot formatting
  ecolLabels <- label_ecol_predictors()
  ecolFname <- paste0(string_msFig_folder(), "forest_coefEcol_", modCodeStr, ".png")
  ecolFormats <- list(w=6, h=3)
  measLabels <- label_meas_predictors()
  measFname <- paste0(string_msFig_folder(), "forest_coefMeas_", modCodeStr, ".png")
  measFormats <- list(w=2.5, h=3)
  
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
    theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=45, vjust=1, hjust=1), axis.text=element_text(size=12))
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
dot_coefCompare <- function(modCodeLs, pltFormats){
  print(match.call())
  
  # plot formatting
  totLabels <- label_tot_predictors()
  w <- pltFormats$w; h <- pltFormats$h; dp <- 300
  exportFname <- paste0(string_msFig_folder(), "dot_coefCompare_", pltFormats$descrip, ".png")
  
  # grab coefData from multiple models
  fullDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), effectType = c(), likelihood = c(), mean = c(), sd = c(), q_025 = c(), q_5 = c(), q_975 = c()))
  
  for(modCode in modCodeLs){
    modDat <- read_csv(string_coef_fname(modCode), col_types = "cci_cccddddd__")
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
    scale_colour_manual("Signif", values = c("-1" = "#ca0020", "1" = "#0571b0"), breaks = c("-1", "1"), labels = c("(-)", "(+)"), na.value = "white") +
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
    scale_fill_manual(name = "", values = c("1" = "#7b3294", "0" = "grey75"), breaks = c("1", "0"), labels = c("match", "no match"), na.value = "grey75") +
    expand_limits(x = ctyMap$long, y = ctyMap$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = c(.9,.3)) +
    facet_wrap(~season, nrow=2)
  
  ggsave(exportFname, choro, height = h, width = w, dpi = dp)
}




