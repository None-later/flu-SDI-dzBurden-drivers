
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
require(ggthemes)
setwd(dirname(sys.frame(1)$ofile))
source("source_export_inlaDiagnostics.R")

#### processing functions ################################
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
label_seas_predictors <- function(){
  cleanRV <- paste0("S", 3:9)
  pltLabels <- c("2002-03", "2003-04", "2004-05", "2005-06", "2006-06", "2007-08", "2008-09")
  
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
    mutate(signif = ifelse(UB < 0, "-1", ifelse(LB > 0, "1", NA)))
  return(returnDat)
}


#### plot functions ################################
################################
choro_stateEffects <- function(modCodeStr){
# draw state choropleth with indicators of group effects
  print(match.call())

  # plot formatting
  states_map <- map_data("state")
  h <- 5; w <- 8; dp <- 300
  exportFname <- paste0(string_msFig_folder(), "stGroupEffects_choro_", modCodeStr, ".png")


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
    geom_map(aes(fill = signif), map = states_map, color = "grey50") +
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
  exportFname <- paste0(string_msFig_folder(), "coefSeas_forest_", modCodeStr, ".png")
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
  ecolFname <- paste0(string_msFig_folder(), "coefEcol_forest_", modCodeStr, ".png")
  ecolFormats <- list(w=6, h=3)
  measLabels <- label_meas_predictors()
  measFname <- paste0(string_msFig_folder(), "coefMeas_forest_", modCodeStr, ".png")
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
  ecolFname <- paste0(string_msFig_folder(), "coefEcol_forest_", modCodeStr, ".png")
  ecolFormats <- list(w=6, h=6)
  measLabels <- label_meas_predictors()
  measFname <- paste0(string_msFig_folder(), "coefMeas_forest_", modCodeStr, ".png")
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

  # plot effects over time
  plotOutput <- ggplot(plotDat, aes(x = season, y = mean, group = RV)) +
    geom_pointrange(aes(ymin = LB, ymax = UB, colour = signif)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    scale_y_continuous("posterior mean (95%CI)") +
    scale_colour_manual(limits = c(TRUE, FALSE), values = c("#008837", "grey75")) +
    facet_wrap(~RV, scales = "free_y") +
    scale_x_continuous("flu season", breaks = seq(3,9,3), labels = label_seas_predictors()$pltLabs[seq(1, length(label_seas_predictors()$pltLabs), 3)]) +
    guides(colour = FALSE) +
    theme_bw() +
    theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=45, vjust=1, hjust=1), axis.text=element_text(size=12))
  ggsave(exportFname, plotOutput, height = h, width = w, dpi = dp)
}

################################
## MAIN ##
setwd(dirname(sys.frame(1)$ofile))

multiSeas_modCodeLs <- c("8a_iliSum_v2-6", "8e_epiDur_v2-3")
for (code in multiSeas_modCodeLs){	
  choro_stateEffects(code) # rerun on ryojin/bansallab
  # forest_coefDistr_seasEffects(code)
  # forest_coefDistr_fixedEffects(code) # multi-season fixed effects
}

singleSeas_modCodeLs <- c("9a_iliSum_v2-4", "9e_epiDur_v2-2")
for (code in singleSeas_modCodeLs){	
  # forest_coefDistr_fixedEffects_singleSeason(code)
}

