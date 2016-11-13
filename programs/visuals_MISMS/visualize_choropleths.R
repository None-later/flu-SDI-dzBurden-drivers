
## Name: Elizabeth Lee
## Date: 9/8/16
## Function: Choropleth map of 2007-08 data for yObs -- for ISDS abstract
## Filenames: physicianCoverage_IMSHealth_state.csv, dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: IMS Health
## Notes: V3: multi-region outputs
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr) # clean_data_functions dependencies
require(RColorBrewer); require(ggplot2); require(ggcounty); require(scales); require(classInt)
setwd(dirname(sys.frame(1)$ofile))
setwd("../")
source("source_export_inlaDiagnostics.R")

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr <- "7a_iliSum_v5-1"
s <- 8

#### functions #################################
plot_countyChoro_legend <- function(exportPath, pltDat, pltVarTxt, code, zeroes){
  # draw state choropleth with tiers or gradient colors and export to file
  print(match.call())
  
  # plot formatting
  us <- ggcounty.us()
  gg <- us$g
  h <- 5; w <- 8; dp <- 300
  
  # tier choropleth
  if (code == 'tier'){
    # process data for tiers
    # 7/21/16: natural breaks w/ classIntervals
    pltDat <- pltDat %>% rename_(pltVar = pltVarTxt) 
    # create natural break intervals with jenks algorithm
    intervals <- classIntervals(pltDat$pltVar[!is.na(pltDat$pltVar)], n = 5, style = "jenks")
    if (zeroes){
      # 0s have their own color
      breaks <- sort(c(0, intervals$brks))
    } else{
      breaks <- c(intervals$brks)
    }
    breaksRound <- round(breaks, 1) 
    breakLabels <- matrix(1:(length(breaksRound)-1))
    for (i in 1:length(breakLabels)){
      # create legend labels
      breakLabels[i] <- paste0("(",as.character(breaksRound[i]), "-", as.character(breaksRound[i+1]), "]")}
    # reverse order of break labels so zeros are green and larger values are red
    breakLabels <- rev(breakLabels) 
    pltDat2 <- pltDat %>%
      mutate(pltVarBin = factor(.bincode(pltVar, breaks, right = TRUE, include.lowest = TRUE))) %>%
      mutate(pltVarBin = factor(pltVarBin, levels = rev(levels(pltVarBin))))
    
    choro <- gg +
      geom_map(data = pltDat2, aes(map_id = fips, fill = pltVarBin), map = us$map, color = "grey25", size = 0.2) +
      scale_fill_brewer("Intensity", palette = "RdYlGn", label = breakLabels, na.value = "grey60") +
      expand_limits(x = gg$long, y = gg$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") 
  
  }
  
  ggsave(exportPath, choro, height = h, width = w, dpi = dp)
}

#### import data #################################
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf("../../R_export/inlaModelData_export/%s", modCodeStr))
path_csvExport <- getwd()

path_importdata <- paste0(path_csvExport, sprintf("/summaryStatsFitted_gamma_%s.csv", modCodeStr))
dummy <- read_csv(path_importdata, col_types = cols(fips = col_character(), ID = col_character(), y1 = col_double())) %>% 
  mutate(y1_origscale = ifelse(is.na(y1), NA, y)) %>%
  mutate(mean_origscale = exp(mean))
importDat <- calculate_residuals(dummy, TRUE) %>%
  filter(season == s)

#### export plots #################################
setwd(dirname(sys.frame(1)$ofile))
setwd("../../graph_outputs/visuals_MISMS2016")
path_plotExport <- getwd()

# choropleth: observed values
path_plotExport_y1_origscale <- paste0(path_plotExport, sprintf("/MISMS_choro_expyObs_%s_S%s.png", modCodeStr, s))
plot_countyChoro_legend(path_plotExport_y1_origscale, importDat, "y1_origscale", "tier", FALSE)

# choropleth: fitted values
path_plotExport_yhat_origscale <- paste0(path_plotExport, sprintf("/MISMS_choro_expyHat_%s_S%s.png", modCodeStr, s))
plot_countyChoro_legend(path_plotExport_yhat_origscale, importDat, "mean_origscale", "tier", FALSE)


