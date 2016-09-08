
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

#### set these! #################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"

#### functions #################################
plot_countyChoro_modified <- function(exportPath, pltDat, pltVarTxt, code, zeroes){
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
      scale_fill_brewer("Seasonal Flu Intensity", palette = "RdYlGn", label = breakLabels, na.value = "grey60") +
      expand_limits(x = gg$long, y = gg$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") 
  
  }
  
  ggsave(exportPath, choro, height = h, width = w, dpi = dp)
}

#### program #################################
# import data 
setwd(dirname(sys.frame(1)$ofile))
setwd("../../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))

iliSum_data <- read_csv(path_response_cty, col_types = "icllcd") %>%
  filter(metric == "ilinDt.sum") %>%
  select(-metric) %>%
  rename(y = burden) 

# plot
setwd(dirname(sys.frame(1)$ofile))
setwd("../../graph_outputs/visuals_ISDS")
path_export <- paste0(getwd(),"/yObs_S8.png")

dat_S8 <- iliSum_data %>% 
  filter(season == 8)

plot_countyChoro_modified(path_export, dat_S8, "y", "tier", TRUE)
# exported 9/8/16

