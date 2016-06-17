
## Name: Elizabeth Lee
## Date: 6/6/16
## Function: functions to export INLA results as data files and diagnostic figures -- specific to county scale 
## Filenames: reference_data/USstate_shapefiles/gz_2010_us_040_00_500k
## Data Source: shapefile from US Census 2010 - https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(RColorBrewer); require(ggplot2); require(ggcounty); require(scales)

#### functions for diagnostic plots  ################################

plot_countyChoro <- function(exportPath, pltDat, pltVarTxt, code){
# draw state choropleth with tiers or gradient colors and export to file
  print(match.call())
  
  # plot formatting
  us <- ggcounty.us()
  gg <- us$g
  h <- 5; w <- 8; dp <- 300

  # tier choropleth
  if (code == 'tier'){
    # process data for tiers
    pltDat <- pltDat %>% rename_(pltVar = pltVarTxt) %>%
      mutate(pltVarBin = cut(pltVar, breaks = pretty_breaks(n = 5, min.n = 3)(pltVar), ordered_result = TRUE, include.lowest = TRUE)) %>%
      mutate(pltVarBin = factor(pltVarBin, levels = rev(levels(pltVarBin))))
    
    choro <- gg +
      geom_map(data = pltDat, aes(map_id = fips, fill = pltVarBin), map = us$map, color = "grey25", size = 0.2) +
      scale_fill_brewer(palette = "RdYlGn") +
      expand_limits(x = gg$long, y = gg$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
  }

  # gradient choropleth
  else if (code == 'gradient'){
    # data for gradient has minimal processing
    pltDat <- pltDat %>% rename_(pltVar = pltVarTxt)
    
    choro <- gg +
      geom_map(data = pltDat, aes(map_id = fips, fill = pltVar), map = us$map, color = "grey25", size = 0.2) +
      scale_fill_continuous(low = "#f0fff0", high = "#006400") +
      expand_limits(x = gg$long, y = gg$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") 
  }
  
  ggsave(exportPath, choro, height = h, width = w, dpi = dp)  
  
}
################################

