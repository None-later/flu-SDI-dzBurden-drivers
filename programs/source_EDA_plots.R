## Name: Elizabeth Lee
## Date: 6/3/16
## Function: Functions to automate the creation of exploratory data analysis (EDA) choropleths
# 
## Filename: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header ####################################
require(ggplot2)
require(dplyr)
require(readr)
require(RColorBrewer)
require(ggcounty)

################################################################

#### time series ################################
tsplots_cty <- function(dummyDat2, tsparams, tsplotparams){
  # function to draw county time series plots, facets by state
  # data format: year = year or season, covariate = variable name, value = covariate value, fips = county fips id, fips_st = state fips id, State = full state name, Abbreviation = state abbreviation, for.plot = each state has a different number
  
  print(match.call())

  # ts data params
  indexes <- tsparams$indexes; years <- tsparams$years; varnames <- tsparams$varnames; spatial <- tsparams$spatial; src <- tsparams$src
  # ts plot params
  num <- tsplotparams$num; h <- tsplotparams$h; w <- tsplotparams$w; dp <- tsplotparams$dp; leg.lab <- tsplotparams$leg.lab
  
  for(i in indexes){
    for(v in varnames){
      dummyplots <- ggplot(fullDat2 %>% filter(for.plot>= i & for.plot < i+num & covariate == v), aes(x=year, y=value)) +
        theme_bw()+
        theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
        geom_line(aes(colour = fips)) +
        scale_y_continuous(name = leg.lab[which(v == varnames)]) +
        scale_x_continuous(breaks = seq(years[1], years[length(years)], 1)) +
        guides(colour = "none") +
        facet_wrap(~State)
      labs <- fullDat2 %>% filter(for.plot>= i & for.plot < i+num) %>% select(Abbreviation) %>% distinct(Abbreviation) %>% arrange(Abbreviation) %>% slice(c(1, num))  %>% unlist
      ggsave(sprintf("%s_%s_%s_%s-%s.png", v, src, spatial, labs[1], labs[2]), dummyplots, width = w, height = h, dpi = dp)
    }
  } 
}

################################################################
choroplots_cty <- function(dummyDat, params, plotparams){
  # function to draw tier and gradient choropleths (all years on same figure)
  # data format: year = year or season, covariate = variable name, value = covariate value, fips = county fips id
  
  print(match.call())
  
  # import data parameters
  spatial <- params$spatial; code <- params$code; lab <- params$lab; src <- params$src; yr <- params$yr
  # import plot parameters
  h <- plotparams$h; w <- plotparams$w; dp <- plotparams$dp
  
  # county map
  us <- ggcounty.us()
  gg <- us$g
  
  # create aggregate df for all processed seasons
  pltDat <- data.frame()
  # process bin categories for each season separately
  for (y in yr){
    dummyDat2 <- dummyDat %>%
      filter(covariate == code, year == y) %>%
      mutate(vbin = cut(value, breaks = pretty_breaks(n= 6, min.n = 3)(value), ordered_result = TRUE, include.lowest = TRUE)) %>%
      mutate(vbin = factor(vbin, levels = rev(levels(vbin)))) 
    pltDat <- bind_rows(pltDat, dummyDat2)
  }
  
  choro.tier <- gg +
    geom_map(data = pltDat, aes(map_id = fips, fill = vbin, group = year), map = us$map, color = "grey25", size = 0.2) +
    scale_fill_brewer(name = lab, palette = "RdYlGn") +
    expand_limits(x = gg$long, y = gg$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
    facet_wrap(~year, nrow = 2) 
  ggsave(sprintf("%s_tiers_%s_%s.png", code, src, spatial), choro.tier, height = h, width = w, dpi = dp)
  
  choro.grad <- gg +
    geom_map(data = pltDat, aes(map_id = fips, fill = value, group = year), map = us$map, color = "black") +
    scale_fill_continuous(name = lab, low = "#f0fff0", high = "#006400") +
    expand_limits(x = gg$long, y = gg$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
    facet_wrap(~year, nrow = 2) 
  ggsave(sprintf("%s_grad_%s_%s.png", code, src, spatial), choro.grad, height = h, width = w, dpi = dp)
} 

################################################################
choroplots_cty_1yr <- function(dummyDat, params, plotparams){
  # function to draw tier and gradient choropleths (one year per figure)
  # data format: year = year or season, covariate = variable name, value = covariate value, fips = county fips id
  
  print(match.call())
  
  # import data parameters
  spatial <- params$spatial; code <- params$code; lab <- params$lab; src <- params$src; yr <- params$yr
  # import plot parameters
  h <- plotparams$h; w <- plotparams$w; dp <- plotparams$dp
  
  # county map
  us <- ggcounty.us()
  gg <- us$g
  
  # process bin categories for each season separately
  for (y in yr){
    pltDat <- dummyDat %>%
      filter(covariate == code, year == y) %>%
      mutate(vbin = cut(value, breaks = pretty_breaks(n= 6, min.n = 3)(value), ordered_result = TRUE, include.lowest = TRUE)) %>%
      mutate(vbin = factor(vbin, levels = rev(levels(vbin)))) 
  
    choro.tier <- gg +
      geom_map(data = pltDat, aes(map_id = fips, fill = vbin), map = us$map, color = "grey25", size = 0.2) +
      scale_fill_brewer(name = lab, palette = "RdYlGn", na.value = "white") +
      expand_limits(x = gg$long, y = gg$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") 
    ggsave(sprintf("%s_tiers_%s_%s_%s.png", code, src, spatial, y), choro.tier, height = h, width = w, dpi = dp)
    
    choro.grad <- gg +
      geom_map(data = pltDat, aes(map_id = fips, fill = value), map = us$map, color = "grey25", size = 0.2) +
      scale_fill_continuous(name = lab, low = "#f0fff0", high = "#006400", na.value = "white") +
      expand_limits(x = gg$long, y = gg$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
    ggsave(sprintf("%s_grad_%s_%s_%s.png", code, src, spatial, y), choro.grad, height = h, width = w, dpi = dp)
  }
} 
################################################################

choroplots_zip3_1yr <- function(fullDat, zipShapefile, params, plotparams){
  # zip3 disease burden choropleths, 1 year per figure
  print(match.call())
  
  # import data parameters
  spatial <- params$spatial; code <- params$code; lab <- params$lab; src <- params$src; seas <- params$seas
  # import plot parameters
  h <- plotparams$h; w <- plotparams$w; dp <- plotparams$dp
  
  # grab lat/lon bounds for continental US
  gg <- ggcounty.us()$g
  
  # process bin categories for each season separately
  for (s in seas){
    seasDat <- fullDat %>%
      filter(season == s)
    mergeDat <- merge(zipShapefile, seasDat, by = "id", all.x = TRUE) %>%
      mutate(vbin = cut(burden, breaks = pretty_breaks(n = 6, min.n = 3)(burden), ordered_result = TRUE, include.lowest = TRUE)) %>%
      mutate(vbin = factor(vbin, levels = rev(levels(vbin))))
    mergeDat2 <- mergeDat[order(mergeDat$order),]
    
    choro.tier <- ggplot() +
      geom_polygon(data = mergeDat2, aes(x = long, y = lat, group = group, fill = vbin), color = "grey25", size = 0.2) +
      scale_fill_brewer(name = lab, palette = "RdYlGn", na.value = "white") +
      scale_x_continuous(limits = c(-124.849, -66.885)) +
      scale_y_continuous(limits = c(24.396, 49.384)) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") 
    ggsave(sprintf("%s_tiers_%s_%s_S%s.png", code, src, spatial, s), choro.tier, height = h, width = w, dpi = dp)
    
    choro.grad <- ggplot() +
      geom_polygon(data = mergeDat2, aes(x = long, y = lat, group = group, fill = burden), color = "grey25", size = 0.2) +
      scale_fill_continuous(name = lab, low = "#f0fff0", high = "#006400", na.value = "white") +
      scale_x_continuous(limits = c(-124.849, -66.885)) +
      scale_y_continuous(limits = c(24.396, 49.384)) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") 
    ggsave(sprintf("%s_grad_%s_%s_S%s.png", code, src, spatial, s), choro.grad, height = h, width = w, dpi = dp)
  }
}
