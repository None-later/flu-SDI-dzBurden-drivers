## Name: Elizabeth Lee
## Date: 11/23/15
## Function: EDA - Census 2000 & 2010 population and population density - choropleth, county-level; time series by county grouped by state
## Filename: SG_covariate_data/Cleaned_Data/clean_Census_popdensity_county.csv
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")
rm(list = ls())
#### header ####################################
require(ggplot2)
require(dplyr)
require(readr)
require(RColorBrewer)
require(ggcounty)

setwd(dirname(sys.frame(1)$ofile))
source("source_clean_data_functions.R")

#### plot formatting ################################
labVec <- paste("Tier", 1:5)
colVec <- brewer.pal(length(labVec), 'RdYlGn')
h <- 5; w <- 8; dp <- 300
h2 <- 12; w2 <- 12
mar <- rep(0, 4)
par(mar = mar, oma = mar)
years <- 2002:2009
num <- 6

# set choropleth parameters
itParams <- list(code = 'infantToddler', lab = 'Population Prop (<5 yo)', src = 'Census', yr = years, h = h, w = w, dp = dp)
cParams <- list(code = 'child', lab = 'Population Prop (5-19 yo)', src = 'Census', yr = years, h = h, w = w, dp = dp)
aParams <- list(code = 'adult', lab = 'Population Prop (20-64 yo)', src = 'Census', yr = years, h = h, w = w, dp = dp)
eParams <- list(code = 'elderly', lab = 'Population Prop (65+ yo)', src = 'Census', yr = years, h = h, w = w, dp = dp)

# set ts parameters
leg.lab <- c('Population Prop (<5 yo)', 'Population Prop (5-19 yo)', 'Population Prop (20-64 yo)', 'Population Prop (65+ yo)')

#### import data ################################
setwd('../reference_data')
abbrDat <- read_csv("state_abbreviations_FIPS.csv", col_types = list(FIPS = col_character())) %>%
  rename(fips_st = FIPS)
# import popDensity and popHousing data
itPopDat <- cleanX_censusInfantToddlerPop_cty()
cPopDat <- cleanX_censusChildPop_cty()
aPopDat <- cleanX_censusAdultPop_cty()
ePopDat <- cleanX_censusElderlyPop_cty()

#### clean and merge data for plotting ################################
# for choro
fullDat <- full_join(itPopDat, cPopDat, by = c("fips", "year")) %>%
  full_join(aPopDat, by = c("fips", "year")) %>%
  full_join(ePopDat, by = c("fips", "year")) %>%
  gather(covariate, value, infantToddler:elderly) %>%
  arrange(fips, year) 

# for ts
uqst <- abbrDat %>% select(State) %>% distinct(State) %>% arrange(State) %>%
  mutate(for.plot = seq_along(1:nrow(.)))
fullDat2 <- fullDat %>%
  mutate(fips_st = substring(fips, 1, 2)) %>%
  left_join(abbrDat, by = "fips_st") %>%
  left_join(uqst, by = "State")
indexes <- seq(1, max(fullDat2 %>% select(for.plot)), by=num)
years <- fullDat2 %>% select(year) %>% distinct(year) %>% arrange(year) %>% unlist
varnames <- fullDat2 %>% select(covariate) %>% unique %>% unlist

#### plot setup ################################
setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/EDA_agePop_Census_cty", showWarnings = FALSE)
setwd("../graph_outputs/EDA_agePop_Census_cty")

#### choropleths ################################
dir.create("./choro", showWarnings = FALSE)
setwd("./choro")
us <- ggcounty.us()
gg <- us$g

################################
# function to draw tier and gradient choropleths (all years on same figure)
choroplots <- function(dummyDat, params){
  # import parameters
  code <- params$code; lab <- params$lab; src <- params$src; yr <- params$yr
  h <- params$h; w <- params$w; dp <- params$dp
  
  # create aggregate df for all processed seasons
  pltDat <- data.frame()
  # process bin categories for each season separately
  for (y in yr){
    dummyDat2 <- dummyDat %>%
      filter(covariate == code, year == y) %>%
      mutate(vbin = cut(value, breaks = quantile(value, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE, include.lowest = TRUE)) %>%
      mutate(vbin = factor(vbin, levels = rev(levels(vbin)), labels = labVec)) %>% 
      mutate(cov_color = factor(vbin, levels = levels(vbin), labels = colVec)) %>%
      mutate(cov_col_string = as.character(cov_color))
    pltDat <- bind_rows(pltDat, dummyDat2)
  }
  
  choro.tier <- gg +
    geom_map(data = pltDat, aes(map_id = fips, fill = vbin, group = year), map = us$map, color = "black") +
    scale_fill_brewer(name = lab, palette = "RdYlGn") +
    expand_limits(x = gg$long, y = gg$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
    facet_wrap(~year, nrow = 2) 
  ggsave(sprintf("%s_tiers_%s_cty.png", code, src), choro.tier, height = h, width = w, dpi = dp)
  
  choro.grad <- gg +
    geom_map(data = pltDat, aes(map_id = fips, fill = value, group = year), map = us$map, color = "black") +
    scale_fill_continuous(name = lab, low = "green", high = "red") +
    expand_limits(x = gg$long, y = gg$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
    facet_wrap(~year, nrow = 2) 
  ggsave(sprintf("%s_grad_%s_cty.png", code, src), choro.grad, height = h, width = w, dpi = dp)
} 

################################
# function to draw tier and gradient choropleths (one year per figure)
choroplots_1yr <- function(dummyDat, params){
  # import parameters
  code <- params$code; lab <- params$lab; src <- params$src; yr <- params$yr
  h <- params$h; w <- params$w; dp <- params$dp
  
  # process bin categories for each season separately
  for (y in yr){
    pltDat <- dummyDat %>%
      filter(covariate == code, year == y) %>%
      mutate(vbin = cut(value, breaks = quantile(value, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE, include.lowest = TRUE)) %>%
      mutate(vbin = factor(vbin, levels = rev(levels(vbin)), labels = labVec)) %>% 
      mutate(cov_color = factor(vbin, levels = levels(vbin), labels = colVec)) %>%
      mutate(cov_col_string = as.character(cov_color))
  
    choro.tier <- gg +
      geom_map(data = pltDat, aes(map_id = fips, fill = vbin), map = us$map, color = "black") +
      scale_fill_brewer(name = lab, palette = "RdYlGn") +
      expand_limits(x = gg$long, y = gg$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") 
    ggsave(sprintf("%sPop_tiers_%s_cty_%s.png", code, src, y), choro.tier, height = h, width = w, dpi = dp)
    
    choro.grad <- gg +
      geom_map(data = pltDat, aes(map_id = fips, fill = value), map = us$map, color = "black") +
      scale_fill_continuous(name = lab, low = "green", high = "red") +
      expand_limits(x = gg$long, y = gg$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom")
    ggsave(sprintf("%sPop_grad_%s_cty_%s.png", code, src, y), choro.grad, height = h, width = w, dpi = dp)
  }
} 

# ################################
# # draw plots
# choroplots_1yr(fullDat, itParams)
# choroplots_1yr(fullDat, cParams)
# choroplots_1yr(fullDat, aParams)
# choroplots_1yr(fullDat, eParams)
# # 5/25/16

#### time series ################################
dir.create("../ts", showWarnings = FALSE)
setwd("../ts")

for(i in indexes){
  for(v in varnames){
    dummyplots <- ggplot(fullDat2 %>% filter(for.plot>= i & for.plot < i+num & covariate == v), aes(x=year, y=value)) +
      theme_bw()+
      theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
      geom_line(aes(colour = fips)) +
      stat_summary(fun.y = 'median', colour = 'black', size = 1.5, geom = "line") +
      scale_y_continuous(name = leg.lab[which(v == varnames)], limits = c(0, 1)) +
      scale_x_continuous(breaks = seq(2002, 2009, 2)) +
      guides(colour = "none") +
      facet_wrap(~State)
    labs <- fullDat2 %>% filter(for.plot>= i & for.plot < i+num) %>% select(Abbreviation) %>% distinct(Abbreviation) %>% arrange(Abbreviation) %>% slice(c(1, num))  %>% unlist
    ggsave(sprintf("%sPop_Census_cty_%s-%s.png", v, labs[1], labs[2]), dummyplots, width = w, height = h, dpi = dp)
  }
} 
# 5/25/16
