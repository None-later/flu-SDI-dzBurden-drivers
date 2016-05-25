## Name: Elizabeth Lee
## Date: 5/24/16
## Function: EDA - percentage of samples laboratory confirmed for influenza (CDC FluVaxView region level) - choro & ts

## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")
rm(list = ls())
#### header ####################################
require(ggplot2)
require(dplyr)
require(readr)
require(RColorBrewer)
require(mapproj)

setwd(dirname(sys.frame(1)$ofile))
source("source_clean_data_functions.R")

#### plot formatting ################################
labVec <- paste("Tier", 1:5)
colVec <- brewer.pal(length(labVec), 'RdYlGn')
h <- 5; w <- 8; dp <- 300
h2 <- 12; w2 <- 12
mar <- rep(0, 4)
par(mar = mar, oma = mar)
num <- 6
seasons <- 2:9
paramls <- list(lab = 'Flu Positive (%)', seas = seasons, h = h, w = w, dp = dp)

#### import data ################################
setwd('../reference_data')
abbrDat <- read_csv("state_abbreviations_FIPS.csv", col_types = list(FIPS = col_character()))
regionNames <- data.frame(region = 1:10, reglabs = c("Boston", "New York", "Philadelphia", "Atlanta", "Chicago", "Dallas", "Kansas City", "Denver", "San Francisco", "Seattle"))

# merge fluPos and abbr data
fluPosDat <- cleanX_cdcFluview_fluPos_region() %>%
  full_join(abbrDat, by = c("fips" = "FIPS")) %>%
  mutate(state = tolower(State)) %>%
  arrange(season, region, fips) %>%
  select(-State, -Abbreviation) %>% 
  full_join(regionNames, by = "region")

#### plot setup ################################
setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/EDA_fluPos_cdcFluView_region", showWarnings = FALSE)
setwd("../graph_outputs/EDA_fluPos_cdcFluView_region")

#### choropleths ################################
dir.create("./choro", showWarnings = FALSE)
setwd("./choro")
states_map <- map_data("state")

################################
# function to draw tier and gradient choropleths
choroplots <- function(dummyDat, params){
  # import parameters
  lab <- params$lab; seas <- params$seas
  h <- params$h; w <- params$w; dp <- params$dp
  
  # create aggregate df for all processed seasons
  pltDat <- data.frame()
  # process bin categories for each season separately
  for (s in seas){
    dummyDat2 <- dummyDat %>%
      filter(season == s) %>%
      mutate(val_bin = cut(fluPos, breaks = quantile(fluPos, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE, include.lowest = TRUE)) %>%
      mutate(val_bin = factor(val_bin, levels = rev(levels(val_bin)), labels = labVec)) %>% 
      mutate(fp_color = factor(val_bin, levels = levels(val_bin), labels = colVec)) %>%
      mutate(fp_col_string = as.character(fp_color))
    pltDat <- bind_rows(pltDat, dummyDat2)
  }
  View(pltDat)
  
  choro.tier <- ggplot(pltDat, aes(map_id = state, group = season)) +
    geom_map(aes(fill = val_bin), map = states_map, color = "black") +
    scale_fill_brewer(name = lab, palette = "RdYlGn") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
    facet_wrap(~season, nrow = 2) 
  ggsave(sprintf("fluPos_tiers_cdcFluView_region.png"), choro.tier, height = h, width = w, dpi = dp)
  
  choro.grad <- ggplot(pltDat, aes(map_id = state, group = season)) +
    geom_map(aes(fill = fluPos), map = states_map, color = "black") +
    scale_fill_continuous(name = lab, low = "green", high = "red") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
    facet_wrap(~season, nrow = 2) 
  ggsave(sprintf("fluPos_grad_cdcFluView_region.png"), choro.grad, height = h, width = w, dpi = dp)
} 

################################
# draw plots
choroplots(fluPosDat, paramls)
# saved 5/24/16

#### time series ################################
dir.create("../ts", showWarnings = FALSE)
setwd("../ts")

fpRegDat <- fluPosDat %>%
  group_by(season, reglabs) %>%
  sample_n(size = 1) %>%
  select(season, reglabs, fluPos) %>% ungroup

tsplot <- ggplot(fpRegDat, aes(x=season, y=fluPos)) +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
  geom_line(aes(colour = reglabs)) +
  geom_point(aes(colour = reglabs)) +
  scale_y_continuous(name = "Flu Positive (%)") +
  coord_cartesian(xlim = c(2, 9))
ggsave(sprintf("fluPos_allRegions.png"), tsplot, width = w, height = h, dpi = dp)
# saved 5/24/16

