## Name: Elizabeth Lee
## Date: 4/11/16
## Function: EDA - NOAA NARR air surface temperature - choropleth and ts aggregated to state level

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

#### plot formatting ################################
incVec <- paste("Tier", 1:5)
colVec <- brewer.pal(length(incVec), 'RdYlGn')
h <- 5; w <- 8; dp <- 300
h2 <- 12; w2 <- 12
mar <- rep(0, 4)
par(mar = mar, oma = mar)
seas <- 2:9
num <- 6

#### import data ################################
source("source_clean_data_functions.R")
tempDat <- cleanX_noaanarrSfcTemp_st() %>% 
  rename(FIPS = fips)

setwd('../reference_data')
abbrDat <- read_csv("state_abbreviations_FIPS.csv", col_types = list(FIPS = col_character()))


#### clean and merge data ################################
fullDat <- left_join(tempDat, abbrDat, by = c("FIPS")) %>%
  mutate(State = tolower(State))

#### plot setup ################################
setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/EDA_env_sfcTemp_NOAANARR_st", showWarnings = FALSE)
setwd("../graph_outputs/EDA_env_sfcTemp_NOAANARR_st")

#### choropleths ################################
dir.create("./choro", showWarnings = FALSE)
setwd("./choro")
states_map <- map_data("state")

for (s in seas){
  pltDat <- fullDat %>%
    filter(season == s) %>%
    mutate(temp_bin = cut(temperature, breaks = quantile(temperature, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE)) %>%
    mutate(temp_bin = factor(temp_bin, levels = rev(levels(temp_bin)), labels = incVec)) %>% 
    mutate(temp_color = factor(temp_bin, levels = levels(temp_bin), labels = colVec)) %>%
    mutate(inc_tmp_string = as.character(temp_color))
  
  choro.tier <- ggplot(pltDat, aes(map_id = State, group = season)) +
    geom_map(aes(fill = temp_bin), map = states_map, color = "black") +
    scale_fill_brewer(name = "Flu Season Surface Temperature", palette = "RdYlGn") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
    facet_wrap(~season, nrow = 2) 
  ggsave(sprintf("surfaceTemperature_tiers_NOAANARR_st_S%s.png", s), choro.tier, height = h, width = w, dpi = dp)
  
  choro.grad <- ggplot(pltDat, aes(map_id = State, group = season)) +
    geom_map(aes(fill = temperature), map = states_map, color = "black") +
    scale_fill_continuous(name = "Flu Season Surface Temperature (K)", low = "green", high = "red") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
    facet_wrap(~season, nrow = 2) 
  ggsave(sprintf("surfaceTemperature_gradient_NOAANARR_st_S%s.png", s), choro.grad, height = h, width = w, dpi = dp)
} # saved 4/11/16

# View(tempDat %>% filter(name == 'Washington County (ME)'))

#### time series ################################
dir.create("../ts", showWarnings = FALSE)
setwd("../ts")

fullDat2 <- fullDat %>%
  mutate(State = ifelse(FIPS == "72", "puerto rico", State))

tsplot <- ggplot(fullDat2, aes(x=season, y=temperature)) +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
  geom_line(colour = 'black') +
  geom_point(colour = 'black') +
  scale_y_continuous(name = "Flu Season Surface Temperature (K)") +
  guides(colour = "none") +
  coord_cartesian(xlim = c(2, 9)) +
  facet_wrap(~State)
ggsave(sprintf("surfaceTemperature_NOAANARR_AL-WY.png"), tsplot, width = w2, height = h2, dpi = dp)
# saved 4/11/16

