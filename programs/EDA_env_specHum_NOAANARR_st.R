## Name: Elizabeth Lee
## Date: 11/20/15
## Function: EDA - SAIPE median household income - choropleth and ts aggregated to state level

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
humidityDat <- cleanX_noaanarrSpecHum_st() %>% 
  rename(FIPS = fips)

setwd('../reference_data')
abbrDat <- read_csv("state_abbreviations_FIPS.csv", col_types = list(FIPS = col_character()))


#### clean and merge data ################################
fullDat <- left_join(humidityDat, abbrDat, by = c("FIPS")) %>%
  mutate(State = tolower(State))

#### plot setup ################################
setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/EDA_env_specHum_NOAANARR_st", showWarnings = FALSE)
setwd("../graph_outputs/EDA_env_specHum_NOAANARR_st")

#### choropleths ################################
dir.create("./choro", showWarnings = FALSE)
setwd("./choro")
states_map <- map_data("state")

for (s in seas){
  pltDat <- fullDat %>%
    filter(season == s) %>%
    mutate(hum_bin = cut(humidity, breaks = quantile(humidity, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE)) %>%
    mutate(hum_bin = factor(hum_bin, levels = rev(levels(hum_bin)), labels = incVec)) %>% 
    mutate(humidity_color = factor(hum_bin, levels = levels(hum_bin), labels = colVec)) %>%
    mutate(inc_hum_string = as.character(humidity_color))
  
  choro.tier <- ggplot(pltDat, aes(map_id = State, group = season)) +
    geom_map(aes(fill = hum_bin), map = states_map, color = "black") +
    scale_fill_brewer(name = "Flu Season Specific Humidity", palette = "RdYlGn") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
    facet_wrap(~season, nrow = 2) 
  ggsave(sprintf("specificHumidity_tiers_NOAANARR_st_S%s.png", s), choro.tier, height = h, width = w, dpi = dp)
  
  choro.grad <- ggplot(pltDat, aes(map_id = State, group = season)) +
    geom_map(aes(fill = humidity), map = states_map, color = "black") +
    scale_fill_continuous(name = "Flu Season Specific Humidity", low = "green", high = "red") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
    facet_wrap(~season, nrow = 2) 
  ggsave(sprintf("specificHumidity_gradient_NOAANARR_st_S%s.png", s), choro.grad, height = h, width = w, dpi = dp)
} # saved 4/10/16

# View(humidityDat %>% filter(name == 'Washington County (ME)'))

#### time series ################################
dir.create("../ts", showWarnings = FALSE)
setwd("../ts")

tsplot <- ggplot(fullDat, aes(x=season, y=humidity)) +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
  geom_line(colour = 'black') +
  geom_point(colour = 'black') +
  scale_y_continuous(name = "Flu Season Specific Humidity") +
  guides(colour = "none") +
  coord_cartesian(xlim = c(2002, 2009)) +
  facet_wrap(~State)
ggsave(sprintf("specificHumidity_NOAANARR_AL-WY.png"), tsplot, width = w2, height = h2, dpi = dp)
# saved 4/10/16

