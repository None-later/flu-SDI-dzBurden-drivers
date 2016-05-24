## Name: Elizabeth Lee
## Date: 11/2/15
## Function: EDA - SAIPE poverty - choropleth, state-level; time series by state
## Notes: "*_universe" means the population size from which the "*_counts" and "*_percent" are derived

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
require(mapproj)
setwd(dirname(sys.frame(1)$ofile))

#### plot formatting ################################
tierVec <- paste("Tier", 1:5)
colVec <- brewer.pal(length(tierVec), 'RdYlGn')
h <- 5; w <- 8; dp <- 300
h2 <- 9; w2 <- 9
mar <- rep(0, 4)
par(mar = mar, oma = mar)
num <- 6

#### import data ################################
setwd('../reference_data')
abbrDat <- read_csv("state_abbreviations_FIPS.csv", col_types = list(FIPS = col_character()))

setwd(dirname(sys.frame(1)$ofile))
setwd("../../../../Sandra Goldlust's Work/Shared_Data/SG_covariate_data/Cleaned_Data")
# poverty data
povDat <- read_csv("clean_SAIPE_poverty.csv", col_types = list("state_id" = col_character(), "county_id" = col_character()), na = c("\\N")) %>%
  filter(type == 'state') %>%
  select(year, state_id, county_id, name, contains("_universe"), contains("_percent")) %>%
  select(-contains("_percentLB"), -contains("_percentUB90"))
# merge data together
fullDat <- left_join(povDat, abbrDat, by = c("state_id" = "FIPS")) %>%
  mutate(cty = tolower(substring(name, 1, nchar(name)-12))) %>%
  mutate(match = paste(tolower(State), cty, sep=',')) %>%
  rename(allPovPerc = all_poverty_percent, u18PovPerc = under18_poverty_percent, b5_17PovPerc = btwn517_poverty_percent, u5PovPerc = under5_poverty_percent)

#### plot variables ################################
varnames <- names(fullDat)[c(9:11)]
leg.lab <- c("All poverty (%)", "Under 18 in poverty (%)", "5-17 in poverty (%)")
uqst <- fullDat %>% select(State) %>% distinct %>% arrange(State) %>%
  mutate(for.plot = seq_along(1:nrow(.)))

fullDat2 <- left_join(fullDat, uqst, by = "State")
indexes <- seq(1, max(fullDat2 %>% select(for.plot)), by=num)
years <- fullDat2 %>% select(year) %>% distinct %>% arrange(year) %>% unlist

#### plot setup ################################
setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/EDA_poverty_SAIPE_st", showWarnings = FALSE)
setwd("../graph_outputs/EDA_poverty_SAIPE_st")

#### choropleths ################################
dir.create("./choro", showWarnings = FALSE)
setwd("./choro")
states_map <- map_data("state")

for (yr in years){
  pltDat <- fullDat2 %>% filter(year == yr) 
  for (v in varnames) {
    pltDat2 <- pltDat %>% rename_(value = as.name(v))
    if (all(is.na(pltDat2$value))) {next}
    else{if (dim(pltDat2 %>% filter(!is.na(value)))[1] < 5) {next} # skip if there are fewer than five counties with data
    pltDat3 <- pltDat2 %>% 
      mutate(val_bin = cut(value, breaks = quantile(value, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE, include.lowest = TRUE)) %>%
      mutate(val_bin = factor(val_bin, levels = rev(levels(val_bin)), labels = tierVec)) %>% 
      mutate(val_color = factor(val_bin, levels = levels(val_bin), labels = colVec)) %>%
      mutate(val_col_string = as.character(val_color))
    
    choro.tier <- ggplot(pltDat3, aes(map_id = tolower(State), group = year)) +
      geom_map(aes(fill = val_bin), map = states_map, color = "black") +
      scale_fill_brewer(name = v, palette = "RdYlGn") +
      expand_limits(x = states_map$long, y = states_map$lat) +
      theme_minimal() +
      theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
      facet_wrap(~year, nrow = 2) 
    ggsave(sprintf("%s_tiers_poverty_SAIPE_st_%s.png", varnames[which(v == varnames)], yr), choro.tier, height = h, width = w, dpi = dp)
    }
  }
} # 11/22/15

#### time series ################################
dir.create("../ts", showWarnings = FALSE)
setwd("../ts")

for(v in varnames){
  dummyplots <- ggplot(fullDat2, aes(x=year, y=eval(parse(text = v)))) +
    theme_bw()+
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(colour = 'black') +
    geom_point(colour = 'black') +
    scale_y_continuous(name = leg.lab[which(v == varnames)]) +
    guides(colour = "none") +
    coord_cartesian(xlim = c(1989, 2013)) +
    facet_wrap(~State)
  ggsave(sprintf("%s_poverty_SAIPE_st_AK_WY.png", v), dummyplots, width = w2, height = h2, dpi = dp)
} # 11/22/15