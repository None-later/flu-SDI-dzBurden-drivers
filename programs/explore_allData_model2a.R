
## Name: Elizabeth Lee
## Date: 12/13/15
## Function: compare choropleths of disease burden and covariates
## Notes: v1 (12/12/15): b0 + b1*x1popDens[i] + b2*x2unins[i] + b3*x3pov[i] 
## v2 (12/13/15): b0
## v3 (12/13/15): b0 + b1*x1popDens[i]
## v4 (12/13/15): b0 + b2*x2unins[i]
## v5 (12/13/15): b0 + b3*x3pov[i] 
## Data Source: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

rm(list = ls())
#### header #################################
# .Rprofile sets wd 'Dropbox (Bansal Lab)/code' and sources "GeneralTools.R" in .myenv
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
require(tidyr)
require(dplyr)
require(readr)
require(ggplot2); require(RColorBrewer)

#### set these ################################
var <- list(span = 0.5, degree = 2)

#### string formatting ################################
code <- ''
code2 <- '_Octfit'
modcode <- '2a'
code.str <- sprintf('_span%s_degree%s', var$span, var$degree)

#### plot formatting ################################
w <- 12; h <- 7

#### import db data ################################
setwd('../R_export')
dbData <- read_csv(sprintf('dbMetrics_periodicReg_%silicnDt%s%s_analyzeDB_st.csv', code, code2, code.str)) %>% 
  rename(abbr_st = state)
metric.ls <- c("ilicnDt.sum", "ilicnDt.excess.BL", "ilicnDt.excess.thresh", "ilicnDt.peak", "epi.dur", "wks.to.epi", "wks.to.peak")
tiers.ls <- c(5, 5, 5, 5, 4, 4, 4)

#### clean db data ################################
setwd('../reference_data')
abbrData <- read_csv('state_abbreviations_FIPS.csv', col_types = 'ccc')
dbData2 <- left_join(dbData, abbrData, by = c("abbr_st" = "Abbreviation")) %>% 
  mutate(State = tolower(State)) %>%
  select(season, abbr_st, State, metric, burden)

#### import covariate data ################################
setwd('../R_export/jagsModelData_import')
covData <- read_csv(sprintf('covData_%s_v1.csv', modcode), col_types="__ddcdddddci") %>%
  select(season, abbr_st, State, popDensity, uninsured_perc, inPoverty_perc) %>% 
  mutate(State = tolower(State)) %>%
  gather(covariate, value, popDensity:inPoverty_perc)
cov.ls <- c("popDensity", "uninsured_perc", "inPoverty_perc")
tiercov.ls <- c(5, 5, 5)

#### plotting functions ################################

## plot data in tiers
plotDataTier <- function(dataset, modcode, metric, tiers){
  states_map <- map_data("state")
  tierLab <- paste("Tier", 1:tiers)
  
  cutDataset <- dataset %>% group_by(season) %>%
    mutate(burdenTier = cut(burden, breaks = unique(quantile(burden, probs = seq(0, 1, by = 1/tiers)), na.rm = T, include.lowest = T), ordered_result = TRUE)) %>%
    mutate(burdenTier = factor(burdenTier, levels = levels(burdenTier), labels = tierLab))
  
  dummyplot <- ggplot(cutDataset, aes(map_id = State, group = season)) +
    geom_map(aes(fill = burdenTier), map = states_map, color = "black") +
    scale_fill_brewer(name = "", palette = "YlOrRd", guide = "legend") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank()) +
    facet_wrap(~season, nrow = 2) + 
    ggtitle(sprintf('%s', metric))
  return(dummyplot)
}

## plot burden in continuous gradient
plotDataGrad <- function(dataset, modcode, metric){
  states_map <- map_data("state")

  dummyplot <- ggplot(dataset, aes(map_id = State, group = season)) +
    geom_map(aes(fill = burden), map = states_map, color = "black") +
    scale_fill_gradient(name = "value", low = "light yellow", high = "red", guide = "legend") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank()) +
    facet_wrap(~season, nrow = 2) + 
    ggtitle(sprintf('%s', metric))
  return(dummyplot)
}


#### plot choropleths ################################
dir.create(sprintf('../../graph_outputs/jagsModelData_model%s', modcode), showWarnings = FALSE)
setwd(sprintf('../../graph_outputs/jagsModelData_model%s', modcode))

#### plot burden ################################
for (i in 1:length(metric.ls)){
  met <- metric.ls[i]; tier <- tiers.ls[i]
  dummyData <- dbData2 %>% filter(metric == met)
  
  gradplot <- plotDataGrad(dummyData, modcode, met)
  ggsave(sprintf("burden_%s_%s.png", modcode, met), gradplot, width=w, height=h)
  
  tierplot <- plotDataTier(dummyData, modcode, met, tier) 
  ggsave(sprintf("burdenTier_%s_%s.png", modcode, met), tierplot, width=w, height=h)
}

#### plot covariate  ################################
for (i in 1:length(cov.ls)){
  met <- cov.ls[i]; tier <- tiercov.ls[i]
  dummyData <- covData %>% rename(burden = value, metric = covariate) %>% filter(metric == met) 
  
  gradplot <- plotDataGrad(dummyData, modcode, met)
  ggsave(sprintf("cov_%s_%s.png", modcode, met), gradplot, width=w, height=h)
  
  tierplot <- plotDataTier(dummyData, modcode, met, tier) 
  ggsave(sprintf("covTier_%s_%s.png", modcode, met), tierplot, width=w, height=h)
}

# #### how many states report burden each season? ################################
# View(dbData2 %>% group_by(season, metric) %>% summarise(counts = length(abbr_st)))
