## Name: Elizabeth Lee
## Date: 5/24/16
## Function: EDA - infant (any and full vax levels) and elderly (any vax level only) vaccination coverage from NIS (infant) and BRFSS (elderly) surveys at the state-level

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
eSeas <- 2:7
iSeas <- 3:9
aSeas <- 6:7

# set choropleth parameters
eldParams <- list(code = 'elderlyAny', lab = 'Flu Vax Coverage (65+, any)', src = 'BRFSS', seas = eSeas, h = h, w = w, dp = dp)
infAParams <- list(code = 'infantAny', lab = 'Flu Vax Coverage (<2, any)', src = 'NIS', seas = iSeas, h = h, w = w, dp = dp)
infFParams <- list(code = 'infantFull', lab = 'Flu Vax Coverage (<2, full)', src = 'NIS', seas = iSeas, h = h, w = w, dp = dp)
adParams <- list(code = 'adultAny', lab = 'Flu Vax Coverage (65+, any)', src = 'BRFSS', seas = aSeas, h = h, w = w, dp = dp) # added 6/8/16

#### import data ################################
setwd('../reference_data')
abbrDat <- read_csv("state_abbreviations_FIPS.csv", col_types = list(FIPS = col_character()))

# infant any, infant full, elderly any, adult any import
infAnyDat <- cleanX_nisInfantAnyVaxCov_st() %>% rename(infantAny95 = interval95)
infFullDat <- cleanX_nisInfantFullVaxCov_st() %>% rename(infantFull95 = interval95)
eldAnyDat <- cleanX_brfssElderlyAnyVaxCov_st() %>% rename(elderlyAny95 = interval95)
adAnyDat <- cleanX_brfssAdultAnyVaxCov_st() %>% rename(adultAny95 = interval95)

#### clean and merge data ################################
fullDat <- full_join(infAnyDat, infFullDat, by = c("season", "location")) %>%
  full_join(eldAnyDat, by = c("season", "location")) %>%
  full_join(adAnyDat, by = c("season", "location")) %>%
  rename(infantAny = infantAnyVax, infantFull = infantFullVax, elderlyAny = elderlyAnyVax, adultAny = adultAnyVax, state = location) %>%
  gather(ageVax, vaxcov, infantAny, infantFull, elderlyAny, adultAny) %>%
  arrange(season, state, ageVax) %>%
  mutate(state = tolower(state)) %>%
  mutate(interval95 = ifelse(ageVax == 'elderlyAny', elderlyAny95, ifelse(ageVax == 'infantAny', infantAny95, ifelse(ageVax == 'infantFull', infantFull95, ifelse(ageVax == 'adultAny', adultAny95, NA))))) %>%
  mutate(lower = vaxcov - interval95, upper = vaxcov + interval95) %>%
  select(-elderlyAny95, -infantAny95, -infantFull95, -adultAny95)

#### plot setup ################################
setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/EDA_immunity_vaxCov_nisBRFSS_st", showWarnings = FALSE)
setwd("../graph_outputs/EDA_immunity_vaxCov_nisBRFSS_st")

#### choropleths ################################
dir.create("./choro", showWarnings = FALSE)
setwd("./choro")
states_map <- map_data("state")

################################
# function to draw tier and gradient choropleths
choroplots <- function(dummyDat, params){
  # import parameters
  code <- params$code; lab <- params$lab; src <- params$src; seas <- params$seas
  h <- params$h; w <- params$w; dp <- params$dp
  
  # create aggregate df for all processed seasons
  pltDat <- data.frame()
  # process bin categories for each season separately
  for (s in seas){
    dummyDat2 <- dummyDat %>%
      filter(ageVax == code, season == s) %>%
      mutate(vc_bin = cut(vaxcov, breaks = quantile(vaxcov, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE, include.lowest = TRUE)) %>%
      mutate(vc_bin = factor(vc_bin, levels = rev(levels(vc_bin)), labels = labVec)) %>% 
      mutate(cov_color = factor(vc_bin, levels = levels(vc_bin), labels = colVec)) %>%
      mutate(cov_col_string = as.character(cov_color))
    pltDat <- bind_rows(pltDat, dummyDat2)
  }
  
  choro.tier <- ggplot(pltDat, aes(map_id = state, group = season)) +
    geom_map(aes(fill = vc_bin), map = states_map, color = "black") +
    scale_fill_brewer(name = lab, palette = "RdYlGn") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
    facet_wrap(~season, nrow = 2) 
  ggsave(sprintf("immunity_%sVax_tiers_%s_st.png", code, src), choro.tier, height = h, width = w, dpi = dp)
  
  choro.grad <- ggplot(pltDat, aes(map_id = state, group = season)) +
    geom_map(aes(fill = vaxcov), map = states_map, color = "black") +
    scale_fill_continuous(name = lab, low = "green", high = "red") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
    facet_wrap(~season, nrow = 2) 
  ggsave(sprintf("immunity_%sVax_grad_%s_st.png", code, src), choro.grad, height = h, width = w, dpi = dp)
} 

################################
# draw plots
choroplots(fullDat, eldParams)
choroplots(fullDat, infAParams)
choroplots(fullDat, infFParams)
choroplots(fullDat, adParams)
# saved 5/24/16

#### time series ################################
dir.create("../ts", showWarnings = FALSE)
setwd("../ts")

tsplot <- ggplot(fullDat, aes(x=season, y=vaxcov)) +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
  geom_line(aes(colour = ageVax)) +
  geom_point(aes(colour = ageVax)) +
  geom_pointrange(aes(ymin = lower, ymax = upper, colour = ageVax)) +
  scale_y_continuous(name = "Flu Vax Coverage (95% CI)") +
  coord_cartesian(xlim = c(2, 9)) +
  facet_wrap(~state)
ggsave(sprintf("immunity_fluVax_AL-WY.png"), tsplot, width = w2, height = h2, dpi = dp)
# saved 5/24/16

