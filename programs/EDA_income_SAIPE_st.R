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
years <- c(1995, 1997:2013)
num <- 6

#### import data ################################
setwd('../reference_data')
abbrDat <- read_csv("state_abbreviations_FIPS.csv", col_types = list(FIPS = col_character()))

setwd(dirname(sys.frame(1)$ofile))
setwd("../../../../Sandra Goldlust's Work/Shared_Data/SG_covariate_data/Cleaned_Data")
# income data
incomeDat <- read_csv("clean_SAIPE_income.csv", col_types = list("state_id" = col_character(), "county_id" = col_character()), na = c("\\N")) %>%
  filter(type == 'state')

#### clean and merge data ################################
fullDat <- left_join(incomeDat, abbrDat %>% select(-State), by = c("state_id" = "FIPS")) %>%
  mutate(state = tolower(name))

#### plot setup ################################
setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/EDA_income_SAIPE_st", showWarnings = FALSE)
setwd("../graph_outputs/EDA_income_SAIPE_st")

#### choropleths ################################
dir.create("./choro", showWarnings = FALSE)
setwd("./choro")
states_map <- map_data("state")

for (yr in years){
  pltDat <- fullDat %>%
    filter(year == yr) %>%
    mutate(mi_bin = cut(med_income, breaks = quantile(med_income, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE)) %>%
    mutate(mi_bin = factor(mi_bin, levels = rev(levels(mi_bin)), labels = incVec)) %>% 
    mutate(income_color = factor(mi_bin, levels = levels(mi_bin), labels = colVec)) %>%
    mutate(inc_col_string = as.character(income_color))
  
  choro.tier <- ggplot(pltDat, aes(map_id = state, group = year)) +
    geom_map(aes(fill = mi_bin), map = states_map, color = "black") +
    scale_fill_brewer(name = "Median HH income", palette = "RdYlGn") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
    facet_wrap(~year, nrow = 2) 
  ggsave(sprintf("medHouseholdIncome_tiers_SAIPE_st_%s.png", yr), choro.tier, height = h, width = w, dpi = dp)
  
  choro.grad <- ggplot(pltDat, aes(map_id = state, group = year)) +
    geom_map(aes(fill = med_income), map = states_map, color = "black") +
    scale_fill_continuous(name = "Median HH income", low = "green", high = "red") +
    expand_limits(x = states_map$long, y = states_map$lat) +
    theme_minimal() +
    theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
    facet_wrap(~year, nrow = 2) 
  ggsave(sprintf("medHouseholdIncome_gradient_SAIPE_st_%s.png", yr), choro.grad, height = h, width = w, dpi = dp)
} # saved 11/20/15

# View(incomeDat %>% filter(name == 'Washington County (ME)'))

#### time series ################################
dir.create("../ts", showWarnings = FALSE)
setwd("../ts")

tsplot <- ggplot(fullDat, aes(x=year, y=med_income)) +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
  geom_line(colour = 'black') +
  geom_point(colour = 'black') +
  scale_y_continuous(name = "Median Household Income") +
  guides(colour = "none") +
  coord_cartesian(xlim = c(1995, 2013)) +
  facet_wrap(~name)
ggsave(sprintf("medHouseholdIncome_SAIPE_AL-WY.png"), tsplot, width = w2, height = h2, dpi = dp)
# saved 11/20/15

