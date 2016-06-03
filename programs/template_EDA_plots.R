## Name: Elizabeth Lee
## Date: 6/2/16
## Function: template code to automate plotting for choropleths and time series; plot functions found in source_EDA_plots.R
## Filename: 
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

setwd(dirname(sys.frame(1)$ofile))
source("source_clean_data_functions.R")
source("source_EDA_plots.R")

#### import data ################################
setwd('../reference_data')
abbrDat <- read_csv("state_abbreviations_FIPS.csv", col_types = list(FIPS = col_character())) %>%
  rename(fips_st = FIPS)

# import covariate data
popdensDat <- cleanX_popDensity_cty()

#### clean data ################################
# for choro
fullDat <- popdensDat %>%
  gather(covariate, value, popDensity) %>%
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


#### SET THESE! ################################
## plot folder parameters ##
plotfolder <- "EDA_popDensity_Census_cty"

## choropleth parameters ##
choroParams <- list(spatial = 'cty', code = 'popDensity', lab = 'Population Density', src = 'Census', yr = years)
choroplotParams <- list(labVec = paste("Tier", 1:5), colVec = brewer.pal(length(labVec), 'RdYlGn'), h = 5, w = 8, dp = 300)

## time series parameters ##
tsParams <- list(indexes = indexes, years = years, varnames = varnames, spatial = 'cty', src = 'Census')
tsplotParams <- list(num = 6, h = 12, w = 12, dp = 300, leg.lab = c("Pop Density per Sq Mile"))


#### draw plots ################################
setwd(dirname(sys.frame(1)$ofile))
dir.create(sprintf("../graph_outputs/%s", plotfolder), showWarnings = FALSE)
setwd(sprintf("../graph_outputs/%s", plotfolder))

# choropleths
dir.create("./choro", showWarnings = FALSE)
setwd("./choro")
choroplots_cty_1yr(fullDat, choroParams, choroplotParams)
choroplots_cty(fullDat, choroParams, choroplotParams)

# time series
dir.create("../ts", showWarnings = FALSE)
setwd("../ts")
tsplots_cty(fullDat2, tsParams, tsplotParams)
