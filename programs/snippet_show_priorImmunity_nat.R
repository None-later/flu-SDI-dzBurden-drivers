
## Name: Elizabeth Lee
## Date: 4/9/17
## Function: Show prior immunity by season for MS appendix table
## Filenames: physicianCoverage_IMSHealth_state.csv, dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: IMS Health
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL) # clean_data_functions dependencies

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
seasons <- c(3:9)

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
source("source_clean_data_functions.R") # functions to clean covariate data
source("source_prepare_inlaData_cty.R") # functions to aggregate all data sources for model
source("source_export_inlaData_cty.R") # functions to plot county-specific model diagnostics
source("source_export_inlaDiagnostics.R")

#### FILEPATHS #################################
setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))

# put all paths in a list to pass them around in functions
path_list <- list(path_response_cty = path_response_cty)

#### MAIN #################################
immDat <- cleanX_protectedFromPrevSeason_cty(path_list)
popDat <- clean_pop_cty_plain() %>%
  mutate(season = year-2000) %>%
  filter(season %in% seasons) %>%
  select(-year)
joinDat <- left_join(immDat, popDat, by = c("fips", "season")) 
fullDat <- joinDat %>%
  group_by(season) %>%
  summarise(protectionPrevSeason = weighted.mean(protectionPrevSeason, pop))

protDat <- cleanX_multsrcSubtypeDistrStrainSim_reg() %>%
  filter(season %in% seasons) %>%
  spread(region, estImmuneProp)
View(protDat)

test <- cleanX_multsrcSubtypeDistrStrainSim_reg()

