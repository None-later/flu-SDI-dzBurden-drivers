
## Name: Elizabeth Lee
## Date: 4/3/17
## Function: Write the disease burden measure wks.to.epi in terms of the weekdate of epidemic start (from weeknum 40), for total pop, children, and adults
## Data Source: IMS Health
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL) # clean_data_functions dependencies
require(ggplot2)

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_child_span0.4_degree2"

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean response data
source("source_clean_data_functions.R") # identify_firstEpiWeekdate


#### FILEPATHS #################################
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")

setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_fullIndic_cty <- paste0(getwd(), sprintf("/fullIndicAll_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_response_cty = path_response_cty,
                  path_fullIndic_cty = path_fullIndic_cty)

#### MAIN #################################
dbDat <- cleanR_wksToEpi_cty(path_list)
dateDat <- identify_firstEpiWeekdate(path_list) 
exportDat <- dateDat %>%
  filter(season %in% 2:9) %>%
  rename(epiStartWeek = t.firstepiweek)
write_csv(exportDat, sprintf("/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/samples/SB_03_16_2017/dbMetrics_epiStartWeek%s_cty.csv", dbCodeStr))

gg <- ggplot(exportDat, aes(x = epiStartWeek)) +
  geom_histogram() +
  scale_y_continuous("Number of counties") +
  scale_x_date("Epidemic start week") +
  facet_wrap(~season, scales='free_x')
print(gg)
ggsave(sprintf("/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/EDA_IMS_burden_wksToEpi_cty/distr_firstEpiWeekdate%s_cty.png", dbCodeStr), height = 4, width = 6, dpi = 300)

