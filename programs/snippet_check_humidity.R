# 
# ## Name: Elizabeth Lee
# ## Date: 4/18/17
# ## Function: Check humdity results
# ## Filenames:
# ## Data Source: IMS Health
# ## Notes:
# ##
# ## useful commands:
# ## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
# ## update.packages(lib.loc = "/usr/local/lib/R/site-library")
# 
# #### header #################################
# rm(list = ls())
# require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL) # clean_data_functions dependencies
# require(maptools); require(spdep) # prepare_inlaData_st.R dependencies
# require(RColorBrewer); require(ggplot2) # export_inlaData_st dependencies
# 
# 
# #### set these! ################################
# dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
# rdmFx_RV <- "phi"
# likString <- "normal"
# dig <- 4 # number of digits in the number of elements at this spatial scale (~3000 counties -> 4 digits)
# s <- 999 # all seasons code for spatiotemporal analysis = 999
# 
# #### SOURCE: clean and import model data #################################
# setwd(dirname(sys.frame(1)$ofile))
# source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
# source("source_clean_data_functions.R") # functions to clean covariate data
# source("source_prepare_inlaData_cty.R") # functions to aggregate all data sources for model
# source("source_export_inlaData_cty.R") # functions to plot county-specific model diagnostics
# 
# #### FILEPATHS #################################
# setwd('../reference_data')
# path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
# path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
# 
# setwd('./UScounty_shapefiles')
# path_adjMxExport_cty <- paste0(getwd(), "/US_county_adjacency.graph")
# path_graphIdx_cty <- paste0(getwd(), "/US_county_graph_index.csv")
# path_shape_cty <- paste0(getwd(), "/gz_2010_us_050_00_500k") # for dbf metadata only
# 
# setwd('../stateFlightpassenger_graph')
# path_graphExport_st <- paste0(getwd(), "/US_statePassenger_edgelist.txt")
# path_graphIdx_st <- paste0(getwd(), "/US_statePassenger_graph_index.csv")
# 
# setwd("../../R_export")
# path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
# 
# # put all paths in a list to pass them around in functions
# path_list <- list(path_abbr_st = path_abbr_st,
#                   path_latlon_cty = path_latlon_cty,
#                   path_shape_cty = path_shape_cty,
#                   path_adjMxExport_cty = path_adjMxExport_cty,
#                   path_response_cty = path_response_cty,
#                   path_graphIdx_cty = path_graphIdx_cty,
#                   path_graphExport_st = path_graphExport_st,
#                   path_graphIdx_st = path_graphIdx_st)
# 
# #### MAIN #################################
# #### Import and process data ####
# modDat <- model8a_iliSum_v7(path_list)
# 
# cor(modDat$y1, modDat$X_humidity, use="complete.obs")
# 
# #### Draw map of results for humidity and imscoverage ####
# summStatsPath <- "../R_export/inlaModelData_export/8a_iliSum_v2-6/summaryStats_8a_iliSum_v2-6.csv"
# summStatsDat <- read_csv(summStatsPath, col_types = cols_only(RV = "c", effectType = "c", mean = "d"))
# 
# beta0 <- summStatsDat$mean[summStatsDat$RV=="intercept_nonzero"]
# beta_humidity <- summStatsDat$mean[summStatsDat$RV=="X_humidity_nonzero"]
# beta_imscoverage <- summStatsDat$mean[summStatsDat$RV=="O_imscoverage_nonzero"]

modDat2 <- left_join(modDat, summStatsDat %>% filter(effectType == "stID"), by = c("fips_st"="RV")) %>%
  rename(stID = mean) %>% select(-effectType) %>%
  left_join(summStatsDat %>% filter(effectType == "spatial"), by = c("fips"="RV")) %>%
  rename(ctyID = mean) %>% select(-effectType) %>% 
  mutate(regionID = paste0("R", regionID)) %>%
  left_join(summStatsDat %>% filter(effectType == "regID"), by = c("regionID"="RV")) %>%
  rename(regID = mean) %>% select(-effectType) %>%
  mutate(season = paste0("S", season)) %>%
  left_join(summStatsDat %>% filter(effectType == "season"), by = c("season"="RV")) %>%
  rename(seasID = mean) %>% select(-effectType) %>%
  mutate(season = as.numeric(substring(season, 2, nchar(season)))) %>%
  mutate(ID = as.character(ID)) %>%
  left_join(summStatsDat %>% filter(effectType == "error"), by = c("ID"="RV")) %>%
  rename(error = mean) %>% select(-effectType) %>%
  rowwise %>%
  # mutate(yPlot = (beta0 + (beta_humidity*X_humidity) + (beta_imscoverage*O_imscoverage) + stID + ctyID + regID)) %>%
  mutate(yPlot = (stID + ctyID + regID + seasID + error)) %>%
  ungroup

setwd(getSrcDirectory(function(x){}))
for (s in 3:9){
  exportPath <- sprintf("../graph_outputs/snippet_check_humidity/choro_humidity_imscoverage_S%s.png", s)
  seasDat <- modDat2 %>% filter(season == s)
  plot_countyChoro(exportPath, seasDat, "yPlot", "tier", FALSE)
}

