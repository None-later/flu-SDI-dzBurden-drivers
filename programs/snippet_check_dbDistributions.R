
## Name: Elizabeth Lee
## Date: 10/4/16
## Function: check appropriateness of gamma distribution for iliSum and iliPeak
## Filenames: physicianCoverage_IMSHealth_state.csv, dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: IMS Health
## Notes: need to SSH into snow server
## v3testing1
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL) # clean_data_functions dependencies
require(maptools); require(spdep) # prepare_inlaData_st.R dependencies
require(INLA) # main dependencies
require(RColorBrewer); require(ggplot2) # export_inlaData_st dependencies


#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
w <- 9; h <- 6

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
source("source_clean_data_functions.R") # functions to clean covariate data

#### FILEPATHS #################################
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
path_adjMxExport_cty <- paste0(getwd(), "/US_county_adjacency_fips.dat")

setwd('./UScounty_shapefiles')
path_shape_cty <- paste0(getwd(), "/gz_2010_us_050_00_500k") # for dbf metadata only

setwd("../../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_shape_cty = path_shape_cty,
                  path_adjMxExport_cty = path_adjMxExport_cty,
                  path_response_cty = path_response_cty)


#### import db data #################################
iliSum <- cleanR_iliSum_cty(path_list)
iliPeak <- cleanR_iliPeak_cty(path_list)

#### export figures #################################
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf("../graph_outputs/explore_dbMetricsDistribution%s_cty", dbCodeStr))

#### histogram of all y data #################################
plt_y_ilisum <- ggplot(iliSum, aes(x = y, group = season)) +
  geom_histogram(bins = 100) +
  facet_wrap(~season, ncol=3) +
  scale_x_continuous("seasonal intensity (all y)") +
  scale_y_continuous("counties")
print(plt_y_ilisum)
ggsave(sprintf("distr_ILITot%s_cty_all.png", dbCodeStr), plt_y_ilisum, width=w, height=h)

plt_y_ilipeak <- ggplot(iliPeak, aes(x = y, group = season)) +
  geom_histogram(bins = 100) +
  facet_wrap(~season, ncol=3) +
  scale_x_continuous("peak intensity (all y)") +
  scale_y_continuous("counties")
print(plt_y_ilipeak)
ggsave(sprintf("distr_pkCount%s_cty_all.png", dbCodeStr), plt_y_ilipeak, width=w, height=h)

#### histogram of y_nonzero data #################################
plt_y1_ilisum <- ggplot(iliSum, aes(x = y1, group = season)) +
  geom_histogram(bins = 100) +
  facet_wrap(~season, ncol=3) +
  scale_x_continuous("seasonal intensity (nonzero y)") +
  scale_y_continuous("counties")
print(plt_y1_ilisum)
ggsave(sprintf("distr_ILITot%s_cty_nonzero.png", dbCodeStr), plt_y1_ilisum, width=w, height=h)

plt_y1_ilipeak <- ggplot(iliPeak, aes(x = y1, group = season)) +
  geom_histogram(bins = 100) +
  facet_wrap(~season, ncol=3) +
  scale_x_continuous("peak intensity (nonzero y)") +
  scale_y_continuous("counties")
print(plt_y1_ilipeak)
ggsave(sprintf("distr_pkCount%s_cty_nonzero.png", dbCodeStr), plt_y1_ilipeak, width=w, height=h)

plt_y1_ilisum_log <- ggplot(iliSum, aes(x = y1, group = season)) +
  geom_histogram(bins = 100) +
  facet_wrap(~season, ncol=3) +
  scale_x_continuous("seasonal intensity (nonzero y)", trans="log") +
  scale_y_continuous("counties")
ggsave(sprintf("distr_ILITot%s_cty_nonzero_log.png", dbCodeStr), plt_y1_ilisum_log, width=w, height=h)

plt_y1_ilipeak_log <- ggplot(iliPeak, aes(x = y1, group = season)) +
  geom_histogram(bins = 100) +
  facet_wrap(~season, ncol=3) +
  scale_x_continuous("peak intensity (nonzero y)", trans="log") +
  scale_y_continuous("counties")
ggsave(sprintf("distr_pkCount%s_cty_nonzero_log.png", dbCodeStr), plt_y1_ilipeak_log, width=w, height=h)

#### summary statistics of y_nonzero data #################################
stats_y1_ilisum <- iliSum %>%
  group_by(season) %>%
  summarise(mn = mean(y1, na.rm=TRUE), sd = sd(y1, na.rm=TRUE), var = var(y1, na.rm=TRUE))
stats_y1_ilipeak <- iliPeak %>%
  group_by(season) %>%
  summarise(mn = mean(y1, na.rm=TRUE), sd = sd(y1, na.rm=TRUE), var = var(y1, na.rm=TRUE))

#10/4/16  
  
  