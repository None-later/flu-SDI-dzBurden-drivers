
## Name: Elizabeth Lee
## Date: 5/31/17
## Function: main code to generate MS figures for pandemic results
## Filenames: 
## Data Source: 
## Notes:

require(RColorBrewer); require(ggplot2); require(dplyr); require(tidyr); require(readr)
require(data.table)
require(lazyeval)
require(ggthemes)
setwd(dirname(sys.frame(1)$ofile))
source("source_export_msFigs.R")
source("source_clean_response_functions_cty.R")

################################
dbCodeStr <- "_ilinDt_Octfit_2009p_span0.4_degree2"

## PATHS ##
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_response_cty = path_response_cty)

################################
## MAIN ##
setwd(dirname(sys.frame(1)$ofile))

## PLOTS ##
###############################################################################
### SEASONAL INTENSITY - pandemic ###################

fit_plotFormats_one <- list(w = 3.15, h = 2.5, popCode = "_2009p", legendStep = 1)
choro_fit_seasIntensityRR_oneSeason("9a_iliSum_2009p_v1-2", fit_plotFormats_one, path_list)

obsFit_plotFormats_one <- list(w = 7, h = 3.5, popCode = "_2009p", legendStep = 1)
choro_obsFit_seasIntensityRR_oneSeason("9a_iliSum_2009p_v1-2", obsFit_plotFormats_one, path_list)

choro_stateEffects("9a_iliSum_2009p_v1-2")
