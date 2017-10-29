
## Name: Elizabeth Lee
## Date: 5/31/17
## Function: main code to generate MS figures for age-specific results
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
dbCodeStr <- "_ilinDt_Octfit_adult_span0.4_degree2"

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
### SEASONAL INTENSITY - age-specific population (not dependent on dbCodeStr) ###################
# N.B. does not rely on dbCodeStr args

obsFit_plotFormats_scatter_age <- list(w = 6, h = 3, ageLabs = c("Children", "Adults"))
scatter_obsFit_seasIntensityRR_multiSeason_age(c("8a_iliSum_v3-6", "8a_iliSum_v4-6"), obsFit_plotFormats_scatter_age, path_list)

# modCodeLs_regionValidation <- c("8a_iliSum_v2-6", "8a_iliSum_v3-6", "8a_iliSum_v4-6")
# ageVec <- c("total", "child", "adult")
# xmaxVec <- c(3.75, 1.5, 1.5)
# for (i in 1:length(modCodeLs_regionValidation)){
#   plotFormats_scatter_regionValidation <- list(w = 4, h = 4, age = ageVec[i], xmax = xmaxVec[i])
#   scatter_regionValidationILI(modCodeLs_regionValidation[i], plotFormats_scatter_regionValidation)
# }


# # not in appendix
# # obsFit_plotFormats_child <- list(w = 5, h = 9, popCode = "_child")
# choro_obsFit_seasIntensityRR_multiSeason("8a_iliSum_v3-6", obsFit_plotFormats_child, path_list)
# obsFit_plotFormats_adult <- list(w = 5, h = 9, popCode = "_adult")
# choro_obsFit_seasIntensityRR_multiSeason("8a_iliSum_v4-6", obsFit_plotFormats_adult, path_list)


###############################################################################
### SEASONAL INTENSITY - age-specific population (dependent on dbCodeStr) ###################
# N.B. must change dbCodeStr args by age group

if (grepl("child", dbCodeStr)){
    fit_plotFormats_one <- list(w = 3.15, h = 2.5, popCode = "_child", legendStep = 2)
    choro_fit_seasIntensityRR_oneSeason("8a_iliSum_v3-6", fit_plotFormats_one, path_list)

    obsFit_plotFormats_multi <- list(w = 4.25, h = 9, popCode = "_child", legendStep = 2)
    choro_obsFit_seasIntensityRR_multiSeason("8a_iliSum_v3-6", obsFit_plotFormats_multi, path_list)

} else if (grepl("adult", dbCodeStr)){
    fit_plotFormats_one <- list(w = 3.15, h = 2.5, popCode = "_adult", legendStep = 2)
    choro_fit_seasIntensityRR_oneSeason("8a_iliSum_v4-6", fit_plotFormats_one, path_list)

    obsFit_plotFormats_multi <- list(w = 4.25, h = 9, popCode = "_adult", legendStep = 2) 
    choro_obsFit_seasIntensityRR_multiSeason("8a_iliSum_v4-6", obsFit_plotFormats_multi, path_list)

}


