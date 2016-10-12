# 10/11/16: check identities of counties with 
require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL)

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr <- "6a_iliSum_v3-10"; testDataOn <- FALSE
seasons <- 2:9 
rdmFx_RV <- "nu"
dig <- 4 # number of digits in the number of elements at this spatial scale (~3000 counties -> 4 digits)

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
source("source_clean_data_functions.R") # functions to clean covariate data
source("source_prepare_inlaData_cty.R") # functions to aggregate all data sources for model
source("source_export_inlaData_cty.R") # functions to plot county-specific model diagnostics
source("source_export_inlaData.R") # functions to plot general model diagnostics
source("source_export_inlaData_hurdle.R") # data export functions for hurdle model

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


#### MAIN #################################

#### import fitted data & calculate gamma residuals ################################
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
gamFit <- data.frame()
for (s in seasons){
  seasDat <- read_csv(sprintf("summaryStatsFitted_gamma_%s_S%s.csv", modCodeStr, s)) %>%
    mutate(y_nonzero = ifelse(y>0, y, NA)) %>%
    mutate(yhat_resid = (y_nonzero-mean)/sd) %>%
    mutate(yhat_rawresid = (y_nonzero-mean)) %>%
    mutate(LB = mean-(sd*2), UB = mean+(sd*2))
  gamFit <- bind_rows(gamFit, seasDat)
}

#### analyze ################################
# subset data outside "1:1" boxes 
badFit <- gamFit %>%
  filter((y_nonzero>50 & mean/y_nonzero<.5))

## ID fips that appear in multiple seasons ##
troubleFips <- badFit %>% 
  group_by(fips) %>%
  count %>%
  arrange(desc(n))

hist(troubleFips$n, breaks=7)
#### findings: 134 of 353 fips appear in >1 season. 4 fips appear in 7 seasons. ####

## subset fips that appear 5+ times ##
fivePlus <- troubleFips %>%
  filter(n>=5) %>%
  select(fips) %>%
  unlist
#### findings: 18 of 134 fips appear in >=5 seasons. ####

## check fitted values for fivePlus fips ##
fiveplusFitted <- gamFit %>%
  filter(fips %in% fivePlus) %>%
  mutate(flag = ifelse((y_nonzero>50 & mean/y_nonzero<.5), TRUE, FALSE))
# View(fiveplusFitted %>% arrange(fips))
#### findings: nothing of note. ####

## check predictors for fivePlus fips ##
dummy <- model6a_iliSum_v2(path_list) # with driver & sampling effort variables
modData <- remove_case_exceptions(dummy)
fiveplusCov <- modData %>%
  right_join(fiveplusFitted %>% select(season, fips, y_nonzero, yhat_rawresid, flag), by = c("season", "fips"))
View(fiveplusCov)
#### findings: nothing of note. ####

# look at raw values for modData
dummy2 <- model6a_iliSum_v2_raw(path_list) # before standardizing
rmodData <- remove_case_exceptions(dummy2)
fiveplusRcov <- rmodData %>% 
  right_join(fiveplusFitted %>% select(season, fips, y_nonzero, yhat_rawresid, flag), by = c("season", "fips"))
View(fiveplusRcov %>% filter(flag))
#### findings: many of the flight covariates are 0, some of the hospaccess covariates are 0. ####


