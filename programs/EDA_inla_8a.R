# 3/23/17

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr); require(DBI); require(RMySQL) # clean_data_functions dependencies
require(maptools); require(spdep) # prepare_inlaData_st.R dependencies
require(INLA)

#### set these #################################
parameterVec <- c(1, 0.25, 0.3, 0.8)

#### source #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
source("source_clean_data_functions.R") # functions to clean covariate data
source("source_prepare_inlaData_cty.R") # functions to aggregate all data sources for model
source("source_pp_checks.R") # functions for posterior predictive checks

 #### FILEPATHS #################################
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")

setwd('./UScounty_shapefiles')
path_graphIdx_cty <- paste0(getwd(), "/US_county_graph_index.csv")

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                path_latlon_cty = path_latlon_cty,
                path_graphIdx_cty = path_graphIdx_cty)

#### simulate data #################################
simulate_dummy8a_iliSum <- function(filepathList, paramVec){
    # 3/23/17 create dummy dataset to verify INLA results
    print(match.call())
    print(filepathList)

    # list of continental states
    statesOnly <- read_csv(filepathList$path_abbr_st, col_types = "__c", col_names = c("stateID"), skip = 1) 
    continentalOnly <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist

    # import data 
    imsCov_cty_df <- cleanO_imsCoverage_cty()
    ahrfHosp_cty_df <- cleanX_ahrfHospitals_cty()
    graphIdx_df <- clean_graphIDx(filepathList, "county")

    # join data
    pred_df <- full_join(imsCov_cty_df, ahrfHosp_cty_df, by = c("year", "fips")) %>%
      full_join(graphIdx_df, by = "fips") %>%
      mutate(fips_st = substring(fips, 1, 2)) %>%
      mutate(O_imscoverage = centerStandardize(adjProviderCoverage)) %>%
      mutate(X_hospaccess = centerStandardize(hospitalAccess)) %>%
      filter(fips_st %in% continentalOnly) %>%
      filter(!is.na(graphIdx)) %>%
      filter(year %in% 2003:2009)

    # simulate data
    full_df <- pred_df %>%
      mutate(E = paramVec[1], logE = log(paramVec[1])) %>%
      mutate(logy = paramVec[2] + O_imscoverage*paramVec[3] + X_hospaccess*paramVec[4]) %>%
      mutate(y1 = exp(paramVec[2] + O_imscoverage*paramVec[3] + X_hospaccess*paramVec[4])) %>%
      mutate(ID = seq_along(fips))

    return(full_df)
}

#### MAIN #################################
formula <- y1 ~ 1 + O_imscoverage + X_hospaccess
fullData <- simulate_dummy8a_iliSum(path_list, parameterVec)

mod <- inla(formula, family = "poisson", data = fullData,
    # control.family(control.link(model = "log")),
    control.fixed = list(mean = 0, prec = 1/100),
    control.predictor = list(compute = TRUE, link = rep(1, nrow(fullData))),
    control.compute = list(dic = TRUE, cpo = TRUE),
    verbose = TRUE)

# # test predictive check functions
# modCodeStr <- "test"
# path_csvExport_replicateData <- "/home/elee/Downloads/EDA_inla_8a_ppchecks/replicateData.csv"
# path_csvExport_repSummData <- "/home/elee/Downloads/EDA_inla_8a_ppchecks/repSummData.csv"
# export_predictiveChecks_seasIntensity(path_csvExport_replicateData, path_csvExport_repSummData, mod, fullData, modCodeStr)


# ##### microbenchmark tests #################################
# # 21749 observations in dataset
# 
# tmarg_time <- microbenchmark(tmarg <- inla.tmarginal(function(x) (x - fullData$logE[i]), mod$marginals.fitted.values[[i]]), times=100)
# print(tmarg_time)
# 
# zmarg_time <- microbenchmark(dummyrow <- inla.zmarginal(tmarg, silent = TRUE), times=50)
# print(zmarg_time)
# 
# testdf <- data.frame()
# addrow_time <- microbenchmark(testdf <- bind_rows(testdf, dummyrow), times=1000)
# print(addrow_time)
# 
# rmarg_time <- microbenchmark(rrow <- inla.rmarginal(30, tmarg), times = 100)
# print(rmarg_time)
# 
# repdf <- data.frame()
# rbindrow_time <- microbenchmark(repdf <- rbind(repdf, rrow), times = 10000)
# print(rbindrow_time)
# 
# pval_time <- microbenchmark(inla.pmarginal(q = fullData$obs_rr[i], marginal = tmarg), times = 50)
# print(pval_time)
# 
# hpd_time <- microbenchmark(inla.hpdmarginal(0.95, tmarg), times = 50)
# print(hpd_time)


# ##### test predictive checks #################################
# predValueMeans <- c()
# predPvalues <- c()
# for (i in 1:nrow(fullData)){
#   predValueMeans[i] <- inla.emarginal(function(x) exp(x), mod$marginals.fitted.values[[i]])
#   predPvalues[i] <- inla.pmarginal(q = fullData$logy[i], marginal = mod$marginals.fitted.values[[i]])
# }
# 
# plot(fullData$y1, predValueMeans, xlab = "Observed Seasonal Intensity", ylab = "Pred. Mean Seas. Intensity")
# plot(fullData$logy, mod$summary.fitted.values$mean)
# 
# # transform marginals to relative risk
# test <- c()
# test[[1]] <- inla.tmarginal(function(x) (x - fullData$logE[i]), mod$marginals.fitted.values[[1]])

  
