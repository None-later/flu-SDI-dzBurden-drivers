
## Name: Elizabeth Lee
## Date: 12/16/16
## Function: Model 8a v2: CAR spatial model, shifted response variable (e.g., seasonal intensity 0 -> 1), with interaction terms
## Filenames: physicianCoverage_IMSHealth_state.csv, dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: IMS Health
## Notes:
##
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(tidyverse) # clean_data_functions dependencies
require(maptools); require(spdep) # prepare_inlaData_st.R dependencies
require(INLA) # main dependencies
require(RColorBrewer) # export_inlaData_st dependencies

dbCodeStr <- "_irDt_Octfit_span0.4_degree2"
# single code
modCodeLs <- c("8a_iliSum_irDt_v2-6")

for (i in 1:length(modCodeLs)){

  #### set these! ################################
  modCodeStr <- modCodeLs[i]
  # keep <- keepLs[i] # comment if single code
  # set.seed(seedLs[i]) # comment if single code
  rdmFx_RV <- "phi"
  likString <- "normal"
  dig <- 4 # number of digits in the number of elements at this spatial scale (~3000 counties -> 4 digits)
  s <- 999 # all seasons code for spatiotemporal analysis = 999

  ################################
  #### CUSTOM FUNCTIONS #################################

  convert_hurdleModel_nz_spatiotemporal <- function(modData_seas){
    # 10/11/16: prepare data seasonal model data for nonzero model component
    print(match.call())

    # bottom half response matrix with NA (binomial lik) and non-zeros/NA (gamma/normal lik)
    Y_nz <- modData_seas %>%
      select(y1) %>%
      unlist

    # covariate matrix for nonzero lik: response, predictors, random effects & offset
    # 10/30/16 control flow for graph Idx # 12/20/16 graph Idx st
    if(is.null(modData_seas$graphIdx) & is.null(modData_seas$graphIdx_st) & is.null(modData_seas$seasonID)){
      Mx_nz <- modData_seas %>%
        select(contains("X_"), contains("O_"), fips, fips_st, regionID, ID, logE, season) %>%
        mutate(intercept = 1)
    } else if(is.null(modData_seas$graphIdx_st) & !is.null(modData_seas$graphIdx) & is.null(modData_seas$seasonID)){
      Mx_nz <- modData_seas %>%
        select(contains("X_"), contains("O_"), fips, fips_st, regionID, ID, logE, season, graphIdx) %>%
        mutate(intercept = 1)
    } else if(!is.null(modData_seas$graphIdx_st) & is.null(modData_seas$graphIdx) & is.null(modData_seas$seasonID)){
      Mx_nz <- modData_seas %>%
        select(contains("X_"), contains("O_"), fips, fips_st, regionID, ID, logE, season, graphIdx_st) %>%
        mutate(intercept = 1)
    } else if(!is.null(modData_seas$graphIdx_st) & !is.null(modData_seas$graphIdx) & is.null(modData_seas$seasonID)){
      Mx_nz <- modData_seas %>%
        select(contains("X_"), contains("O_"), fips, fips_st, regionID, ID, logE, season, graphIdx, graphIdx_st) %>%
        mutate(intercept = 1)
    } else if(!is.null(modData_seas$graphIdx_st) & !is.null(modData_seas$graphIdx) & !is.null(modData_seas$seasonID)){
      Mx_nz <- modData_seas %>%
        select(contains("X_"), contains("O_"), fips, fips_st, regionID, ID, logE, season, graphIdx, graphIdx_st, seasonID) %>%
        mutate(intercept = 1)
    }
    colnames(Mx_nz) <- paste0(colnames(Mx_nz), "_nonzero")

    # convert matrix information to a list of lists/matrixes
    modData_seas_lists <- list()
    for (column in colnames(Mx_nz)){
      modData_seas_lists[[column]] <- Mx_nz[[column]]
    }
    # add Y response vector as a list
    modData_seas_lists[['Y']] <- Y_nz

    return(modData_seas_lists)
  }
  ################################
  substr.Right <- function(x, numchar){
    return(substr(x, nchar(x)-(numchar-1), nchar(x)))
  }

  #### SOURCE: clean and import model data #################################
  setwd(dirname(sys.frame(1)$ofile))
  source("source_export_inlaData_cty.R") # functions to plot county-specific model diagnostics
  source("source_export_inlaData.R") # functions to plot general model diagnostics
  source("source_export_inlaData_hurdle.R") # data export functions for hurdle model
  source("source_pp_checks.R") # export cpo & pit observations

  #### FILEPATHS #################################
  file_dataImport <- paste0(getwd(), "/../R_export/inlaModelData_import/inlaImport_model8a_iliSum_irDt_v7.csv")
  path_adjMxExport_cty <- paste0(getwd(), "/../reference_data/UScounty_shapefiles/US_county_adjacency.graph")

  #### MAIN #################################
  #### Import and process data ####
  modData_full <- read_csv(file_dataImport, col_types = cols(fips = col_character(), fips_st = col_character()))

  formula <- Y ~ -1 +
    f(ID_nonzero, model = "iid") +
    f(fips_nonzero, model = "iid") +
    f(graphIdx_nonzero, model = "besag", graph = path_adjMxExport_cty) +
    f(fips_st_nonzero, model = "iid") +
    f(regionID_nonzero, model = "iid") +
    f(season_nonzero, model = "iid") +
    intercept_nonzero + O_imscoverage_nonzero + O_careseek_nonzero + O_insured_nonzero + X_poverty_nonzero + X_child_nonzero + X_adult_nonzero + X_hospaccess_nonzero + X_popdensity_nonzero + X_housdensity_nonzero + X_vaxcovI_nonzero + X_vaxcovE_nonzero + X_H3A_nonzero + X_B_nonzero + X_priorImmunity_nonzero + X_humidity_nonzero + X_pollution_nonzero + X_singlePersonHH_nonzero + X_H3A_nonzero*X_adult_nonzero + X_B_nonzero*X_child_nonzero + offset(logE_nonzero)

  #### export formatting ####
  # diagnostic plot export directories
  setwd(dirname(sys.frame(1)$ofile))
  dir.create(sprintf("../graph_outputs/inlaModelDiagnostics/%s", modCodeStr), showWarnings = FALSE)
  setwd(sprintf("../graph_outputs/inlaModelDiagnostics/%s", modCodeStr))
  path_plotExport <- getwd()

  # csv file export directories
  setwd(dirname(sys.frame(1)$ofile))
  dir.create(sprintf("../R_export/inlaModelData_export/%s", modCodeStr), showWarnings = FALSE)
  setwd(sprintf("../R_export/inlaModelData_export/%s", modCodeStr))
  path_csvExport <- getwd()

  #### run models for all seasons ################################
  modData_hurdle <- convert_hurdleModel_nz_spatiotemporal(modData_full)

  mod <- inla(formula,
              family = "gaussian",
              data = modData_hurdle,
              control.fixed = list(mean = 0, prec = 1/100), # set prior parameters for regression coefficients
              control.predictor = list(compute = TRUE, link = rep(1, nrow(modData_full))),
              control.compute = list(dic = TRUE, cpo = TRUE, config = TRUE),
              control.inla = list(correct = TRUE, correct.factor = 10, diagonal = 0, tolerance = 1e-8), # http://www.r-inla.org/events/newfeaturesinr-inlaapril2015
              # control.mode = list(result = starting3, restart = TRUE),
              verbose = TRUE,
              keep = TRUE, debug = TRUE)


  #### model summary outputs ################################

  #### write DIC and CPO summaries in separate tables by season ####
  # file path
  path_csvExport_dic <- paste0(path_csvExport, sprintf("/modFit_%s.csv", modCodeStr))
  # DIC & CPO summary file formatting
  dicData <- unlist(c(modCodeStr, s, as.character(Sys.Date()), mod$dic$dic, sum(log(mod$cpo$cpo), na.rm=TRUE), sum(mod$cpo$failure, na.rm=TRUE), use.names=FALSE))
  dicData2 <- as.data.frame(matrix(dicData, nrow = 1), byrow = TRUE)
  names(dicData2) <- c("modCodeStr", "season", "exportDate", "DIC", "CPO", "cpoFail")
  # write DIC & CPO summaries
  export_DIC(path_csvExport_dic, dicData2)

  #### write DIC and CPO for individual observations ####
  # file path
  path_csvExport_cpoPIT <- paste0(path_csvExport, sprintf("/cpoPIT_observations_%s.csv", modCodeStr))
  # write CPO and PIT for each observation to file
  export_cpoPIT_observations(path_csvExport_cpoPIT, mod)

  #### write random and group effect identities ####
  # file path
  path_csvExport_ids <- paste0(path_csvExport, sprintf("/ids_%s.csv", modCodeStr))
  # write identity codes to file
  export_ids(path_csvExport_ids, modData_full)
  
  #### write fixed and random effects summary statistics ####
  # file path
  path_csvExport_summaryStats <- paste0(path_csvExport, sprintf("/summaryStats_%s.csv", modCodeStr))
  # write all summary statistics to file
  export_summaryStats_hurdle_likString(path_csvExport_summaryStats, mod, rdmFx_RV, modCodeStr, dbCodeStr, s, likString) # assuming hyperpar, fixed always exist
  
  
  #### process fitted values for each model ################################
  # normal model processing
  path_csvExport_fittedNonzero <- paste0(path_csvExport, sprintf("/summaryStatsFitted_%s_%s.csv", likString, modCodeStr))
  dummy_nz <- mod$summary.fitted.values[1:nrow(modData_full),]
  mod_nz_fitted <- export_summaryStats_fitted_hurdle(path_csvExport_fittedNonzero, dummy_nz, modData_full, modCodeStr, dbCodeStr, s)
  
  #### draw sample posteriors ################################
  path_csvExport_posteriorSamples <- paste0(path_csvExport, sprintf("/posteriorSamples_%s_%s.csv", likString, modCodeStr))
  export_posterior_samples(path_csvExport_posteriorSamples, mod)
  
  #### Diagnostic plots ################################
  #### normal likelihood figures ####
  # marginal posteriors: first 6 county random effects (nu or phi)
  path_plotExport_rdmFxSample_nonzero <- paste0(path_plotExport, sprintf("/inla_%s_%s1-6_marg_%s.png", modCodeStr, rdmFx_RV, likString))
  plot_rdmFx_marginalsSample(path_plotExport_rdmFxSample_nonzero, mod$marginals.random$fips_nonzero, "nu")
  
  # marginal posteriors: first 6 observation error terms
  path_plotExport_rdmFxSample_nonzero <- paste0(path_plotExport, sprintf("/inla_%s_ID1-6_marg_%s.png", modCodeStr, likString))
  plot_rdmFx_marginalsSample(path_plotExport_rdmFxSample_nonzero, mod$marginals.random$ID_nonzero, "obs err")
  
}



