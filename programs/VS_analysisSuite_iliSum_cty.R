
## Name: Elizabeth Lee
## Date: 6/2/16
## Function: EDA suite of variable selection analyses for iliSum at county level
## Filenames: dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB.csv, source_clean_data_function.R
## Data Source: 
## Notes: before running singleVarWrite and singleVarPlot singly, DELETE ALL FILES WITH PATTERN: sprintf("VS_coefDat_%s_cty_pt#", rCode)
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
# rm(list = ls())
require(dplyr); require(tidyr); require(readr) # clean_data_functions dependencies
require(RColorBrewer); require(ggplot2)

#### set these! ################################
agecode <- ""
dbCodeStr <- sprintf("_ilinDt_Octfit%s_span0.4_degree2", agecode)
rCode <- "iliSum" # epiDur, iliSum
seasons <- 3:9
analysesOn <- c('loadData', 'singleVarWrite', 'singleVarPlot') 
# 'loadData', 'dataQuality', 'pairwise', 'singleVarWrite', 'singleVarPlot'
type_cleanDataFxns <- ''
# '' or '_nofill

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)
source(sprintf("source_clean_data_functions%s.R", type_cleanDataFxns)) # functions to clean covariate data
source("source_variableSelection_cty.R") # functions for variable selection analyses specific to county scale
source("source_variableSelection.R") # functions for variable selection, generally
source("source_prepare_inlaData_cty.R") # import prepared inla data
source("source_export_inlaDiagnostics.R") # clean_RVnames
source("source_export_msFigs.R") # label_tot_predictors

#### FILEPATHS #################################
setwd('../reference_data')
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")

setwd('./UScounty_shapefiles')
path_adjMxExport_cty <- paste0(getwd(), "/US_county_adjacency.graph")
path_graphIdx_cty <- paste0(getwd(), "/US_county_graph_index.csv")
path_shape_cty <- paste0(getwd(), "/gz_2010_us_050_00_500k") # for dbf metadata only

setwd('../stateFlightpassenger_graph')
path_graphExport_st <- paste0(getwd(), "/US_statePassenger_edgelist.txt")
path_graphIdx_st <- paste0(getwd(), "/US_statePassenger_graph_index.csv")

setwd("../../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))
fname_coefDat <- sprintf("/VS_coefDat_%s_cty%s", rCode, type_cleanDataFxns)
path_coefDat <- paste0(getwd(), fname_coefDat)
# path_tempDatQuality <- paste0(getwd(), sprintf("/VS_tempDatQuality_%s_cty%s.csv", rCode, type_cleanDataFxns))

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_shape_cty = path_shape_cty,
                  path_adjMxExport_cty = path_adjMxExport_cty,
                  path_response_cty = path_response_cty, 
                  path_graphIdx_cty = path_graphIdx_cty, 
                  path_graphExport_st = path_graphExport_st,
                  path_graphIdx_st = path_graphIdx_st)

setwd(dirname(sys.frame(1)$ofile))
setwd("../graph_outputs")
path_pltExport <- paste0(getwd(), "/VS_analysisSuite_iliSum_cty_noRdm")

#### PLOT FORMATTING ################################
w <- 7.5; h <- 7.5; dp <- 300
w2 <- 6; h2 <- 3
setwd(path_pltExport)

#### MAIN #################################################################

#### Import and process response & covariate data ####################################
if("loadData" %in% analysesOn){
  
  # load data frame with all available cleaned variables
  # allDat <- prepare_allCov_iliSum_cty(path_list) # VS all variables
  allDat <- model8a_iliSum_v7(path_list) # total pop data
  # allDat <- model8a_iliSum_v3(path_list) # child pop data
  # allDat <- model8a_iliSum_v4(path_list) # adult pop data
  # allDat <- model8e_epiDur_v7(path_list)
  summary(allDat)
  
} # end loadData

#### Check data quality ####################################
if("dataQuality" %in% analysesOn){
  
  # output county counts by season for each variable: VS_tempDatQuality_%s_cty.csv
  # check counts in each column
  dataQuality <- allDat %>% group_by(season) %>% summarise_each(funs(ct = sum(!is.na(.))))
  write_csv(dataQuality, path_tempDatQuality)
  
} # end dataQuality

#### Pairwise variable comparisons ####################################
if("pairwise" %in% analysesOn){
  
  # # full scatterplot matrix
  # png(sprintf("scatterMx_%s_cty%s%s.png", rCode, dbCodeStr, type_cleanDataFxns), width = w, height = h, units = "in", res = dp)
  # scatterMx <- pairs_scatterplotMatrix(allDat)
  # print(scatterMx)
  # dev.off()

  # full correlation matrix
  png(sprintf("corrMx_spearman_%s_cty%s%s.png", rCode, dbCodeStr, type_cleanDataFxns), width = w, height = h, units = "in", res = dp)
  corrMx <- pairs_corrMatrix(allDat)
  print(corrMx)
  dev.off()
  
} # end pairwise

#### Single variable models - Write to file ####################################
if("singleVarWrite" %in% analysesOn){
  
  modDat <- convert_hurdleModel_nz_spatiotemporal(allDat)
  
  num <- 1
  varlist <- grep("[OX]{1}[_]{1}", names(modDat), value = TRUE)  # grab all varnames
  indexes <- seq(1, length(varlist), by=num)

  # for(i in indexes){
  #   # 6/2/16: grab list of variables to export model data in pieces -- kept crashing before
  #   if((i+num-1) > length(varlist)){
  #     varsublist <- varlist[i:length(varlist)]
  #   } else{
  #     varsublist <- varlist[i:(i+num-1)]
  #   }
    # generate empty data frame to store coefficient data
    coefDat <- tbl_df(data.frame(respCode = c(), RV = c(), exportDate = c(), mean = c(), sd = c(), LB = c(), UB = c(), DIC = c()))
    # loop through all variables and seasons
    for (varInterest in varlist){
      modDat[['varInterest']] <- modDat[[varInterest]]
      coefRow <- model_singleVariable_inla_noRdm(modDat, rCode, s, varInterest) # N.B. model includes intercept
      # append to model data object
      coefDat <- bind_rows(coefDat, coefRow)
    } # end for varlist
    
    # write to file in parts
    write_csv(coefDat, sprintf("%s%s_pt%s.csv", path_coefDat, agecode, 1)) 
  # }

} # end singleVarWrite

#### Single variable models - plot coef ####################################
if("singleVarPlot" %in% analysesOn){
  setwd(dirname(sys.frame(1)$ofile))
  setwd("../R_export")
  
  # pull together all relevant file names since data were exported in pieces
  allFiles <- list.files()
  pattern <- substring(sprintf("%s%s_pt", fname_coefDat, agecode), 2, nchar(sprintf("%s%s_pt", fname_coefDat, agecode)))
  fnamels <- grep(pattern, allFiles, value = TRUE)
  
  # bind all of the data pieces together
  coefDat <- tbl_df(data.frame(respCode = c(), RV = c(), exportDate = c(), mean = c(), sd = c(), LB = c(), UB = c(), DIC = c()))
  for (fname in fnamels){
    newDat <- read_csv(fname)
    coefDat <- bind_rows(coefDat, newDat)
  }
  
  totLabels <- label_tot_predictors()
  coefDat2 <- coefDat %>% 
    calculate_95CI(.) %>%
    clean_RVnames(.) %>%
    mutate(RV = factor(RV, levels = totLabels$RV, labels = totLabels$pltLabs))
  
  setwd(path_pltExport)
  plt_coefTime <- plot_singleVarCoef(coefDat2)

  ggsave(sprintf("singleVarCoef_%s_cty%s%s.png", rCode, type_cleanDataFxns, agecode), width = w2, height = h2, dpi = dp)
}


