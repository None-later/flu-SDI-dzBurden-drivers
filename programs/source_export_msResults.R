
## Name: Elizabeth Lee
## Date: 1/16/17
## Function: general functions to report MS results (non-figures)
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")
setwd(dirname(sys.frame(1)$ofile))
require(readr); require(dplyr); require(tidyr)
require(DBI); require(RMySQL) # read tables from mysql database
source("source_export_msFigs.R")

#### import data ################################
string_fit_fname <- function(modCodeStr){
  if(substring(modCodeStr, 2, 2) == 'a'){
    fname <- paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_export/", modCodeStr, "/summaryStatsFitted_normal_", modCodeStr, ".csv")
  } else if (substring(modCodeStrd, 2, 2) == 'e'){
    fname <- paste0(dirname(sys.frame(1)$ofile), "/../R_export/inlaModelData_export/", modCodeStr, "/summaryStatsFitted_poisson_", modCodeStr, ".csv")
  }
  return(fname)
}


#### reports ################################
################################
report_observed_predicted_quantile_matches <- function(modCodeStr, quantileStep){
	# report percentage of observed and predicted mean 'quantileSteps' (e.g., deciles) matched across the ddataset

  fittedDatFname <- string_fit_fname(modCodeStr)
	fittedDat <- read_csv(fittedDatFname)
	obs <- fittedDat$y1
	pred <- fittedDat$mean
	
	# find quantile cutpoints
	obsQuant <- quantile(obs, probs = seq(0, 1, quantileStep), type = 9, na.rm = TRUE)
	predQuant <- quantile(fittedDat$mean, probs = seq(0, 1, quantileStep), type = 9, na.rm = TRUE)

	# bin data according to quantile cutpoints
	obsBin <- cut(obs, obsQuant, include.lowest = TRUE, labels = FALSE)
	predBin <- cut(pred, predQuant, include.lowest = TRUE, labels = FALSE)

	# bind bins to new DF 
	mergDat <- tbl_df(data.frame(obsBin = obsBin, predBin = predBin)) %>%
	filter(!is.na(obsBin)) %>%
	mutate(match = ifelse(obsBin == predBin, 1, 0))

	# calculate percentage match
	matchPerc <- sum(mergDat$match)/nrow(mergDat)*100
	print(sprintf("For %s, %s percent of %s step quantiles match between observations and predicted means.", modCodeStr, matchPerc, quantileStep))
}

################################
report_observed_predicted_pearsonCorr <- function(modCodeStr){
	# report Pearson's R between observed and predicted mean 

  fittedDatFname <- string_fit_fname(modCodeStr)
	fittedDat <- read_csv(fittedDatFname)
	obs <- fittedDat$y1
	pred <- fittedDat$mean
	
	pearson <- cor(obs, pred, use = "na.or.complete", method = "pearson")

	print(sprintf("For %s, Pearson's R = %s between observations and predicted means.", modCodeStr, pearson))
}
################################
report_total_ILI_claimsILINetVisits <- function(){
  # report summary statistics of total and ILI medical claims and ILINet visits

  # claims data
  con <- dbConnect(RMySQL::MySQL(), group = "rmysql-sdi")
  dbListTables(con)
  
  dbListFields(con, "flu")
  sel.statement <- "SELECT WEEK AS week, PATIENT_ZIP3 AS zip3, sum(ILI_m) AS ili, sum(ANY_DIAG_VISIT_CT) AS visits FROM flu WHERE SERVICE_PLACE = 'TOTAL' AND patient_zip3 = 'TOT' AND (MONTH(week) <= 4 OR MONTH(week) >= 11) AND AGEGROUP = 'TOTAL' GROUP BY WEEK, PATIENT_ZIP3"
  dummy <- dbGetQuery(con, sel.statement)
  
  dbDisconnect(con)

  summDat <- dummy %>%
    mutate(week = as.Date(week)) %>%
    mutate(season = ifelse(lubridate::month(week) >= 11, lubridate::year(week)-2000+1, lubridate::year(week)-2000)) %>%
    group_by(season) %>%
    summarise(claims_ili = sum(ili), claims_visits = sum(visits))
  
  # ilinet data
  dummy2 <- read_csv(paste0(herepath, "/../../../CDC_Source/Import_Data/all_cdc_source_data.csv"))
  
  summDat2 <- dummy2 %>%
    filter(wk >= 44 | wk < 16) %>%
    group_by(season) %>%
    summarise(ilinet_ili = sum(ilitot, na.rm=TRUE), ilinet_visits = sum(patients, na.rm=TRUE))
  
  fullDat <- full_join(summDat, summDat2, by = "season") %>%
    mutate(iliPerc = ilinet_ili/claims_ili*100) %>%
    mutate(vizPerc = ilinet_visits/claims_visits*100) %>%
    mutate(iliTimes = claims_ili/ilinet_ili) %>%
    mutate(vizTimes = claims_visits/ilinet_visits) %>%
    filter(season >= 3)
  print(fullDat)
  
  return(fullDat)
}
################################
report_percentCtyMatch_replicates <- function(baseCodeLs, pltFormats){
  
  stopifnot(length(baseCodeLs) >= 2L)

  # data formatting
  numReplicates <- pltFormats$numReplicates
  repcodelength <- pltFormats$repcodelength
  
  ## grab fitData from multiple models ##
  fullDf <- tbl_df(data.frame(modCodeStr = c(), season = c(), fips = c(), LB = c(), UB = c()))
  
  # import complete model fitData
  completeCode <- baseCodeLs[which(nchar(baseCodeLs)==min(nchar(baseCodeLs)))]
  completeDat <- read_csv(string_fit_fname(completeCode), col_types = "c_d_c_dd______") %>%
      mutate(LB = mean-(1*sd), UB = mean+(1*sd)) %>% # 5/15/17 okay approximation because we are looking at overlap between posteriors (not 95% CI)
      select(modCodeStr, season, fips, LB, UB)

  # import replicates for each baseCode
  baseRepCodeLs <- baseCodeLs[which(nchar(baseCodeLs)!=min(nchar(baseCodeLs)))]
  for (baseRepCode in baseRepCodeLs){
    repCodeLs <- c(paste0(baseRepCode, ""), paste(baseRepCode, 1:(numReplicates-1), sep = "-"))
    modDat <- import_fitReplicates(repCodeLs) 
    fullDf <- bind_rows(fullDf, modDat)
    print(paste(baseRepCode, "imported"))
  }

  # clean and organize bound data  
  prepDat <- bind_rows(fullDf, completeDat) %>%
    gather(bound, value, LB:UB) %>%
    mutate(bound_spread = paste(modCodeStr, bound, sep = "_")) %>%
    arrange(modCodeStr, bound) 

  # create bound dataframe
  boundLs <- prepDat %>% distinct(bound_spread) %>% unlist
  boundMx <- matrix(boundLs, ncol=2, byrow = TRUE)

  # identify overlaps with complete model
  spreadDat <- prepDat %>%
    select(-bound, -modCodeStr) %>%
    spread(bound_spread, value) 

  # indicate overlap with all replicates
  overlapDat <- spreadDat
  for (i in 2:nrow(boundMx)){
    newcol <- paste0("o_", substring(boundMx[i,1], 16, nchar(boundMx[i,1])-3))
    overlapDat <- do.call(overlapping_intervals, list(df = overlapDat, intervalA_LB = boundMx[1,1], intervalA_UB = boundMx[1,2], intervalB_LB = boundMx[i,1], intervalB_UB = boundMx[i,2])) %>%
      rename_(.dots = setNames("overlap", newcol))
  }
  
  # summarise across replicates and seasons
  seasRepcodeDat <- overlapDat %>%
    select(season, fips, contains("o_")) %>%
    gather(repcode, overlap, contains("o_")) %>%
    mutate(repgroup = substring(repcode, 1, repcodelength)) %>%
    group_by(season, repcode) %>%
    summarise(repgroup = first(repgroup), overlapCty = sum(as.numeric(overlap)), totCty = length(overlap)) %>%
    mutate(percMatch = overlapCty/totCty*100) 

  # summarise across season, repgroup -- avg percent match
  repgroupDat <- seasRepcodeDat %>%
    group_by(repgroup) %>%
    summarise(percMatch = mean(percMatch))

  print(repgroupDat)
  return(repgroupDat)
}


################################
## MAIN ##
setwd(dirname(sys.frame(1)$ofile))
herepath <- getwd()
setwd("../R_export/msReports")
sink("msReport.txt", append = FALSE)

# modCodeLs <- c("8a_iliSum_v2-6", "8a_iliSum_v3-6", "8a_iliSum_v4-6", "8e_epiDur_v2-3")
# qstep <- 0.2

# for (code in modCodeLs){

# 	report_observed_predicted_quantile_matches(code, qstep)
# 	report_observed_predicted_pearsonCorr(code)
# }

fullDat <- report_total_ILI_claimsILINetVisits()

## county missing sequence
baseCtySeq <- c("8a_iliSum_v2-6", "8a_iliSum_v2-6_c80", "8a_iliSum_v2-6_c60", "8a_iliSum_v2-6_c40", "8a_iliSum_v2-6_c20")
ctyPlotFormats <- list(lvls = baseCtySeq[2:length(baseCtySeq)],  labs = c("80% of counties", "60% of counties", "40% of counties", "20% of counties"), descrip = "ctySeq", repcodelength = 5, numReplicates = 10)
ctySeq <- report_percentCtyMatch_replicates(baseCtySeq, ctyPlotFormats)

## random missing sequence
baseMissSeq <- c("8a_iliSum_v2-6", "8a_iliSum_v2-6_m20", "8a_iliSum_v2-6_m40", "8a_iliSum_v2-6_m60", "8a_iliSum_v2-6_m80")
missPlotFormats <- list(lvls = baseMissSeq[2:length(baseMissSeq)], labs = c("missing 20%", "missing 40%", "missing 60%", "missing 80%"), descrip = "missSeq", repcodelength = 5, numReplicates = 10)
missSeq <- report_percentCtyMatch_replicates(baseMissSeq, missPlotFormats)

## season missing sequence
baseSeasSeq <- c("8a_iliSum_v2-6", "8a_iliSum_v2-6_s6", "8a_iliSum_v2-6_s4", "8a_iliSum_v2-6_s2")
seasPlotFormats <- list(lvls = baseSeasSeq[2:length(baseSeasSeq)], labs = c("missing 1", "missing 3","missing 5"), descrip = "seasSeq", repcodelength = 4, numReplicates = 10)
seassSeq <- report_percentCtyMatch_replicates(baseSeasSeq, seasPlotFormats)

sink(type = "message")
sink()
file.show("msReport.txt")

