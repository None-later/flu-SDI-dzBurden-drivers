
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

require(readr); require(dplyr); require(tidyr)
require(DBI); require(RMySQL) # read tables from mysql database


#### functions ################################

string_fit_fname <- function(modCodeStr){
  if(substring(modCodeStr, 2, 2) == 'a'){
    fname <- sprintf("/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export/inlaModelData_export/%s/summaryStatsFitted_normal_%s.csv", modCodeStr, modCodeStr)  
  } else if (substring(modCodeStr, 2, 2) == 'e'){
    fname <- sprintf("/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export/inlaModelData_export/%s/summaryStatsFitted_poisson_%s.csv", modCodeStr, modCodeStr) 
  }
  return(fname)
}

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
## MAIN ##
setwd(dirname(sys.frame(1)$ofile))
setwd("../R_export/msReports")
sink("msReport.txt", append = FALSE)

modCodeLs <- c("8a_iliSum_v2-6", "8a_iliSum_v3-6", "8a_iliSum_v4-6", "8e_epiDur_v2-3")
qstep <- 0.2

for (code in modCodeLs){
	
	report_observed_predicted_quantile_matches(code, qstep)
	report_observed_predicted_pearsonCorr(code)
}


