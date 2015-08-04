
## Name: Elizabeth Lee
## Date: 8/4/15
## Function: Compare Mantel test implementations in multiple packages (ade4, ecodist, vegan). ecodist implementation of Mantel test should be preferred in future studies.
## code copied from "analyze_dbMetricsDistance_IR.R" for testing purposes
## Filenames: dbMetrics_periodicReg_%s_analyzeDbdist.csv
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

setwd('/home/elee/R/source_functions')
source("dfsumm.R")
require(dplyr)
require(tidyr)
require(readr)

####################################
# set these!
code = "t2_" # parabolic time trend term
####################################
# import data
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')
data <- read_csv(sprintf('dbMetrics_periodicReg_%sanalyzeDBdist.csv', code), col_types=list(zipname=col_character()))

####################################
# local functions
filterOutcomeData <- function(dataset, seasonnum, metricchar){
  subset <- dataset %>% filter(season==seasonnum & metric==metricchar) %>% select(burden)
  outcome.dists <- dist(subset, upper=T)
  return(outcome.dists)
}
####################################
# count zip3s per season
zip3cts <- data %>% group_by(season, metric) %>% summarise(ct = length(zipname))
# View(zip3cts) # number of zip3s varies each season

# CONSEQUENTLY, filter data to include only zip3s with db metrics for every season
zip3s_toinclude <- data %>% group_by(zipname) %>% summarise(ct = length(burden)) %>% filter(ct == 45) 
# View(zip3s_toinclude) # 337 zip3s
data2 <- ungroup(data) %>% filter(zipname %in% zip3s_toinclude$zipname)

# # review spatial distribution in new (smaller) dataset
# # 42 states are represented; states missing data include: AK, AZ, HI, MT, ND, NM, UT, VT, (DC)
# View(data2 %>% select(STATE) %>% distinct %>% arrange(STATE))

####################################
# create spatial distance matrix (applies across all seasons bc distances don't change)
forDistMx <- data2 %>% filter(season==1 & metric=="IR.mean") %>% select(long, lat)
zip3_dists <- dist(forDistMx, upper=T)

####################################
# set seed 1) spatial vs outcome distances
set.seed(19)
####################################
# compare mantel test times and results among different implementations
# require(ade4)
# require(vegan)
require(ecodist)

metriclist <- data2 %>% select(metric) %>% distinct %>% unlist
for (s in 4:4){
  for (metric in metriclist){
    print(sprintf("Here are the results for S%s %s vs. spatial distances:", s, metric))
    dummy <- filterOutcomeData(data2, s, metric) # create outcome matrix
#     time <- system.time(results <- mantel.rtest(zip3_dists, dummy, nrepet = 1000)) # ade4
#     time <- system.time(results <- mantel(zip3_dists, dummy, permutations = 1000)) # vegan
    time <- system.time(results <- mantel(zip3_dists ~ dummy, nperm = 1000)) # ecodist
    print(time)
  }
}
# 8/4/15 Conclusions
# all packages seem to be performing the same evaluation of matrices
# fastest to slowest: ecodist (~0.5 sec) > ade4 (~2.6 sec) > vegan (~3.1 sec)
# features: ecodist documentation denotes the hypothesis test and multiple p-values depending on the null
# ecodist seems to be the clear winner in terms of speed, reported results, and documentation

