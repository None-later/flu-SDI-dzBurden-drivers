
## Name: Elizabeth Lee
## Date: 7/28/15
## Function: compare distance matrixes; 1) spatial vs outcome distances for each season and outcome metric; 2) outcome distances across seasons for each outcome metric
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
require(ade4)
require(readr)

####################################
# set these!
# code = "t2sa_" # semi-annual periodicity
code = "t2_" # parabolic time trend term
# code="" # linear time trend term
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
# checked that zipnames are ordered across seasons and metrics for a small sample

####################################
# set seed 1) spatial vs outcome distances
set.seed(19)
####################################
# 1) spatial vs outcome distances for each season and outcome metric
# comparing observations and simulated p-values from the Monte Carlo Mantel test, the observations (correlations) are the same, but the simulated p-values changed as repetition numbers changed
# for a couple of examples, p-values stabilized when nrepet was roughly 1000

seasonlist <- seq(1, 9, by=1)
metriclist <- data2 %>% select(metric) %>% distinct %>% unlist
for (s in seasonlist){
  for (metric in metriclist){
    print(sprintf("Here are the results for S%s %s vs. spatial distances:", s, metric))
    dummy <- filterOutcomeData(data2, s, metric) # create outcome matrix
    results <- mantel.rtest(zip3_dists, dummy, nrepet = 1000)
    print(results)
    print("-------------------------------------------")
    # plot(results)
  }
} # 7/28/15: Results recorded in:
# /home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export/
# file: results_MantelTest.ods$outcome_spatial
# IR.mean and epi.dur tended to be 'significant' more frequently than other metrics
# all metrics in season 6 were significant
# matrix correlations were very small across the board (no values surpassed 0.2 in magnitude)

####################################
# set seed 2) outcome distances across seasons
set.seed(220)
####################################
# 2) outcome distances across seasons for each outcome metric
metriclist <- data2 %>% select(metric) %>% distinct %>% unlist

metric <- metriclist[1] # IR.mean, IR.excess.BL, IR.excess.thresh, IR.peak, epi.dur
refSeason <- 1
ref_dist <- filterOutcomeData(data2, refSeason, metric)
for (s in (refSeason+1):9){
    print(sprintf("Here are the results for S%s vs. S%s (metric = %s):", refSeason, s, metric))
    dummy2 <- filterOutcomeData(data2, s, metric) # create outcome matrix
    results2 <- mantel.rtest(ref_dist, dummy2, nrepet = 1000)
    print(results2)
    print("-------------------------------------------")
    # plot(results2)
} # 7/28/15 Results recorded in:
# /home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export/
# file: results_MantelTest.ods$outcome_byseason
# IR.mean had clear temporal autocorrelation: adjacent seasons had the highest corr and all values were significant; coefficients increased in later seasons as well, which suggested that perhaps the increase in reporting played a role?
# IR.excess.BL had temporal autocorrelation as well, but it was not as strong as that for IR.mean; all values were significant and coefficients were in the moderately strong positive range; coefficients increased in later seasons
# IR.excess.thresh had similar results to IR.excess.BL; temporal autocorrelation; all values significant; coefficients moderately strong positive; coefficients increased in later seasons
# IR.peak had similar results; temporal autocorrelation; all values significant; coefficients strongly positive; coefficients increased in later seasons
# epi.dur; warning "In is.euclid(m1) : Zero distance(s)" appeared meaning that some rows were duplicates and their distance was 0; temporal cutocorrelation not evident; coefficients were very small across the board (no greater than 0.2 in magnitude for any pairs); fewer matches achieved significant correlations
