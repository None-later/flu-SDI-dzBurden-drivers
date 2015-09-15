
## Name: Elizabeth Lee
## Date: 9/15/15
## Function: write relative magnitude of disease burden with ILI-Oct data; data processing: Zip3-season combinations with equal or more consecutive weeks above the epidemic threshold in the non-flu period than the flu period are filtered out (see explore_fluSeasonDefinition_ILI.R, Analysis 3 for additional details)
### disease burden metrics: total ili summed across epidemic weeks, cumulative difference in ili and baseline, cumulative difference in ili and epidemic threshold, rate of ili at epidemic peak, epidemic duration, weeks from Nov1 to epidemic start, weeks from epidemic start to peak ili week 

## Filenames: periodicReg_%sallZip3Mods_ILI_Oct.csv
## Data Source: IMS Health 
## Notes: 9/15/15 - Refer to explore_fluSeasonDefinition_ILI.R for definition of "flu epidemic". Zip3s are considered to have experienced a flu epidemic if they had 4+ consecutive weeks above the epidemic threshold in the flu period. 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")


#### header ####################################
setwd('~/Dropbox/code')
source("GeneralTools.R")
require(dplyr)
require(ggplot2)
require(readr)
require(ISOweek)
require(tidyr)
setwd(dirname(sys.frame(1)$ofile))

#### local functions ####################################
# return logical column; T if data should be considered as "flu season" (ie. data falls within period with maximum consecutive weeks)
consider.flu.season <- function(x){
  rle.results = rle(x)
  max.consec = max(0, rle.results$lengths[which(rle.results$values)])
  dummy <- rep(F, length(x))
  pre.index <- (which(rle.results$values & rle.results$lengths==max.consec))[1]
  post.index <- tail((which(rle.results$values & rle.results$lengths==max.consec)), n=1)
  converted.pre.index <- ifelse(pre.index==1, 0, sum(rle.results$lengths[1:(pre.index-1)]))
#   converted.post.index <- converted.pre.index + max.consec
#   print(rle.results)
#   print(pre.index)
#   print(which(rle.results$values & rle.results$lengths==max.consec))
  converted.post.index <- ifelse(post.index, sum(rle.results$lengths[1:post.index]), NA)
  if(!is.na(converted.pre.index)){
    dummy[(converted.pre.index+1):converted.post.index] <- T
  }
  return(dummy)
}

#### set these! ####################################
# code = "t2sa_" # semi-annual periodicity
code = "t2_" # parabolic time trend term
# code="" # linear time trend term
code2 = "_Oct"

# # commented out on 9/15 after data was exported
# #### data processing (based on explore_fluSeasonDefinition_ILI.R) ####################################
# setwd('../R_export')
# data <- read_csv(file=sprintf('periodicReg_%sallZip3Mods_ILI%s.csv', code, code2), col_types=list(ili=col_double(), .fitted=col_double(), .se.fit=col_double()))
# 
# # 1) add ISO week numbers; 2) add season numbers ; 3) add real zip3 names
# data2 <- data %>% mutate(wknum = as.numeric(substr.Right(ISOweek(Thu.week), 2))) %>% mutate(season = ifelse(wknum<40, as.integer(substr(Thu.week, 3, 4)), as.integer(substr(Thu.week, 3, 4))+1)) %>% mutate(zipname = substr.Right(gsub("X", "00", zip3), 3))
# 
# # 1) include only zip3s where lm was performed; 2) set .fitted + 1.96*.se.fit as the epidemic threshold; 3) identify which weeks are epi weeks
# data3 <- data2 %>% filter(incl.lm)  %>% mutate(epi.thresh = .fitted+(1.96*.se.fit)) %>% mutate(epi.week = ili>epi.thresh)
# 
# ## See explore_fluSeasonDefinition_IR.R for derivation of flu season definition
# # 9/15/15: filter out zip3-season combinations with equivalent or more ILI activity in the non-flu season than flu season
# dummy.flu <- data3 %>% filter(flu.week) %>% group_by(season, zipname) %>% summarise(consec.flu.epiweeks = rle.func(epi.week))
# dummy.nf <- data3 %>% filter(!flu.week) %>% group_by(season, zipname) %>% summarise(consec.nf.epiweeks = rle.func(epi.week))
# # summarize season-zip3 combinations that have epidemics (4+ consecutive epidemic weeks)
# zip3s_with_epi <- right_join(dummy.flu, dummy.nf, by=c("season", "zipname")) %>% mutate(incl.analysis = consec.flu.epiweeks > consec.nf.epiweeks) %>% mutate(has.epi = (consec.flu.epiweeks>=4))
# 
# # join summary data to full dataset (adds has.epi and incl.analysis indicators)
# data4 <- right_join(data3, zip3s_with_epi %>% select(-contains("consec.")), by=c("season", "zipname"))
# # 9/15/15: in.season indicator: must meet flu.week, has.epi, incl.analysis, and consecutive epi.week criteria (FLU PERIOD DATA ONLY)
# data5 <- data4 %>% filter(flu.week & has.epi & incl.analysis) %>% group_by(season, zipname) %>% mutate(in.season = consider.flu.season(epi.week))
# data6 <- left_join(data4, (data5 %>% ungroup %>% select(Thu.week, zipname, in.season)), by = c("Thu.week", "zipname")) %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01")) %>% filter(has.epi & incl.analysis)
# 
# # save to file (9/15/15)
# # these data are saved for reference
# write.csv(data5, file = sprintf('fullIndicFlu_periodicReg_%sILI%s_analyzeDB.csv', code, code2), row.names=FALSE)
# write.csv(data6, file = sprintf('fullIndicAll_periodicReg_%sILI%s_analyzeDB.csv', code, code2), row.names=FALSE)

#### read saved data back in ##################
# 9/15/15 read data back in
setwd('../R_export')
data5 <- read_csv(sprintf('fullIndicFlu_periodicReg_%sILI%s_analyzeDB.csv', code, code2), col_names = T, col_types = list(zipname=col_character()))

#### create dz burden metrics ##################
# 7/27/15 update dz burden metrics (\cite{Viboud2014} for inspiration of 1b & 1c)
# create disease burden metrics: 1a) sum ILI across epidemic weeks (overall magnitude), 1b) cumulative difference in ILI and baseline (second proxy of overall magnitude), 1c) cumulative difference in ILI and epidemic threshold (third proxy of overall magnitude), 2) rate of ILI at epidemic peak, 3) epidemic duration
dbMetrics <- data5 %>% group_by(season, zipname) %>% filter(in.season) %>% summarize(ili.sum = sum(ili, na.rm=T), ili.excess.BL = sum(ili-.fitted, na.rm=T), ili.excess.thresh = sum(ili-epi.thresh, na.rm=T), ili.peak = max(ili, na.rm=T), epi.dur = sum(in.season))

# 8/6/15 create epidemic start timing and peak timing metrics
# data processing in preparation for timing metrics
data7 <- data5 %>% select(Thu.week, t, ili, season, zipname, flu.week, in.season)
createTiming1 <- data7 %>% group_by(season, zipname) %>% mutate(t.minweek = ifelse(Thu.week==min(Thu.week), t, 0)) %>% select(Thu.week, season, zipname, t.minweek) %>% ungroup
createTiming2 <- data7 %>% group_by(season, zipname) %>% filter(in.season) %>% mutate(t.firstepiweek = ifelse(Thu.week==min(Thu.week, na.rm=T), t, 0)) %>% mutate(t.peakweek = ifelse(ili==max(ili, na.rm=T), t, 0)) %>% ungroup %>% select(Thu.week, zipname, t.firstepiweek, t.peakweek)
createTiming <- left_join(createTiming1, createTiming2, by=c("Thu.week", "zipname")) %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01"))
# create dataset with metrics
# 4) wks.to.epi = # weeks between start of flu period and start of epidemic; 5) wks.to.peak = # weeks between start of epidemic and peak IR week
dbMetrics.timing <- createTiming %>% group_by(season, zipname) %>% summarise(minweek = max(t.minweek), firstepiweek = max(t.firstepiweek, na.rm=T), peakweek = max(t.peakweek, na.rm=T)) %>% mutate(wks.to.epi = 1 + firstepiweek - minweek) %>% mutate(wks.to.peak = 1 + peakweek - firstepiweek) 
# merge timing metrics with other dz burden metrics
dbMetrics2 <- full_join(dbMetrics, dbMetrics.timing, by=c("season", "zipname")) %>% select(-minweek, -firstepiweek, -peakweek)

dbMetrics.g <- gather(dbMetrics2, metric, burden, 3:9)
# mean and sd for each metric by season for viewing
dbMetrics_summary <- dbMetrics.g %>% group_by(season, metric) %>% summarize(metric.mn = mean(burden), metric.sd = sd(burden))

# import zip3 lat/long coordinate data
setwd('../reference_data')
import <- read_csv(file='Coord3digits.csv')
coordsData <- tbl_df(import) %>% select(zip3, STATE, st_FIPS, pop, lat, long, w_lat, w_long) %>% mutate(zipname = substr.Right(paste0("00", zip3), 3))

#### coordsData checks ##################
# # performed checks on coordsData (7/28/15; holdover from _IR program version)
# # 1) state-zip3 assignment: cross-referenced a sample of zip3-state combinations using zip code prefixes page on wikipedia
# View(coordsData %>% select(zip3, STATE))
# # 2) zip3-latlong assignment: cross-referenced a sample of zip3-latlong combinations using zip3 centroids google fusion table (Id: http://maps.huge.info/zip3.htm)
# # coordinates are similar, but I don't understand the differences between the lat/long and w_lat/w_long variables (perhaps regular lat/long centroids or population-weighted lat/long centroids). Nevertheless, the coordinate decimal places are not exact, so I'm not sure about the accuracy.
# # 3) pop variables in coordsData and IMS Health data
# checkpop <- inner_join(data5, coordsData, by = "zipname") %>% select(season, zipname, pop.x, pop.y) %>% distinct %>% mutate(pop.diff = pop.x - pop.y) %>% group_by(season) %>% summarise(sum.pop.diff = sum(abs(pop.diff), na.rm=T))
# View(checkpop)
# # pop differences are smallest in the earliest season (i.e. 2000-01)

#### merge coords with dz burden metrics ##################
# merge coord data with dz burden metrics data
coordsData2 <- coordsData %>% select(-w_lat, -w_long, -pop, -zip3)
dbMetrics.coords <- left_join(dbMetrics.g, coordsData2, by = "zipname")

#### save summary data ##################
# # save summary data to file (9/15/15) 
# # these data are used in "explore_dbMetricsDistribution_IR.R" for exploratory analysis of outcome metrics
# setwd('../R_export')
# write.csv(dbMetrics.g, file = sprintf('dbMetrics_periodicReg_%sILI%s_analyzeDB.csv', code, code2), row.names=FALSE)
# 
# # save summary data to file with coords (9/15/15) 
# # these data are used in "analyze_dbMetricsDistance_IR.R" for matrix correlations
# setwd('../R_export')
# write.csv(dbMetrics.coords, file = sprintf('dbMetrics_periodicReg_%sILI%s_analyzeDBdist.csv', code, code2), row.names=FALSE)

#### checks on metrics ##################
# # perform checks on db_metric calculations
# View(data5 %>% filter(season==1 & zipname=='010') %>% select(ili, in.season))
# dbMetrics.g %>% filter(season==1 & zipname=='010')
# # everything seems to work
#### checks on consider.flu.season ##################
# # perform checks on consider.flu.season function
# # there are weeks that do not have epidemics that are considered "in flu season" -> this can occur if there are sets of 4 consecutive epi weeks separated by non-epi weeks.
# View(data5 %>% filter(!epi.week & in.season))
# View(data5 %>% filter(season==8 & zipname=='537'))
# View(data5 %>% filter(season==7 & zipname=='102'))
# # everything seems to work
#### test data for consider.flu.season prototype ##################
# # test data and code for consider.flu.season function prototyping
# test <- data.frame(zip3=c(1,1,1,2,2,2,3,3,3,4,4,4), epi.week=c(T,T,F,F,F,T,T,T,T,F,F,F))
# test %>% group_by(zip3) %>% select(epi.week) %>% consider.flu.season
# test %>% group_by(zip3) %>% mutate(in.seas = consider.flu.season(epi.week))
