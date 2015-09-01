
## Name: Elizabeth Lee
## Date: 5/30/15
## Function: write relative magnitude of disease burden
### disease burden metrics: mean IR across epidemic weeks, cumulative difference in IR and baseline, cumulative difference in IR and epidemic threshold, rate of ILI at epidemic peak, epidemic duration, weeks from Nov1 to epidemic start, weeks from epidemic start to peak IR week 
## Filenames: periodicReg_%sallZip3Mods.csv
## Data Source: IMS Health 
## Notes: 5/31/15 - Refer to explore_fluSeasonDefinition_IR.R for explanation of "flu epidemic" is defined. Zip3s are considered to have experienced a flu epidemic if they had 4+ consecutive weeks above the epidemic threshold in the flu period.
## 7/28/15: renamed "analyze_relativeDiseaseBurden_IR.R" to "write_relativeDiseaseBurden_IR.R", add lat/long coords by zip
## 8/6/15: add peak timing (# weeks since start of epi period) and epidemic start timing (true week number) as disease burden metrics
## 8/31/15: edit epidemic duration metric
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

####################################
# header
setwd('/home/elee/R/source_functions')
source("dfsumm.R")
require(dplyr)
require(ggplot2)
require(readr)
require(ISOweek)
require(tidyr)

####################################
# local functions
# substring from right
substr.Right <- function(x, numchar){
  return(substr(x, nchar(x)-(numchar-1), nchar(x)))
}
# return maximum number of consecutive T values in x
rle.func <- function(x){
  rle.results = rle(x)
  return(max(0, rle.results$lengths[which(rle.results$values)]))
}
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


####################################
# set these!
# code = "t2sa_" # semi-annual periodicity
code = "t2_" # parabolic time trend term
# code="" # linear time trend term

# ####################################
# # 8/31/15 comment out saved data processing 
# setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')
# data <- read_csv(file=sprintf('periodicReg_%sallZip3Mods.csv', code), col_types=list(ili=col_double(), .fitted=col_double(), .se.fit=col_double(), IR=col_double(), pop=col_integer()))
# 
# # 1) add ISO week numbers; 2) add season numbers ; 3) add real zip3 names
# data2 <- data %>% mutate(wknum = as.numeric(substr.Right(ISOweek(Thu.week), 2))) %>% mutate(season = ifelse(wknum<40, as.integer(substr(Thu.week, 3, 4)), as.integer(substr(Thu.week, 3, 4))+1)) %>% mutate(zipname = substr.Right(gsub("X", "00", zip3), 3))
# 
# # 1) include only zip3s where lm was performed; 2) set .fitted + 1.96*.se.fit as the epidemic threshold; 3) identify which weeks are epi weeks
# data3 <- data2 %>% filter(incl.lm)  %>% mutate(epi.thresh = .fitted+(1.96*.se.fit)) %>% mutate(epi.week = IR>epi.thresh)
# 
# ## See explore_fluSeasonDefinition_IR.R for derivation of flu season definition
# # summarize season-zip3 combinations that have epidemics (4+ consecutive epidemic weeks)
# zip3s_with_epi <- data3 %>% filter(flu.week)  %>% group_by(season, zipname) %>% summarise(consec.epiweeks = rle.func(epi.week)) %>% mutate(has.epi = (consec.epiweeks>=4))
# 
# # join summary data to full dataset (adds has.epi indicator)
# data4 <- right_join(data3, zip3s_with_epi %>% select(-consec.epiweeks), by=c("season", "zipname"))
# 
# data5 <- data4 %>% filter(flu.week) %>% filter(has.epi) %>% group_by(season, zipname) %>% mutate(in.season = consider.flu.season(epi.week))

# # save to file (6/1/15)
# # these data are saved for reference
# setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')
# write.csv(data5, file = sprintf('fullIndicFlu_periodicReg_%sanalyzeDB.csv', code), row.names=FALSE) # 8/18/15 name change


# ## 8/31/15: found an error in the in.season variable for the fullIndicAll data
# data6 <- left_join(data4, (data5 %>% ungroup %>% select(Thu.week, zipname, in.season)), by = c("Thu.week", "zipname"))
# ## save to file (8/31/15)
# ## these data are saved for reference
# setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')
# write.csv(data6, file = sprintf('fullIndicAll_periodicReg_%sanalyzeDB.csv', code), row.names=FALSE)

##################
# 8/31/15 read dz burden data directly
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')
data5 <- read_csv(sprintf('fullIndicFlu_periodicReg_%sanalyzeDB.csv', code), col_names = T, col_types = list(zipname=col_character()))

##################
# 7/27/15 update dz burden metrics (\cite{Viboud2014} for inspiration of 1b & 1c)
# create disease burden metrics: 1a) mean IR across epidemic weeks (proxy of overall magnitude), 1b) cumulative difference in IR and baseline (second proxy of overall magnitude), 1c) cumulative difference in IR and epidemic threshold (third proxy of overall magnitude), 2) rate of ILI at epidemic peak, 3) epidemic duration
dbMetrics <- data5 %>% group_by(season, zipname) %>% filter(in.season) %>% summarize(IR.mean = mean(IR, na.rm=T), IR.excess.BL = sum(IR-.fitted, na.rm=T), IR.excess.thresh = sum(IR-epi.thresh, na.rm=T), IR.peak = max(IR, na.rm=T), epi.dur = sum(in.season))

# 8/6/15 create epidemic start timing and peak timing metrics
# data processing in preparation for timing metrics
data6 <- data5 %>% select(Thu.week, t, IR, season, zipname, flu.week, in.season)
createTiming1 <- data6 %>% group_by(season, zipname) %>% mutate(t.minweek = ifelse(Thu.week==min(Thu.week), t, 0)) %>% select(Thu.week, season, zipname, t.minweek) %>% ungroup
createTiming2 <- data6 %>% group_by(season, zipname) %>% filter(in.season) %>% mutate(t.firstepiweek = ifelse(Thu.week==min(Thu.week, na.rm=T), t, 0)) %>% mutate(t.peakweek = ifelse(IR==max(IR, na.rm=T), t, 0)) %>% ungroup %>% select(Thu.week, zipname, t.firstepiweek, t.peakweek)
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
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/reference_data')
import <- read_csv(file='Coord3digits.csv')
coordsData <- tbl_df(import) %>% select(zip3, STATE, st_FIPS, pop, lat, long, w_lat, w_long) %>% mutate(zipname = substr.Right(paste0("00", zip3), 3))

##################
# # performed checks on coordsData (7/28/15)
# # 1) state-zip3 assignment: cross-referenced a sample of zip3-state combinations using zip code prefixes page on wikipedia
# View(coordsData %>% select(zip3, STATE))
# # 2) zip3-latlong assignment: cross-referenced a sample of zip3-latlong combinations using zip3 centroids google fusion table (Id: http://maps.huge.info/zip3.htm)
# # coordinates are similar, but I don't understand the differences between the lat/long and w_lat/w_long variables (perhaps regular lat/long centroids or population-weighted lat/long centroids). Nevertheless, the coordinate decimal places are not exact, so I'm not sure about the accuracy.
# # 3) pop variables in coordsData and IMS Health data
# checkpop <- inner_join(data5, coordsData, by = "zipname") %>% select(season, zipname, pop.x, pop.y) %>% distinct %>% mutate(pop.diff = pop.x - pop.y) %>% group_by(season) %>% summarise(sum.pop.diff = sum(abs(pop.diff), na.rm=T))
# View(checkpop)
# # pop differences are smallest in the earliest season (i.e. 2000-01)

##################
# merge coord data with dz burden metrics data
coordsData2 <- coordsData %>% select(-w_lat, -w_long, -pop, -zip3)
dbMetrics.coords <- left_join(dbMetrics.g, coordsData2, by = "zipname")

##################
# # save summary data to file (8/31/15) 
# # these data are used in "explore_dbMetricsDistribution_IR.R" for exploratory analysis of outcome metrics
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')
write.csv(dbMetrics.g, file = sprintf('dbMetrics_periodicReg_%sanalyzeDB.csv', code), row.names=FALSE)

# # save summary data to file with coords (8/31/15) 
# # these data are used in "analyze_dbMetricsDistance_IR.R" for matrix correlations
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')
write.csv(dbMetrics.coords, file = sprintf('dbMetrics_periodicReg_%sanalyzeDBdist.csv', code), row.names=FALSE)

##################
# perform checks on db_metric calculations
# View(data5 %>% filter(season==1 & zipname=='010') %>% select(IR, in.season))
# everything seems to work
##################
# perform checks on consider.flu.season function
# there are weeks that do not have epidemics that are considered "in flu season" -> this can occur if there are sets of 4 consecutive epi weeks separated by non-epi weeks.
# View(data5 %>% filter(!epi.week & in.season))
# View(data5 %>% filter(season==9 & zipname=='631'))
# View(data5 %>% filter(season==9 & zipname=='464'))
# everything seems to work
# ##################
# # test data and code for consider.flu.season function prototyping
# test <- data.frame(zip3=c(1,1,1,2,2,2,3,3,3,4,4,4), epi.week=c(T,T,F,F,F,T,T,T,T,F,F,F))
# test %>% group_by(zip3) %>% select(epi.week) %>% consider.flu.season
# test %>% group_by(zip3) %>% mutate(in.seas = consider.flu.season(epi.week))
