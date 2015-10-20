
## Name: Elizabeth Lee
## Date: 9/15/15
## Function: write relative magnitude of disease burden with ILI-Oct data; data processing: Zip3-season combinations with equal or more consecutive weeks above the epidemic threshold in the non-flu period than the flu period are filtered out (see explore_fluSeasonDefinition_ILI.R, Analysis 3 for additional details)
### disease burden metrics: total ili summed across epidemic weeks, cumulative difference in ili and baseline, cumulative difference in ili and epidemic threshold, rate of ili at epidemic peak, epidemic duration, weeks from Nov1 to epidemic start, weeks from epidemic start to peak ili week 

## Filenames: periodicReg_%sallZip3Mods_ILI_Oct.csv
## Data Source: IMS Health 
## Notes: 9/15/15 - Refer to explore_fluSeasonDefinition_ILI.R for definition of "flu epidemic". Zip3s are considered to have experienced a flu epidemic if they had 4+ consecutive weeks above the epidemic threshold in the flu period. 
## 10/20/15 - Split program with write_fullIndic_periodicReg_ILI.R (run that first)
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

rm(list = ls())
#### header ####################################
require(dplyr)
require(ggplot2)
require(readr)
require(ISOweek)
require(tidyr)
setwd(dirname(sys.frame(1)$ofile))

#### set these! ####################################
# code <- "t2sa_" # semi-annual periodicity
# code <- "t2_" # parabolic time trend term
code <- "t4_" # quartic time trend
# code <-"" # linear time trend term

# code2 <- "_Oct" # fluseason = Oct to Apr
code2 <- "_Octfit" # fit = Apr to Oct and fluseason = Oct to Apr

#### read saved data from write_fullIndic_periodicReg_ILI.R ##################
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
createTiming2 <- data7 %>% mutate(ili = as.numeric(ili)) %>% group_by(season, zipname) %>% filter(in.season) %>% mutate(t.firstepiweek = ifelse(Thu.week==min(Thu.week, na.rm=T), t, 0)) %>% mutate(t.peakweek = as.numeric(ifelse(ili==max(ili, na.rm=T), t, 0))) %>% ungroup %>% select(Thu.week, zipname, t.firstepiweek, t.peakweek) # as.numeric wrapper on if/else statement for t.peakweek mutate is due to issue: https://github.com/hadley/dplyr/issues/1036
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
# save summary data to file (10/20/15) 
# these data are used in "explore_dbMetricsDistribution_IR.R" for exploratory analysis of outcome metrics
setwd('../R_export')
write.csv(dbMetrics.g, file = sprintf('dbMetrics_periodicReg_%sILI%s_analyzeDB.csv', code, code2), row.names=FALSE)

# save summary data to file with coords (10/20/15) 
# these data are used in "analyze_dbMetricsDistance_IR.R" for matrix correlations
setwd('../R_export')
write.csv(dbMetrics.coords, file = sprintf('dbMetrics_periodicReg_%sILI%s_analyzeDBdist.csv', code, code2), row.names=FALSE)


