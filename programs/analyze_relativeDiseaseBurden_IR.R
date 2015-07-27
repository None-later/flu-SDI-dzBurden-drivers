
## Name: Elizabeth Lee
## Date: 5/30/15
## Function: analyze relative magnitude of disease burden
### disease burden metrics: total ILI attack rate, rate of ILI at epidemic peak, epidemic duration
## Filenames: periodicReg_%sallZip3Mods.csv
## Data Source: IMS Health 
## Notes: 5/31/15 - Refer to explore_fluSeasonDefinition_IR.R for explanation of "flu epidemic" is defined. Zip3s are considered to have experienced a flu epidemic if they had 4+ consecutive weeks above the epidemic threshold in the flu period.
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
####################################
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')
data <- read_csv(file=sprintf('periodicReg_%sallZip3Mods.csv', code), col_types=list(ili=col_double(), .fitted=col_double(), .se.fit=col_double(), IR=col_double(), pop=col_integer()))

# 1) add ISO week numbers; 2) add season numbers ; 3) add real zip3 names
data2 <- data %>% mutate(wknum = as.numeric(substr.Right(ISOweek(Thu.week), 2))) %>% mutate(season = ifelse(wknum<40, as.integer(substr(Thu.week, 3, 4)), as.integer(substr(Thu.week, 3, 4))+1)) %>% mutate(zipname = substr.Right(gsub("X", "00", zip3), 3))

# 1) include only zip3s where lm was performed; 2) set .fitted + 1.96*.se.fit as the epidemic threshold; 3) identify which weeks are epi weeks
data3 <- data2 %>% filter(incl.lm)  %>% mutate(epi.thresh = .fitted+(1.96*.se.fit)) %>% mutate(epi.week = IR>epi.thresh)

## See explore_fluSeasonDefinition_IR.R for derivation of flu season definition
# summarize season-zip3 combinations that have epidemics (4+ consecutive epidemic weeks)
zip3s_with_epi <- data3 %>% filter(flu.week)  %>% group_by(season, zipname) %>% summarise(consec.epiweeks = rle.func(epi.week)) %>% mutate(has.epi = (consec.epiweeks>=4))

# join summary data to full dataset (adds has.epi indicator)
data4 <- right_join(data3, zip3s_with_epi %>% select(-consec.epiweeks), by=c("season", "zipname"))

data5 <- data4 %>% filter(flu.week) %>% filter(has.epi) %>% group_by(season, zipname) %>% mutate(in.season = consider.flu.season(epi.week))

# # save to file (6/1/15)
# setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')
# write.csv(data5, file = sprintf('fullIndic_periodicReg_%sanalyzeDB.csv', code), row.names=FALSE)

##################
# create disease burden metrics: total ILI attack rate (proxy: mean IR), rate of ILI at epidemic peak, epidemic duration
dbMetrics <- data5 %>% group_by(season, zipname) %>% filter(in.season) %>% summarize(tot.ar = mean(IR, na.rm=T), peak.rate = max(IR, na.rm=T), epi.dur = length(in.season))
dbMetrics.g <- gather(dbMetrics, metric, burden, 3:5)
# mean and sd for each metric by season
dbMetrics_summary <- dbMetrics %>% group_by(season) %>% summarize(tot.ar.mn = mean(tot.ar), tot.ar.sd = sd(tot.ar), peak.rate.mn = mean(peak.rate), peak.rate.sd = sd(peak.rate), epi.dur.mn = mean(epi.dur), epi.dur.sd = sd(epi.dur))

# # save to file (6/1/15)
# setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')
# write.csv(dbMetrics.g, file = sprintf('dbMetrics_periodicReg_%sanalyzeDB.csv', code), row.names=FALSE)

##################
# zscore all dbMetrics by the mean and sd for that season, in order to make data comparable across seasons
# Although the distributions are not normally distributed, the distributions across seasons are fairly similar for each disease burden metric, which might suggest that the standardized values should comparable across seasons. Standardization is still beneficial because it will render the values more comparable, but it will not help with comparisons between disease burden metrics.

dbMetrics.gz <- dbMetrics.g %>% group_by(season, metric) %>% mutate(burden.z = (burden - mean(burden))/sd(burden))


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
