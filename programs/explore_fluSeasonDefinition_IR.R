
## Name: Elizabeth Lee
## Date: 5/31/15
## Function: explore ways to define the flu season
# a) number of weeks during the flu period that exceed the epidemic threshold
# b) number of consecutive weeks during the flu period that exceed the epidemic threshold (as compared to consecutive weeks during the non-flu period)
# Result of analysis (t2): If a zip3-season combination has 4+ consecutive weeks above the epidemic threshold during the flu season, it has an epidemic

## Filenames: 
## Data Source: 
## Notes: 
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

####################################
# local functions
substr.Right <- function(x, numchar){
  return(substr(x, nchar(x)-(numchar-1), nchar(x)))
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

# 1) include only zip3s where lm was performed; 2) set .fitted + 1.96*.se.fit as the epidemic threshold
data3 <- data2 %>% filter(incl.lm)  %>% mutate(epi.thresh = .fitted+(1.96*.se.fit)) 

###########################
## Try different analyses to determine flu season bounds

# Analysis 1: a zip3 had a flu epidemic if IR > epi.threshold for at least x weeks during the flu period
# identify zip3s with IR > epi.threshold for at least 4 weeks during a flu.week == TRUE
zips.over.thresh_ct <- data3 %>% filter(flu.week) %>% group_by(season, zipname) %>% summarise(., num.epi.weeks = sum(IR>epi.thresh, na.rm=TRUE)) %>% mutate(incl.analysis = num.epi.weeks >= 4)

test <- data3 %>% filter(zipname=='013', season==1, flu.week) %>% select(IR, epi.thresh) %>% mutate(IR>epi.thresh)

# Analysis 2: a zip3 had a flu epidemic if IR > epi.threshold for at least x consecutive weeks during the flu period

# function that returns maximum number of consecutive T values in x
rle.func <- function(x){
  rle.results = rle(x)
  return(max(0, rle.results$lengths[which(rle.results$values)]))
}
# maximum number of consecutive epidemics during flu weeks
zips.over.thresh_flu_consec <- data3 %>% filter(flu.week) %>% mutate(epidemic = IR>epi.thresh) %>% group_by(season, zipname) %>% summarise(flu = rle.func(epidemic))
# maximum number of consecutive epidemics during non-flu weeks
zips.over.thresh_nonflu_consec <- data3 %>% filter(!flu.week) %>% mutate(epidemic = IR>epi.thresh) %>% group_by(season, zipname) %>% summarise(nf = rle.func(epidemic))
zips.over.thresh_consec_flat <- right_join(zips.over.thresh_flu_consec, zips.over.thresh_nonflu_consec, by = c("season", "zipname"))
zips.over.thresh_consec <- gather(zips.over.thresh_consec_flat, period, consec.weeks, 3:4)

# plot histograms for consecutive weeks
setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_fluSeasonDefinition_%sIR', code))

# single histogram for all seasons
consec.flu.nf.single <- ggplot(zips.over.thresh_consec, aes(x=consec.weeks, group=period)) +
  geom_histogram(aes(y=..density.., color=period), binwidth=1, alpha=0.5, position="dodge") +
  coord_cartesian(ylim=c(0, 0.25))
ggsave(sprintf("consecEpiWeeks_%sIR.png", code), consec.flu.nf.single, width=4, height=4)

# compare consecutive epi weeks across flu and non-flu periods by season
# flu period plot
consec.flu <- ggplot(zips.over.thresh_consec %>% filter(period=="flu"), aes(x=consec.weeks, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=1) + 
  coord_cartesian(ylim=c(0, 0.3)) +
  facet_wrap(~season) + ggtitle("flu period")
ggsave(sprintf("consecEpiWeeks_fluPeriod_%sIR.png", code), consec.flu, width=9, height=6)
# nonflu period plot
consec.nflu <- ggplot(zips.over.thresh_consec %>% filter(period=="nf"), aes(x=consec.weeks, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=1) + 
  coord_cartesian(ylim=c(0, 0.3)) +
  facet_wrap(~season) + ggtitle("non-flu period")
ggsave(sprintf("consecEpiWeeks_nfPeriod_%sIR.png", code), consec.nflu, width=9, height=6)

# summarise means and variance of consecutive flu weeks for each season
consec.summary <- zips.over.thresh_consec_flat %>% group_by(season) %>% summarise(., flu.mn = mean(flu), nflu.mn = mean(nf), flu.var = var(flu), nflu.var = var(nf))

# quantile for flu period
zips.over.thresh_consec %>% filter(period=="flu") %>% select(consec.weeks) %>% unlist %>% quantile
# quantile for non flue period
zips.over.thresh_consec %>% filter(period=="nf") %>% select(consec.weeks) %>% unlist %>% quantile
# during flu period: 75% of zip3-season combinations have 5+ consecutive weeks above the epidemic threshold
# during non-flu period: 75% of zip3-season combinations have 4 or fewer consecutive weeks above the epidemic threshold

