
## Name: Elizabeth Lee
## Date: 9/15/15
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


#### header ####################################
setwd('~/Dropbox (Bansal Lab)/code')
source("GeneralTools.R")
require(dplyr)
require(ggplot2)
require(readr)
require(ISOweek)
setwd(dirname(sys.frame(1)$ofile))

#### set these! ####################################
# code = "t2sa_" # semi-annual periodicity
code = "t2_" # parabolic time trend term
# code="" # linear time trend term
code2 = "_Oct"

#### data import ####################################
setwd('../R_export')
data <- read_csv(file=sprintf('periodicReg_%sallZip3Mods_ILI%s.csv', code, code2), col_types=list(ili=col_double(), .fitted=col_double(), .se.fit=col_double()))

# 1) add ISO week numbers; 2) add season numbers ; 3) add real zip3 names
data2 <- data %>% mutate(wknum = as.numeric(substr.Right(ISOweek(Thu.week), 2))) %>% mutate(season = ifelse(wknum<40, as.integer(substr(Thu.week, 3, 4)), as.integer(substr(Thu.week, 3, 4))+1)) %>% mutate(zipname = substr.Right(gsub("X", "00", zip3), 3))

# 1) include only zip3s where lm was performed; 2) set .fitted + 1.96*.se.fit as the epidemic threshold
data3 <- data2 %>% filter(incl.lm)  %>% mutate(epi.thresh = .fitted+(1.96*.se.fit)) 


## Try different analyses to determine flu season bounds
#### Analysis 1  ###########################
# Analysis 1: a zip3 had a flu epidemic if ILI > epi.threshold for at least x weeks during the flu period
# identify zip3s with ILI > epi.threshold for at least 4 weeks during a flu.week == TRUE (not necessarily consec)
zips.over.thresh_ct <- data3 %>% filter(flu.week) %>% group_by(season, zipname) %>% summarise(., num.epi.weeks = sum(ili>epi.thresh, na.rm=TRUE)) %>% mutate(incl.analysis = num.epi.weeks >= 4)
# compare expectation to zip3 = 013
test <- data3 %>% filter(zipname=='013', season==1, flu.week) %>% select(ili, epi.thresh) %>% mutate(ili>epi.thresh)
# 9/15/15 not using this metric for identifying flu seasons

#### Analysis 2 ###########################
# Analysis 2: a zip3 had a flu epidemic if ILI > epi.threshold for at least x consecutive weeks during the flu period
# maximum number of consecutive epidemics during flu weeks
zips.over.thresh_flu_consec <- data3 %>% filter(flu.week) %>% mutate(epidemic = ili>epi.thresh) %>% group_by(season, zipname) %>% summarise(flu = rle.func(epidemic))
# maximum number of consecutive epidemics during non-flu weeks
zips.over.thresh_nonflu_consec <- data3 %>% filter(!flu.week) %>% mutate(epidemic = ili>epi.thresh) %>% group_by(season, zipname) %>% summarise(nf = rle.func(epidemic))
zips.over.thresh_consec_flat <- right_join(zips.over.thresh_flu_consec, zips.over.thresh_nonflu_consec, by = c("season", "zipname"))
zips.over.thresh_consec <- gather(zips.over.thresh_consec_flat, period, consec.weeks, 3:4)

# plot histograms for consecutive weeks
dir.create(sprintf('../graph_outputs/explore_fluSeasonDefinition_%sILI%s', code, code2), showWarnings=FALSE)
setwd(sprintf('../graph_outputs/explore_fluSeasonDefinition_%sILI%s', code, code2))

# single histogram for all seasons
consec.flu.nf.single <- ggplot(zips.over.thresh_consec, aes(x=consec.weeks, group=period)) +
  geom_histogram(aes(y=..density.., color=period), binwidth=1, alpha=0.5, position="dodge") +
  coord_cartesian(ylim=c(0, 0.25))
ggsave(sprintf("consecEpiWeeks_%sILI%s.png", code, code2), consec.flu.nf.single, width=4, height=4)

# compare consecutive epi weeks across flu and non-flu periods by season
# flu period plot
consec.flu <- ggplot(zips.over.thresh_consec %>% filter(period=="flu"), aes(x=consec.weeks, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=1) + 
  coord_cartesian(ylim=c(0, 0.3)) +
  facet_wrap(~season) + ggtitle("flu period")
ggsave(sprintf("consecEpiWeeks_fluPeriod_%sILI%s.png", code, code2), consec.flu, width=9, height=6)
# nonflu period plot
consec.nflu <- ggplot(zips.over.thresh_consec %>% filter(period=="nf"), aes(x=consec.weeks, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=1) + 
  coord_cartesian(ylim=c(0, 0.3)) +
  facet_wrap(~season) + ggtitle("non-flu period")
ggsave(sprintf("consecEpiWeeks_nfPeriod_%sILI%s.png", code, code2), consec.nflu, width=9, height=6)

# summarise means and variance of consecutive flu weeks for each season
consec.summary <- zips.over.thresh_consec_flat %>% group_by(season) %>% summarise(., flu.mn = mean(flu), nflu.mn = mean(nf), flu.var = var(flu), nflu.var = var(nf))

# quantile for flu period
zips.over.thresh_consec %>% filter(period=="flu") %>% select(consec.weeks) %>% unlist %>% quantile
# quantile for non flue period
zips.over.thresh_consec %>% filter(period=="nf") %>% select(consec.weeks) %>% unlist %>% quantile
# during flu period: 75% of zip3-season combinations have 2+ consecutive weeks above the epidemic threshold
# during non-flu period: 75% of zip3-season combinations have 4 or fewer consecutive weeks above the epidemic threshold

#### Analysis 3: 9/15/15 ###########################
# a) remove the zip-season combinations where # consecutive weeks that ILI > epi.threshold is greater during the non-flu period than the flu period. We reason that zip3s with more or equivalent ILI activity during the non-flu season than during the flu season have too much noise to detect the true flu activity
# b) (like analysis 2) a zip3 had a flu epidemic if ILI > epi.threshold for at least x consecutive weeks during the flu period
# maximum number of consecutive epidemics during flu weeks
zips.over.thresh_flu_consec <- data3 %>% filter(flu.week) %>% mutate(epidemic = ili>epi.thresh) %>% group_by(season, zipname) %>% summarise(flu = rle.func(epidemic))
# maximum number of consecutive epidemics during non-flu weeks
zips.over.thresh_nonflu_consec <- data3 %>% filter(!flu.week) %>% mutate(epidemic = ili>epi.thresh) %>% group_by(season, zipname) %>% summarise(nf = rle.func(epidemic))
zips.over.thresh_consec_flat2 <- right_join(zips.over.thresh_flu_consec, zips.over.thresh_nonflu_consec, by = c("season", "zipname")) %>% mutate(incl.analysis = flu > nf) # 4596/6464 zip3-seasons were T for incl.analysis
zips.over.thresh_consec2 <- zips.over.thresh_consec_flat2 %>% filter(incl.analysis) %>% gather(period, consec.weeks, 3:4) %>% select(-incl.analysis)

# single histogram for all seasons, zip3 noise removed
consec.flu.nf.single2 <- ggplot(zips.over.thresh_consec2, aes(x=consec.weeks, group=period)) +
  geom_histogram(aes(y=..density.., color=period), binwidth=1, alpha=0.5, position="dodge") +
  coord_cartesian(ylim=c(0, 0.3))
ggsave(sprintf("consecEpiWeeks_%sILI%s_rmNoise.png", code, code2), consec.flu.nf.single2, width=4, height=4)

# quantile for flu period
zips.over.thresh_consec2 %>% filter(period=="flu") %>% select(consec.weeks) %>% unlist %>% quantile(seq(0, 1, 0.05))
# quantile for non flu period
zips.over.thresh_consec2 %>% filter(period=="nf") %>% select(consec.weeks) %>% unlist %>% quantile(seq(0, 1, 0.05))
# during flu period: 85% of zip3-season combinations have 4+ consecutive weeks above the epidemic threshold
# during non-flu period: 80% of zip3-season combinations have 4 or fewer consecutive weeks above the epidemic threshold
# 9/15/15: use four consecutive weeks as the lower threshold
