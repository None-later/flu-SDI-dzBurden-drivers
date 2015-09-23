
## Name: Elizabeth Lee
## Date: 9/16/15
## Function: write zip3-season combinations to include in BHM analysis. These combos should pass the following filters: 1) rm zip3-season combos with peak ILI counts < 25; 2) rm zip3-season combos with >1 week of missing data during the flu season
### disease burden metrics: ili count at epidemic peak

## Filenames: sprintf('fullIndicFlu_periodicReg_%sILI%s_analyzeDB.csv', code, code2); sprintf('dbMetrics_periodicReg_%sILI%s_analyzeDB.csv', code, code2)
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
#### set these! ####################################
# code = "t2sa_" # semi-annual periodicity
code = "t2_" # parabolic time trend term
# code="" # linear time trend term
code2 = "_Oct"
#### read data ##################
setwd('../R_export')
data5 <- read_csv(sprintf('fullIndicFlu_periodicReg_%sILI%s_analyzeDB.csv', code, code2), col_names = T, col_types = list(zipname=col_character()))
dbMetrics.g <- read.csv(sprintf('dbMetrics_periodicReg_%sILI%s_analyzeDB.csv', code, code2), header=T, colClasses=c(zipname="character", metric="character"))

#### 1) rm zip3-season combos with peak ILI counts < 25 ####################################
zips.season <- dbMetrics.g %>% filter(metric == "ili.peak") %>% count(season) # total number of zip3s in each season
zips.filt.season <- dbMetrics.g %>% filter(metric == "ili.peak" & burden >=25) %>% count(season) # total number of zips with >= 25 ili peak count
zip.counts <- full_join(zips.season, zips.filt.season, by="season") %>% mutate(perc.notfiltered = n.y/n.x*100) %>% mutate(num.filtered = n.x-n.y) # retain ~80-90% of data when zip3s are filtered out if they have fewer than 25 cases at the peak

# combinations from filter 1
zipseas.combos1 <- dbMetrics.g %>% filter(metric == "ili.peak" & burden >=25) %>% select(season, zipname)

#### 2) rm zip3-season combos with >1 week of missing data during the flu season ####################################
# N.B. data5 is already filtered by incl.analysis and has.epi
summ.NA.data <- data5 %>% group_by(season, zipname) %>% summarise(num.NAs = sum(is.na(ili))) %>% filter(season!=1)
# explore distribution of # NAs by season
NAplot.no0 <- ggplot(summ.NA.data %>% filter(num.NAs!=0), aes(x=num.NAs)) +
  geom_histogram(aes(y=..density..), binwidth=1) +
  facet_wrap(~season)
print(NAplot.no0)
# summary: number of zip3s with greater than one NA during flu period, by season
summ.flu.per <- summ.NA.data %>% filter(num.NAs > 1) %>% ungroup %>% count(season) # rm 30-60 zip3s per season due to NAs

# combinations from filter 2
zipseas.combos2 <- data5 %>% group_by(season, zipname) %>% summarise(num.NAs = sum(is.na(ili))) %>% filter(num.NAs <= 1) %>% select(-num.NAs)

#### intersection of zip3-season combos ####################################
zipseas.combos.filt <- intersect(zipseas.combos1, zipseas.combos2)
summ.filtered <- zipseas.combos.filt %>% count(season) # roughly 300 to 600 zip3s by season
#### visual checks ####################################
removedCombos <- setdiff(dbMetrics.g %>% filter(metric=="ili.peak") %>% select(season, zipname), zipseas.combos.filt) %>% filter(season!=1)
removedCombos %>% count(season) # with the two filters, 50 to 90 combinations are removed per season

#### 9/23/15: how many zip3s have data across all 8 seasons? ####################################
zipseas.combos.filt %>% filter(season != 1) %>% count(zipname) %>% filter(n == 8) # only 97 of 760 zip3s meet these requirements for seasons 2-9
distr <- zipseas.combos.filt %>% filter(season != 1) %>% count(zipname)
hist(distr$n, breaks=8, xlab='seasons that meet peak ILI & missing data criteria', main='distribution of zip3s')
# saved 9/23/15 in explore_periodicReg_%s_fits_ILI%s/inSeason

#### write to file ####################################
# write.csv(zipseas.combos.filt, file=sprintf('zip3SeasonCombos_%sILI%s.csv', code, code2), row.names=F)
# saved 9/16/15

