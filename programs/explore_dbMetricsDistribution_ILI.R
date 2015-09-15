## Name: Elizabeth Lee
## Date: 9/15/15
## Function: explore distributions of disease burden metrics -- are they normally distributed?
## Results: magnitude metrics could be truncated and shifted normals, but timing metrics don't appear to be normally distributed
### disease burden metrics: sum ILI across epidemic weeks, cumulative difference in ILI and baseline, cumulative difference in ILI and epidemic threshold, rate of ILI at epidemic peak, epidemic duration, time to epidemic from start of flu period, time to epidemic peak from start of epidemic
## Filenames: sprintf('dbMetrics_periodicReg_%sILI%s_analyzeDB.csv', code, code2)
## Data Source: IMS Health 
## Notes: 
## 9/15/15 - Refer to explore_fluSeasonDefinition_ILI.R for explanation of "flu epidemic" is defined. Zip3s are considered to have experienced a flu epidemic if they had 4+ consecutive weeks above the epidemic threshold in the flu period.
## 9/15/15 - Refer to write_relativeDiseaseBurden_ILI.R for export of disease burden metrics data file.
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header ####################################
setwd('~/Dropbox/code')
source("GeneralTools.R")
require(ggplot2)
require(readr)
require(dplyr)
require(tidyr)
setwd(dirname(sys.frame(1)$ofile))
#### set these! ####################################
# code = "t2sa_" # semi-annual periodicity
code <- "t2_" # parabolic time trend term
# code="" # linear time trend term
code2 <- "_Oct"
#### import data ####################################
setwd('../R_export')
dbMetrics.g <- read.csv(sprintf('dbMetrics_periodicReg_%sILI%s_analyzeDB.csv', code, code2), header=T, colClasses=c(zipname="character", metric="character")) %>% filter(season!=1)
# standardized data
dbMetrics.gz <- dbMetrics.g %>% group_by(season, metric) %>% mutate(burden.z = (burden - mean(burden))/sd(burden))

#### plot formatting ####################################
w = 9
h = 6

#### plot distribution of dbMetrics ####################################
# 9/15/15 - saved figures
dir.create(sprintf('../graph_outputs/explore_dbMetricsDistribution_%sILI%s', code, code2), showWarnings=FALSE)
setwd(sprintf('../graph_outputs/explore_dbMetricsDistribution_%sILI%s', code, code2))

# total ILI plot
plt.distr.iliSum <- ggplot(dbMetrics.g %>% filter(metric=='ili.sum'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=25) + geom_density() + 
  coord_cartesian(xlim=c(0, 5000)) +
  facet_wrap(~season) + ggtitle("Sum ILI during flu season")
ggsave(sprintf("distr_ILITot_%sILI%s.png", code, code2), plt.distr.iliSum, width=w, height=h)

# total ILI plot zoomed!
plt.distr.iliSum.zm <- ggplot(dbMetrics.g %>% filter(metric=='ili.sum'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=25) + geom_density() + 
  coord_cartesian(xlim=c(0, 1000)) +
  facet_wrap(~season) + ggtitle("Sum ILI during flu season (zoom)")
ggsave(sprintf("distr_ILITot_%sILI%s_zm.png", code, code2), plt.distr.iliSum.zm, width=w, height=h)

# ILI in excess of modeled seasonal baseline
plt.distr.ILIexcessBL <- ggplot(dbMetrics.g %>% filter(metric=='ili.excess.BL'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=25) + geom_density() + 
  coord_cartesian(xlim=c(0, 3000)) +
  facet_wrap(~season) + ggtitle("ILI in excess of modeled seasonal baseline during flu season")
ggsave(sprintf("distr_ILIexcessBL_%sILI%s.png", code, code2), plt.distr.ILIexcessBL, width=w, height=h)

# ili in excess of modeled epidemic threshold (BL + 1.96*se)
plt.distr.ILIexcessThresh <- ggplot(dbMetrics.g %>% filter(metric=='ili.excess.thresh'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=25) + geom_density() + 
  coord_cartesian(xlim=c(0, 3000)) +
  facet_wrap(~season) + ggtitle("ILI in excess of modeled epidemic threshold during flu season")
ggsave(sprintf("distr_ILIexcessThresh_%sILI%s.png", code, code2), plt.distr.ILIexcessThresh, width=w, height=h)

# ili peak case count plot
plt.distr.pkCount <- ggplot(dbMetrics.g %>% filter(metric=='ili.peak'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=25) + geom_density() + 
  coord_cartesian(xlim=c(0, 1000)) +
  facet_wrap(~season) + ggtitle("peak ILI count during flu season")
ggsave(sprintf("distr_pkCount_%sILI%s.png", code, code2), plt.distr.pkCount, width=w, height=h)

# epidemic duration plot
plt.distr.epiDur <- ggplot(dbMetrics.g %>% filter(metric=='epi.dur'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=1) + geom_density() + 
  #coord_cartesian(xlim=c(0, 30)) +
  facet_wrap(~season) + ggtitle("epidemic duration (weeks) during flu season")
ggsave(sprintf("distr_epiDur_%sILI%s.png", code, code2), plt.distr.epiDur, width=w, height=h)

# epidemic timing plot
plt.distr.epiTime <- ggplot(dbMetrics.g %>% filter(metric=='wks.to.epi'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=1) + geom_density() + 
  #coord_cartesian(xlim=c(0, 30)) +
  facet_wrap(~season) + ggtitle("weeks to epidemic start during flu season")
ggsave(sprintf("distr_epiTime_%sILI%s.png", code, code2), plt.distr.epiTime, width=w, height=h)

# peak timing plot
plt.distr.pkTime <- ggplot(dbMetrics.g %>% filter(metric=='wks.to.peak'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=1) + geom_density() + 
  #coord_cartesian(xlim=c(0, 30)) +
  facet_wrap(~season) + ggtitle("weeks to peak during epidemic")
ggsave(sprintf("distr_pkTime_%sILI%s.png", code, code2), plt.distr.pkTime, width=w, height=h)

# FINDING: magnitude metrics could be truncated and shifted normals, but timing metrics don't appear to be normally distributed
####################################
# compare the mean and variance for each metric by season
metric.summ <- dbMetrics.g %>% group_by(season, metric) %>% summarise(MN = mean(burden), VAR = var(burden))

