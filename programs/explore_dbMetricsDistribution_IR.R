## Name: Elizabeth Lee
## Date: 6/4/15
## Function: explore distributions of disease burden metrics -- are they normally distributed, meaning that a z-score normalization would be appropriate?
### disease burden metrics: total ILI attack rate, rate of ILI at epidemic peak, epidemic duration
## Filenames: sprintf('dbMetrics_periodicReg_%sanalyzeDB.csv', code)
## Data Source: IMS Health 
## Notes: 
## 5/31/15 - Refer to explore_fluSeasonDefinition_IR.R for explanation of "flu epidemic" is defined. Zip3s are considered to have experienced a flu epidemic if they had 4+ consecutive weeks above the epidemic threshold in the flu period.
## 6/4/15 - Refer to analyze_relativeDiseaseBurden_IR.R for export of disease burden metrics data file.
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

####################################
# header
setwd('/home/elee/R/source_functions')
source("dfsumm.R")
require(ggplot2)
require(readr)
require(dplyr)
require(tidyr)

####################################
# set these!
# code = "t2sa_" # semi-annual periodicity
code = "t2_" # parabolic time trend term
# code="" # linear time trend term

####################################
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')
dbMetrics.g <- read.csv(sprintf('dbMetrics_periodicReg_%sanalyzeDB.csv', code), header=T, colClasses=c(zipname="character", metric="character"))
# standardized data
dbMetrics.gz <- dbMetrics.g %>% group_by(season, metric) %>% mutate(burden.z = (burden - mean(burden))/sd(burden))

####################################
# plot distribution of dbMetrics (to see if zscore is appropriate)
setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_dbMetricsDistribution_%sIR', code))

# attack rate plot
plt.distr.totAR <- ggplot(dbMetrics.g %>% filter(metric=='tot.ar'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.005) + geom_density() + 
  coord_cartesian(xlim=c(0, 0.35)) +
  facet_wrap(~season) + ggtitle("tot.ar (proxy) during flu season")
ggsave(sprintf("distr_totAR_%sIR.png", code), plt.distr.totAR, width=9, height=6)

# peak rate plot
plt.distr.pkRate <- ggplot(dbMetrics.g %>% filter(metric=='peak.rate'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.005) + geom_density() + 
  coord_cartesian(xlim=c(0, 0.5)) +
  facet_wrap(~season) + ggtitle("peak rate during flu season")
ggsave(sprintf("distr_pkRate_%sIR.png", code), plt.distr.pkRate, width=9, height=6)

# epidemic duration plot
plt.distr.epiDur <- ggplot(dbMetrics.g %>% filter(metric=='epi.dur'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=1) + geom_density() + 
  coord_cartesian(xlim=c(0, 30)) +
  facet_wrap(~season) + ggtitle("epidemic duration (weeks) during flu season")
ggsave(sprintf("distr_epiDur_%sIR.png", code), plt.distr.epiDur, width=9, height=6)

# FINDING: plots by season are not normally distributed for any of the metrics
####################################
# compare the mean and variance for each metric by season
metric.summ <- dbMetrics.g %>% group_by(season, metric) %>% summarise(MN = mean(burden), VAR = var(burden))

####################################
# plot distribution of dbMetrics.gz (STANDARDIZED PLOTS)
setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_dbMetricsDistribution_%sIR', code))

# attack rate plot
plt.distr.totAR.z <- ggplot(dbMetrics.gz %>% filter(metric=='tot.ar'), aes(x=burden.z, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.1) + geom_density() + 
  coord_cartesian(xlim=c(-1.5, 3)) +
  facet_wrap(~season) + ggtitle("std tot.ar (proxy) during flu season")
ggsave(sprintf("zDistr_totAR_%sIR.png", code), plt.distr.totAR.z, width=9, height=6)

# peak rate plot
plt.distr.pkRate.z <- ggplot(dbMetrics.gz %>% filter(metric=='peak.rate'), aes(x=burden.z, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.1) + geom_density() + 
  coord_cartesian(xlim=c(-1.5, 3)) +
  facet_wrap(~season) + ggtitle("std peak rate during flu season")
ggsave(sprintf("zDistr_pkRate_%sIR.png", code), plt.distr.pkRate.z, width=9, height=6)

# epidemic duration plot
plt.distr.epiDur.z <- ggplot(dbMetrics.gz %>% filter(metric=='epi.dur'), aes(x=burden.z, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.1) + geom_density() + 
  coord_cartesian(xlim=c(-3, 4)) +
  facet_wrap(~season) + ggtitle("std epidemic duration (weeks) during flu season")
ggsave(sprintf("zDistr_epiDur_%sIR.png", code), plt.distr.epiDur.z, width=9, height=6)
