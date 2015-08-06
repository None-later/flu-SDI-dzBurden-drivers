## Name: Elizabeth Lee
## Date: 6/4/15
## Function: explore distributions of disease burden metrics -- are they normally distributed, meaning that a z-score normalization would be appropriate?
## Results: They are not normally distributed.
### disease burden metrics: mean IR across epidemic weeks, cumulative difference in IR and baseline, cumulative difference in IR and epidemic threshold, rate of ILI at epidemic peak, epidemic duration
## Filenames: sprintf('dbMetrics_periodicReg_%sanalyzeDB.csv', code)
## Data Source: IMS Health 
## Notes: 
## 5/31/15 - Refer to explore_fluSeasonDefinition_IR.R for explanation of "flu epidemic" is defined. Zip3s are considered to have experienced a flu epidemic if they had 4+ consecutive weeks above the epidemic threshold in the flu period.
## 6/4/15 - Refer to analyze_relativeDiseaseBurden_IR.R for export of disease burden metrics data file.
## 8/6/15 - Examine distributions of new epidemic timing and peak timing disease burden metrics 
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
# uniform plot formatting
w = 9
h = 6

####################################
# plot distribution of dbMetrics (to see if zscore is appropriate)
# 7/27/15 - saved figures
setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_dbMetricsDistribution_%sIR', code))

# IR mean plot
plt.distr.IRmean <- ggplot(dbMetrics.g %>% filter(metric=='IR.mean'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.005) + geom_density() + 
  #coord_cartesian(xlim=c(0, 0.35)) +
  facet_wrap(~season) + ggtitle("mean IR during flu season")
ggsave(sprintf("distr_IRmn_%sIR.png", code), plt.distr.IRmean, width=w, height=h)

# IR in excess of modeled seasonal baseline
plt.distr.IRexcessBL <- ggplot(dbMetrics.g %>% filter(metric=='IR.excess.BL'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.05) + geom_density() + 
  #coord_cartesian(xlim=c(0, 1.5)) +
  facet_wrap(~season) + ggtitle("IR in excess of modeled seasonal baseline during flu season")
ggsave(sprintf("distr_IRexcessBL_%sIR.png", code), plt.distr.IRexcessBL, width=w, height=h)

# IR in excess of modeled epidemic threshold (BL + 1.96*se)
plt.distr.IRexcessThresh <- ggplot(dbMetrics.g %>% filter(metric=='IR.excess.thresh'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.05) + geom_density() + 
  #coord_cartesian(xlim=c(0, 1.5)) +
  facet_wrap(~season) + ggtitle("IR in excess of modeled epidemic threshold during flu season")
ggsave(sprintf("distr_IRexcessThresh_%sIR.png", code), plt.distr.IRexcessThresh, width=w, height=h)

# IR peak rate plot
plt.distr.pkRate <- ggplot(dbMetrics.g %>% filter(metric=='IR.peak'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.005) + geom_density() + 
  #coord_cartesian(xlim=c(0, 0.5)) +
  facet_wrap(~season) + ggtitle("peak IR during flu season")
ggsave(sprintf("distr_pkRate_%sIR.png", code), plt.distr.pkRate, width=w, height=h)

# epidemic duration plot
plt.distr.epiDur <- ggplot(dbMetrics.g %>% filter(metric=='epi.dur'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=1) + geom_density() + 
  #coord_cartesian(xlim=c(0, 30)) +
  facet_wrap(~season) + ggtitle("epidemic duration (weeks) during flu season")
ggsave(sprintf("distr_epiDur_%sIR.png", code), plt.distr.epiDur, width=w, height=h)

# epidemic timing plot
plt.distr.epiTime <- ggplot(dbMetrics.g %>% filter(metric=='wks.to.epi'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=1) + geom_density() + 
  #coord_cartesian(xlim=c(0, 30)) +
  facet_wrap(~season) + ggtitle("weeks to epidemic start during flu season")
ggsave(sprintf("distr_epiTime_%sIR.png", code), plt.distr.epiTime, width=w, height=h)

# peak timing plot
plt.distr.pkTime <- ggplot(dbMetrics.g %>% filter(metric=='wks.to.peak'), aes(x=burden, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=1) + geom_density() + 
  #coord_cartesian(xlim=c(0, 30)) +
  facet_wrap(~season) + ggtitle("weeks to peak during epidemic")
ggsave(sprintf("distr_pkTime_%sIR.png", code), plt.distr.pkTime, width=w, height=h)

# FINDING: plots by season are not normally distributed for any of the metrics
# Although the distributions are not normally distributed, the distributions across seasons are fairly similar for each disease burden metric, which might suggest that the standardized values should comparable across seasons. Standardization is still beneficial because it will render the values more comparable, but it will not help with comparisons between disease burden metrics.
# 7/28/15: on the other hand, it seems that the standardized distributions are similar to the raw distributions. what is the benefit of the transformation?
####################################
# compare the mean and variance for each metric by season
metric.summ <- dbMetrics.g %>% group_by(season, metric) %>% summarise(MN = mean(burden), VAR = var(burden))

####################################
# plot distribution of dbMetrics.gz (STANDARDIZED PLOTS)
# 7/27/15: save figures
setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_dbMetricsDistribution_%sIR', code))

# standardized IR mean plot
plt.distr.IRmean.z <- ggplot(dbMetrics.gz %>% filter(metric=='IR.mean'), aes(x=burden.z, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.1) + geom_density() + 
  #coord_cartesian(xlim=c(-1.5, 3)) +
  facet_wrap(~season) + ggtitle("std mean IR during flu season")
ggsave(sprintf("zDistr_IRmn_%sIR.png", code), plt.distr.IRmean.z, width=w, height=h)

# standardized IR in excess of modeled seasonal baseline
plt.distr.IRexcessBL.z <- ggplot(dbMetrics.gz %>% filter(metric=='IR.excess.BL'), aes(x=burden.z, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.1) + geom_density() + 
  #coord_cartesian(xlim=c(-1.5, 5.5)) +
  facet_wrap(~season) + ggtitle("std IR in excess of modeled seasonal baseline during flu season")
ggsave(sprintf("zDistr_IRexcessBL_%sIR.png", code), plt.distr.IRexcessBL.z, width=w, height=h)

# IR in excess of modeled epidemic threshold (BL + 1.96*se)
plt.distr.IRexcessThresh.z <- ggplot(dbMetrics.gz %>% filter(metric=='IR.excess.thresh'), aes(x=burden.z, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.1) + geom_density() + 
  #coord_cartesian(xlim=c(-1.5, 5.5)) +
  facet_wrap(~season) + ggtitle("std IR in excess of modeled epidemic threshold during flu season")
ggsave(sprintf("zDistr_IRexcessThresh_%sIR.png", code), plt.distr.IRexcessThresh.z, width=w, height=h)

# peak rate plot
plt.distr.pkRate.z <- ggplot(dbMetrics.gz %>% filter(metric=='IR.peak'), aes(x=burden.z, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.1) + geom_density() + 
  #coord_cartesian(xlim=c(-1.5, 3)) +
  facet_wrap(~season) + ggtitle("std peak IR during flu season")
ggsave(sprintf("zDistr_pkRate_%sIR.png", code), plt.distr.pkRate.z, width=w, height=h)

# epidemic duration plot
plt.distr.epiDur.z <- ggplot(dbMetrics.gz %>% filter(metric=='epi.dur'), aes(x=burden.z, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.1) + geom_density() + 
  #coord_cartesian(xlim=c(-3, 4)) +
  facet_wrap(~season) + ggtitle("std epidemic duration (weeks) during flu season")
ggsave(sprintf("zDistr_epiDur_%sIR.png", code), plt.distr.epiDur.z, width=w, height=h)

# epidemic timing plot
plt.distr.epiTime.z <- ggplot(dbMetrics.gz %>% filter(metric=='wks.to.epi'), aes(x=burden.z, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.1) + geom_density() + 
  #coord_cartesian(xlim=c(0, 30)) +
  facet_wrap(~season) + ggtitle("std weeks to epidemic start during flu season")
ggsave(sprintf("zDistr_epiTime_%sIR.png", code), plt.distr.epiTime.z, width=w, height=h)

# peak timing plot
plt.distr.pkTime.z <- ggplot(dbMetrics.gz %>% filter(metric=='wks.to.peak'), aes(x=burden.z, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.1) + geom_density() + 
  #coord_cartesian(xlim=c(0, 30)) +
  facet_wrap(~season) + ggtitle("std weeks to peak during epidemic")
ggsave(sprintf("zDistr_pkTime_%sIR.png", code), plt.distr.pkTime.z, width=w, height=h)
