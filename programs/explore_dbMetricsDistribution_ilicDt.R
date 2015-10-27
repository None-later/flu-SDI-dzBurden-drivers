## Name: Elizabeth Lee
## Date: 10/26/15
## Function: explore distributions of disease burden metrics for ilicDt-- are they normally distributed?
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

explore_dbMetricsDistribution_ilicDt <- function(span.var, degree.var){
  #### header ####################################
  require(ggplot2)
  require(readr)
  require(dplyr)
  require(tidyr)
  require(microbenchmark)
  setwd(dirname(sys.frame(1)$ofile))
  
  #### set these! ####################################
  code <-"" # linear time trend term
  code2 <- "_Octfit" # fit = Apr to Oct and fluseason = Oct to Apr
  
#   span.var <- 0.5 # 0.4, 0.6
#   degree.var <- 2
  code.str <- sprintf('_span%s_degree%s', span.var, degree.var)
  
  #### import data ####################################
  setwd('../R_export')
  dbMetrics.g <- read.csv(sprintf('dbMetrics_periodicReg_%silicDt%s%s_analyzeDB.csv', code, code2, code.str), header=T, colClasses=c(zip3="character", metric="character")) %>% filter(season!=1)
  # standardized data
  dbMetrics.gz <- dbMetrics.g %>% group_by(season, metric) %>% mutate(burden.z = (burden - mean(burden))/sd(burden))
  
  #### plot formatting ####################################
  w = 6
  h = 4
  
  #### plot distribution of dbMetrics ####################################
  # 9/15/15 - saved figures
  print(sprintf('plotting db metrics distribution %s', code.str))
  
  dir.create(sprintf('../graph_outputs/explore_dbMetricsDistribution_%silicDt%s%s', code, code2, code.str), showWarnings=FALSE)
  setwd(sprintf('../graph_outputs/explore_dbMetricsDistribution_%silicDt%s%s', code, code2, code.str))
  
  # total ILI plot
  plt.distr.iliSum <- ggplot(dbMetrics.g %>% filter(metric=='ilicDt.sum'), aes(x=burden, group=season)) +
    geom_histogram(aes(y=..density..), binwidth=10) + geom_density() + 
    coord_cartesian(xlim=c(0, 250)) +
    facet_wrap(~season) + ggtitle("Sum ilicDt during flu season")
  ggsave(sprintf("distr_ILITot_%silicDt%s%s.png", code, code2, code.str), plt.distr.iliSum, width=w, height=h)
  
  # ILI in excess of modeled seasonal baseline
  plt.distr.ILIexcessBL <- ggplot(dbMetrics.g %>% filter(metric=='ilicDt.excess.BL'), aes(x=burden, group=season)) +
    geom_histogram(aes(y=..density..), binwidth=10) + geom_density() + 
    coord_cartesian(xlim=c(0, 200)) +
    facet_wrap(~season) + ggtitle("ilicDt in excess of modeled seasonal baseline during flu season")
  ggsave(sprintf("distr_ILIexcessBL_%silicDt%s%s.png", code, code2, code.str), plt.distr.ILIexcessBL, width=w, height=h)
  
  # ili in excess of modeled epidemic threshold (BL + 1.96*se)
  plt.distr.ILIexcessThresh <- ggplot(dbMetrics.g %>% filter(metric=='ilicDt.excess.thresh'), aes(x=burden, group=season)) +
    geom_histogram(aes(y=..density..), binwidth=10) + geom_density() + 
    coord_cartesian(xlim=c(-150, 250)) +
    facet_wrap(~season) + ggtitle("ilicDt in excess of modeled epidemic threshold during flu season")
  ggsave(sprintf("distr_ILIexcessThresh_%silicDt%s%s.png", code, code2, code.str), plt.distr.ILIexcessThresh, width=w, height=h)
  
  # ili peak case count plot
  plt.distr.pkCount <- ggplot(dbMetrics.g %>% filter(metric=='ilicDt.peak'), aes(x=burden, group=season)) +
    geom_histogram(aes(y=..density..), binwidth=5) + geom_density() + 
    coord_cartesian(xlim=c(0, 50)) +
    facet_wrap(~season) + ggtitle("peak ilicDt count during flu season")
  ggsave(sprintf("distr_pkCount_%silicDt%s%s.png", code, code2, code.str), plt.distr.pkCount, width=w, height=h)
  
  # epidemic duration plot
  plt.distr.epiDur <- ggplot(dbMetrics.g %>% filter(metric=='epi.dur'), aes(x=burden, group=season)) +
    geom_histogram(aes(y=..density..), binwidth=1) + geom_density() + 
    #coord_cartesian(xlim=c(0, 30)) +
    facet_wrap(~season) + ggtitle("epidemic duration (weeks) during flu season")
  ggsave(sprintf("distr_epiDur_%silicDt%s%s.png", code, code2, code.str), plt.distr.epiDur, width=w, height=h)
  
  # epidemic timing plot
  plt.distr.epiTime <- ggplot(dbMetrics.g %>% filter(metric=='wks.to.epi'), aes(x=burden, group=season)) +
    geom_histogram(aes(y=..density..), binwidth=1) + geom_density() + 
    #coord_cartesian(xlim=c(0, 30)) +
    facet_wrap(~season) + ggtitle("weeks to epidemic start during flu season")
  ggsave(sprintf("distr_epiTime_%silicDt%s%s.png", code, code2, code.str), plt.distr.epiTime, width=w, height=h)
  
  # peak timing plot
  plt.distr.pkTime <- ggplot(dbMetrics.g %>% filter(metric=='wks.to.peak'), aes(x=burden, group=season)) +
    geom_histogram(aes(y=..density..), binwidth=1) + geom_density() + 
    #coord_cartesian(xlim=c(0, 30)) +
    facet_wrap(~season) + ggtitle("weeks to peak during epidemic")
  ggsave(sprintf("distr_pkTime_%silicDt%s%s.png", code, code2, code.str), plt.distr.pkTime, width=w, height=h)
  
  print('finished plotting db distributions')
  
  # FINDING: magnitude metrics could be truncated and shifted normals, but timing metrics don't appear to be normally distributed
  ####################################
  # compare the mean and variance for each metric by season
  metric.summ <- dbMetrics.g %>% group_by(season, metric) %>% summarise(MN = mean(burden), VAR = var(burden))
  print(sprintf('span %s degree %s', span.var, degree.var))
  print(metric.summ)
  
}



