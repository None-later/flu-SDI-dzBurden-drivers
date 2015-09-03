## Name: Elizabeth Lee
## Date: 9/2/15
## Function: explore epidemic threshold and epidemic period distributions to think of ways to filter out noise (that doesn't appear to be a flu epidemic)
## Results: 

### disease burden metrics: 
## Filenames: sprintf('dbMetrics_periodicReg_%sanalyzeDB.csv', code)
## Data Source: IMS Health 
## Notes: 

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
# import db metrics data
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')
dbMetrics.g <- read.csv(sprintf('dbMetrics_periodicReg_%sanalyzeDB.csv', code), header=T, colClasses=c(zipname="character", metric="character"))
# standardized data
dbMetrics.gz <- dbMetrics.g %>% group_by(season, metric) %>% mutate(burden.z = (burden - mean(burden))/sd(burden))

# import time series data
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')
fullIndic <- read_csv(file=sprintf('fullIndicAll_periodicReg_%sanalyzeDB.csv', code), col_types=list(zipname=col_character()))

####################################
# uniform plot formatting
w = 9
h = 6
ct = 6

####################################
# process plotting data
data_plot <- fullIndic %>% filter(flu.week) %>% filter (season != 1) %>% group_by(season, zipname) %>% summarise(mn.thresh = mean(epi.thresh, na.rm=T), mn.IR = mean(IR, na.rm=T)) %>% ungroup
####################################
# quantiles dataset
data_plot_q <- data_plot %>% group_by(season) %>% summarise(thresh10 = quantile(mn.thresh, probs=0.1, na.rm=T), thresh25 = quantile(mn.thresh, probs=0.25, na.rm=T), thresh50 = quantile(mn.thresh, probs=0.50, na.rm=T), thresh75 = quantile(mn.thresh, probs=0.75, na.rm=T), thresh90 = quantile(mn.thresh, probs=0.9, na.rm=T), IR10 = quantile(mn.IR, probs=0.1, na.rm=T), IR25 = quantile(mn.IR, probs=0.25, na.rm=T), IR50 = quantile(mn.IR, probs=0.5, na.rm=T), IR75 = quantile(mn.IR, probs=0.75, na.rm=T), IR90 = quantile(mn.IR, probs=0.9, na.rm=T))
####################################
# plot mean epidemic threshold histograms
setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_epidemicPeriodDefinition_%sIR', code))
# mean threshold pooled across seasons
epiThresh.pooled <- ggplot(data_plot, aes(x=mn.thresh)) +
  geom_histogram(aes(y=..density..), alpha=0.5, position="dodge") 
ggsave(sprintf("mnEpithresh_pooled_%sIR.png", code), epiThresh.pooled, width=4, height=4)
# mean threshold by season
epiThresh.byseas <- ggplot(data_plot, aes(x=mn.thresh, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=0.001, alpha=0.5, position="dodge") +
  geom_vline(aes(xintercept = c(thresh10, thresh90)), data_plot_q, color='red', size=1) +
  coord_cartesian(xlim=c(0, 0.2)) +
  facet_wrap(~season, scales="fixed")
ggsave(sprintf("mnEpithresh_byseas_%sIR.png", code), epiThresh.byseas, width=w, height=h)
####################################
# plot mean IR histograms
# mean IR pooled across seasons
IR.pooled <- ggplot(data_plot, aes(x=mn.IR)) +
  geom_histogram(aes(y=..density..), alpha=0.5, position="dodge") 
ggsave(sprintf("mnIR_pooled_%sIR.png", code), IR.pooled, width=4, height=4)
# mean IR by season
IR.byseas <- ggplot(data_plot, aes(x=mn.IR)) +
  geom_histogram(aes(y=..density..), binwidth=0.001, alpha=0.5, position="dodge") +
  geom_vline(aes(xintercept = c(IR10, IR90)), data_plot_q, color='red', size=1) +
  coord_cartesian(xlim=c(0, 0.2)) +
  facet_wrap(~season, scales="fixed")
ggsave(sprintf("mnIR_byseas_%sIR.png", code), IR.byseas, width=w, height=h)





