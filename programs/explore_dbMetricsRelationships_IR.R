## Name: Elizabeth Lee
## Date: 8/4/15
## Function: explore the relationships of metrics related to epidemic size (3), peak size, and epidemic duration
## Results: Epidemic size is positively correlated with peak size, but the relationship to epidemic duration is not clear or necessarily consistent across epidemic size metrics.
### disease burden metrics: mean IR across epidemic weeks, cumulative difference in IR and baseline, cumulative difference in IR and epidemic threshold, rate of ILI at epidemic peak, epidemic duration
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
require(gridExtra)

####################################
# set these!
# code = "t2sa_" # semi-annual periodicity
code = "t2_" # parabolic time trend term
# code="" # linear time trend term

####################################
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')
dbMetrics.g <- read.csv(sprintf('dbMetrics_periodicReg_%sanalyzeDB.csv', code), header=T, colClasses=c(zipname="character", metric="character"))
dbMetrics <- spread(dbMetrics.g, metric, burden)
####################################
# uniform plot formatting
w = 9
h = 6
####################################
# plot the following relationships between dbMetrics:
# 1) mean IR across epidemic weeks vs. (peak ILI rate or epidemic duration)
# 2) cumulative diff in IR & BL vs. (peak ILI rate or epidemic duration)
# 3) cumulative diff in IR & epi thresh vs. (peak ILI rate or epidemic duration)

setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_dbMetricsRelationships_%sIR', code))
ggsave <- ggplot2::ggsave; body(ggsave) <- body(ggplot2::ggsave)[-2] # hack to bypass checks in ggsave that allow only ggplot objects to be saved (see evernote "ggsave a gtable object")

#######################
# 1) mean IR vs. (peak ILI rate or epidemic duration)
plt.IRmean.pkRate <- ggplot(dbMetrics, aes(x=IR.mean, y=IR.peak)) +
  geom_point(aes(color=as.character(season))) +
#   scale_color_discrete(breaks=2:9, name="season") +
  guides(color="none") +
  ggtitle("mean IR vs. peak IR")
# ggsave(sprintf("IRmn_pkRate_%sIR.png", code), plt.IRmean.pkRate, width=w, height=h)

plt.IRmean.epiDur <- ggplot(dbMetrics, aes(x=IR.mean, y=epi.dur)) +
  geom_point(aes(color=as.character(season))) +
  scale_color_discrete(breaks=2:9, name="season") +
  ggtitle("mean IR vs. epidemic duration")
# ggsave(sprintf("IRmn_epiDur_%sIR.png", code), plt.IRmean.epiDur, width=w, height=h)

# save as 2-panel figure
ggsave(sprintf("IRmn_pkRate-epiDur_%sIR.png", code), grid.arrange(plt.IRmean.pkRate, plt.IRmean.epiDur, ncol=2), width=w, height=h)

########################
# 2) cumulative diff in IR & BL vs. (peak ILI rate or epidemic duration)
plt.IRexcessBL.pkRate <- ggplot(dbMetrics, aes(x=IR.excess.BL, y=IR.peak)) +
  geom_point(aes(color=as.character(season))) +
#   scale_color_discrete(breaks=2:9, name="season") +
  guides(color="none") +
  ggtitle("BL excess vs. peak IR")
# ggsave(sprintf("IRexcessBL_pkRate_%sIR.png", code), plt.IRexcessBL.pkRate, width=w, height=h)

plt.IRexcessBL.epiDur <- ggplot(dbMetrics, aes(x=IR.mean, y=epi.dur)) +
  geom_point(aes(color=as.character(season))) +
  scale_color_discrete(breaks=2:9, name="season") +
  ggtitle("BL excess vs. epidemic duration")
# ggsave(sprintf("IRexcessBL_epiDur_%sIR.png", code), plt.IRexcessBL.epiDur, width=w, height=h)

# save as 2-panel figure
ggsave(sprintf("IRexcessBL_pkRate-epiDur_%sIR.png", code), grid.arrange(plt.IRexcessBL.pkRate, plt.IRexcessBL.epiDur, ncol=2), width=w, height=h)

########################
# 3) cumulative diff in IR & epi thresh vs. (peak ILI rate or epidemic duration)
plt.IRexcessThresh.pkRate <- ggplot(dbMetrics, aes(x=IR.excess.thresh, y=IR.peak)) +
  geom_point(aes(color=as.character(season))) +
#   scale_color_discrete(breaks=2:9, name="season") +
  guides(color="none") +
  ggtitle("threshold excess vs. peak IR")
# ggsave(sprintf("IRexcessThresh_pkRate_%sIR.png", code), plt.IRexcessThresh.pkRate, width=w, height=h)

plt.IRexcessThresh.epiDur <- ggplot(dbMetrics, aes(x=IR.excess.thresh, y=epi.dur)) +
  geom_point(aes(color=as.character(season))) +
  scale_color_discrete(breaks=2:9, name="season") +
  ggtitle("threshold excess vs. epidemic duration")
# ggsave(sprintf("IRexcessThresh_epiDur_%sIR.png", code), plt.IRexcessThresh.epiDur, width=w, height=h)

# save as 2-panel figure
ggsave(sprintf("IRexcessThresh_pkRate-epiDur_%sIR.png", code), grid.arrange(plt.IRexcessThresh.pkRate, plt.IRexcessThresh.epiDur, ncol=2), width=w, height=h)
