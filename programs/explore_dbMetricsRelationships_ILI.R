## Name: Elizabeth Lee
## Date: 9/16/15
## Function: explore the relationships of metrics related to epidemic size (3), peak size, and epidemic duration (ILI version)
## Results: Epidemic size is positively correlated with peak size, but a wide range of epidemic durations (4-31 weeks) may exist across epidemic size. Larger episizes (>=10,000 indiv) had "mid-range" epidemic durations (~10-20 weeks long), and epidemic durations above 20 weeks long were much sparser (which is what you would expect). These results are more promising than the IR plots.
### disease burden metrics: sum ILI across epidemic weeks, cumulative difference in ILI and baseline, cumulative difference in ILI and epidemic threshold, rate of ILI at epidemic peak, epidemic duration
## Filenames: sprintf('dbMetrics_periodicReg_%sLI%s_analyzeDB.csv', code, code2)
## Data Source: IMS Health 
## Notes: 
## 9/15/15 - Refer to explore_fluSeasonDefinition_ILI.R for explanation of "flu epidemic". Zip3s are considered to have experienced a flu epidemic if they had 4+ consecutive weeks above the epidemic threshold in the flu period.
## 9/15/15 - Refer to write_relativeDiseaseBurden_ILI.R for export of disease burden metrics data file.
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

rm(list = ls())
#### header ####################################
require(ggplot2)
require(readr)
require(dplyr)
require(tidyr)
require(gridExtra)
setwd(dirname(sys.frame(1)$ofile))

#### set these! ####################################
# code <- "t2sa_" # semi-annual periodicity
# code <- "t2_" # parabolic time trend term
# code <- "" # linear time trend term
code <- "t4_" # quartic time trend term 

# code2 <- "_Oct" # fluseason = Oct to Apr
code2 <- "_Octfit" # fit = Apr to Oct and fluseason = Oct to Apr

#### import data ####################################
setwd('../R_export')
dbMetrics.g <- read.csv(sprintf('dbMetrics_periodicReg_%sILI%s_analyzeDB.csv', code, code2), header=T, colClasses=c(zipname="character", metric="character")) %>% filter(season!=1)
dbMetrics <- spread(dbMetrics.g, metric, burden)

#### plot formatting ####################################
w = 9
h = 6
#### plots ####################################
# plot the following relationships between dbMetrics:
# 1) total ILI across epidemic weeks vs. (peak ILI count or epidemic duration)
# 2) cumulative diff in ILI & BL vs. (peak ILI count or epidemic duration)
# 3) cumulative diff in ILI & epi thresh vs. (peak ILI count or epidemic duration)

# figures saved 9/16/15 morning
dir.create(sprintf('../graph_outputs/explore_dbMetricsRelationships_%sILI%s', code, code2), showWarnings=FALSE)
setwd(sprintf('../graph_outputs/explore_dbMetricsRelationships_%sILI%s', code, code2))
ggsave <- ggplot2::ggsave; body(ggsave) <- body(ggplot2::ggsave)[-2] # hack to bypass checks in ggsave that allow only ggplot objects to be saved (see evernote "ggsave a gtable object")

#######################
# 1) total ILI across epidemic weeks vs. (peak ILI count or epidemic duration)
plt.ILItot.pkCt <- ggplot(dbMetrics, aes(x=ili.sum, y=ili.peak)) +
  geom_point(aes(color=as.character(season))) +
#   scale_color_discrete(breaks=2:9, name="season") +
  guides(color="none") +
  ggtitle("total ILI vs. peak ILI")
# ggsave(sprintf("ILITot_pkCount_%sILI%s.png", code, code2), plt.ILItot.pkCt, width=w, height=h)

plt.ILItot.epiDur <- ggplot(dbMetrics, aes(x=ili.sum, y=epi.dur)) +
  geom_point(aes(color=as.character(season))) +
  scale_color_discrete(breaks=2:9, name="season") +
  ggtitle("total ILI vs. epidemic duration")
# ggsave(sprintf("ILITot_epiDur_%sILI%s.png", code, code2), plt.ILItot.epiDur, width=w, height=h)

# save as 2-panel figure
ggsave(sprintf("ILITot_pkCount-epiDur_%sIR.png", code), grid.arrange(plt.ILItot.pkCt, plt.ILItot.epiDur, ncol=2), width=w, height=h)

########################
# 2) cumulative diff in ILI & BL vs. (peak ILI count or epidemic duration)
plt.ILIexcessBL.pkCount <- ggplot(dbMetrics, aes(x=ili.excess.BL, y=ili.peak)) +
  geom_point(aes(color=as.character(season))) +
#   scale_color_discrete(breaks=2:9, name="season") +
  guides(color="none") +
  ggtitle("BL excess vs. peak ILI")
# ggsave(sprintf("ILIexcessBL_pkCount_%sILI%s.png", code, code2), plt.ILIexcessBL.pkCount, width=w, height=h)

plt.ILIexcessBL.epiDur <- ggplot(dbMetrics, aes(x=ili.excess.BL, y=epi.dur)) +
  geom_point(aes(color=as.character(season))) +
  scale_color_discrete(breaks=2:9, name="season") +
  ggtitle("BL excess vs. epidemic duration")
# ggsave(sprintf("ILIexcessBL_epiDur_%sILI%s.png", code, code2), plt.ILIexcessBL.epiDur, width=w, height=h)

# save as 2-panel figure
ggsave(sprintf("ILIexcessBL_pkCount-epiDur_%sILI%s.png", code, code2), grid.arrange(plt.ILIexcessBL.pkCount, plt.ILIexcessBL.epiDur, ncol=2), width=w, height=h)

########################
# 3) cumulative diff in ILI & epi thresh vs. (peak ILI count or epidemic duration)
plt.ILIexcessThresh.pkCount <- ggplot(dbMetrics, aes(x=ili.excess.thresh, y=ili.peak)) +
  geom_point(aes(color=as.character(season))) +
#   scale_color_discrete(breaks=2:9, name="season") +
  guides(color="none") +
  ggtitle("threshold excess vs. peak ILI")
# ggsave(sprintf("ILIexcessThresh_pkCount_%sILI%s.png", code, code2), plt.ILIexcessThresh.pkCount, width=w, height=h)

plt.ILIexcessThresh.epiDur <- ggplot(dbMetrics, aes(x=ili.excess.thresh, y=epi.dur)) +
  geom_point(aes(color=as.character(season))) +
  scale_color_discrete(breaks=2:9, name="season") +
  ggtitle("threshold excess vs. epidemic duration")
# ggsave(sprintf("IRexcessThresh_epiDur_%sIR.png", code), plt.ILIexcessThresh.epiDur, width=w, height=h)

# save as 2-panel figure
ggsave(sprintf("ILIexcessThresh_pkCount-epiDur_%sILI%s.png", code, code2), grid.arrange(plt.ILIexcessThresh.pkCount, plt.ILIexcessThresh.epiDur, ncol=2), width=w, height=h)
