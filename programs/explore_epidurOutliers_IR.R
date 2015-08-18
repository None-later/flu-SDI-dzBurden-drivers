## Name: Elizabeth Lee
## Date: 8/18/15
## Function: examine zip3-season time series with short and long epidemic durations; Does the time series appear to follow an expected epidemic pattern?
## Results: In most cases, the long epidemic durations appear to correspond with true epidemics. In some cases, however, it appears that there is a lot of noise during the epidemic and the main peak is in fact much shorter. The short epidemic durations do not appear to capture the full epidemic dynamics well.
# Legend: black line at 0 means epi.week=T, other color designations refer to in.season variable

### disease burden metrics: epidemic duration after epidemic start
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
# LONG DURATIONS (GREATER THAN OR EQUAL TO 20 WEEKS)
####################################
# examine ts of zip-seasons with long epidemic durations
db.dur20 <- dbMetrics.g %>% filter(metric=="epi.dur" & burden>=20) %>% mutate(id.combo = paste0(season, zipname))
zip3list1 <- db.dur20 %>% select(zipname) %>% distinct %>% mutate(for.plot = seq_along(1:nrow(.))) # id all zip3s with long durations

# subset full data
fi.dur20.all <- fullIndic %>% filter(zipname %in% zip3list1$zipname) %>% filter(season!=1)
data_plot <- right_join(fi.dur20.all, zip3list1, by='zipname')

####################################
# plot full time series for zip3s with long durations
indexes <- seq(1, max(data_plot %>% select(for.plot)), by=ct)

# IR plots
setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_epidurOutliers_%sIR/over20/IR',code))
for(i in indexes){
  dummyplots <- ggplot(data_plot %>% filter(for.plot>= i & for.plot < i+ct)), aes(x=week, y=IR, group=zipname)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = in.season)) + scale_color_brewer(palette="Set1") +
#     coord_cartesian(ylim=c(0, 0.2)) + 
    facet_wrap(~zipname)
  # grab zip3s in plot for file name
  ziplabels <- data_plot %>% select(zipname) %>% distinct %>% slice(c(i, i+ct-1)) 
  ggsave(sprintf("longEpiDur_%sfits_IR_%s-%s.png", code, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
}

# ili plots
setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_epidurOutliers_%sIR/over20/iliProp', code))
for(i in indexes){
  dummyplots <- ggplot(data_plot %>% filter(for.plot>= i & for.plot < i+ct), aes(x=week, y=ili, group=zipname)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = in.season)) + scale_color_brewer(palette="Set1")
#     coord_cartesian(ylim=c(0, 0.2)) + 
    facet_wrap(~zipname)
  # grab zip3s in plot for file name
  ziplabels <- data_plot %>% select(zipname) %>% distinct %>% slice(c(i, i+ct-1)) 
  ggsave(sprintf("longEpiDur_%sfits_iliProp_%s-%s.png", code, ziplabels[1,], ziplabels[2,]), dummyplots, width=9, height=6)
}

####################################
# subset season-zip3 combinations in db.dur20 only
fi.dur20.seas <- fullIndic %>% mutate(id.combo = paste0(season, zipname)) %>% filter(id.combo %in% db.dur20$id.combo)
zip3list2 <- fi.dur20.seas %>% select(id.combo) %>% distinct %>% mutate(for.plot = seq_along(1:nrow(.)))
data_plot2 <- right_join(fi.dur20.seas, zip3list2, by="id.combo")

####################################
# plot epidemic time series for zip3-season combinations with long durations
indexes2 <- seq(1, max(data_plot2 %>% select(for.plot)), by=ct)

# IR plots by season
setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_epidurOutliers_%sIR/over20/bySeason/IR', code))
for(i in indexes2){
  dummyplots <- ggplot(data_plot2 %>% filter(for.plot>= i & for.plot < i+ct) %>% mutate(is.epiweek = ifelse(epi.week, 0, NA)), aes(x=Thu.week, y=IR, group=id.combo)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = in.season)) + scale_color_brewer(palette="Set1") +
    geom_line(aes(y = is.epiweek), color = 'black') + # appears if epi.week=T
    facet_wrap(~id.combo, scales = "free")
  # grab zip3s in plot for file name
  ziplabels <- data_plot2 %>% select(id.combo) %>% distinct %>% slice(c(i, i+ct-1)) 
  ggsave(sprintf("longEpiDur_seas_%sfits_IR_%s-%s.png", code, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
}

# ili proportion by season
setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_epidurOutliers_%sIR/over20/bySeason/iliProp', code))
for(i in indexes2){
  dummyplots <- ggplot(data_plot2 %>% filter(for.plot>= i & for.plot < i+ct) %>% mutate(is.epiweek = ifelse(epi.week, 0, NA)), aes(x=Thu.week, y=ili, group=id.combo)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = in.season)) + scale_color_brewer(palette="Set1") +
    geom_line(aes(y = is.epiweek), color = 'black') + # appears if epi.week=T
    facet_wrap(~id.combo, scales = "free")
  # grab zip3s in plot for file name
  ziplabels <- data_plot2 %>% select(id.combo) %>% distinct %>% slice(c(i, i+ct-1)) 
  ggsave(sprintf("longEpiDur_seas_%sfits_iliProp_%s-%s.png", code, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
}

####################################
# SHORT DURATIONS (LESS THAN OR EQUAL TO 5 WEEKS)
####################################
# examine ts of zip-seasons with short epidemic durations
db.dur5 <- dbMetrics.g %>% filter(metric=="epi.dur" & burden<=5) %>% mutate(id.combo = paste0(season, zipname))

# subset season-zip3 combinations in db.dur8 only
fi.dur5.seas <- fullIndic %>% filter(season != 1) %>% mutate(id.combo = paste0(season, zipname)) %>% filter(id.combo %in% db.dur5$id.combo)
zip3list3 <- fi.dur5.seas %>% select(id.combo) %>% distinct %>% mutate(for.plot = seq_along(1:nrow(.)))
data_plot3 <- right_join(fi.dur5.seas, zip3list3, by="id.combo")

####################################
# plot epidemic time series for zip3-season combinations with short durations
indexes3 <- seq(1, max(data_plot3 %>% select(for.plot)), by=ct)

# IR plots by season
setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_epidurOutliers_%sIR/under5/bySeason/IR', code))
for(i in indexes3){
  dummyplots <- ggplot(data_plot3 %>% filter(for.plot>= i & for.plot < i+ct) %>% mutate(is.epiweek = ifelse(epi.week, 0, NA)), aes(x=Thu.week, y=IR, group=id.combo)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = in.season)) + scale_color_brewer(palette="Set1") +
    geom_line(aes(y = is.epiweek), color = 'black') + # appears if epi.week=T
    facet_wrap(~id.combo, scales = "free")
  # grab zip3s in plot for file name
  ziplabels <- data_plot3 %>% select(id.combo) %>% distinct %>% slice(c(i, i+ct-1)) 
  ggsave(sprintf("shortEpiDur_seas_%sfits_IR_%s-%s.png", code, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
}

# ili proportion by season
setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_epidurOutliers_%sIR/under5/bySeason/iliProp', code))
for(i in indexes3){
  dummyplots <- ggplot(data_plot3 %>% filter(for.plot>= i & for.plot < i+ct) %>% mutate(is.epiweek = ifelse(epi.week, 0, NA)), aes(x=Thu.week, y=ili, group=id.combo)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = in.season)) + scale_color_brewer(palette="Set1") +
    geom_line(aes(y = is.epiweek), color = 'black') + # appears if epi.week=T    
    facet_wrap(~id.combo, scales = "free")
  # grab zip3s in plot for file name
  ziplabels <- data_plot3 %>% select(id.combo) %>% distinct %>% slice(c(i, i+ct-1)) 
  ggsave(sprintf("shortEpiDur_seas_%sfits_iliProp_%s-%s.png", code, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
}

# all files saved 08/18/15 evening
