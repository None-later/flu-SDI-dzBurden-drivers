## Name: Elizabeth Lee
## Date: 9/16/15
## Function: examine zip3-season time series with short and long epidemic durations; Does the time series appear to follow an expected epidemic pattern?
## Results: Similar to IR epiDur outliers, long epidemics seem more acceptable than short ones, which are noisy and often miss at least part of the epidemic peak if there appears to be one.
# Legend: black line at 0 means epi.week=T, other color designations refer to in.season variable

## 9/21/15: filter zip3-season combos before looking at outliers (write_zip3seasonFiltered_ILI.R), add adjusted R2 (indicator of model fit) to plot

### disease burden metrics: epidemic duration
## Filenames: sprintf('dbMetrics_periodicReg_%sILI%s_analyzeDB.csv', code, code2), sprintf('fullIndicAll_periodicReg_%sILI%s_analyzeDB.csv', code, code2), sprintf('zip3SeasonCombos_%sILI%s.csv', code, code2), sprintf('summaryStats_periodicReg_%sallZip3Modes_ILI%s.csv', code, code2)
## Data Source: IMS Health 
## Notes: 

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
dbMetrics.g <- read.csv(sprintf('dbMetrics_periodicReg_%sILI%s_analyzeDB.csv', code, code2), header=T, colClasses=c(zipname="character", metric="character"))
# standardized data
dbMetrics.gz <- dbMetrics.g %>% group_by(season, metric) %>% mutate(burden.z = (burden - mean(burden))/sd(burden))
# import time series data
fullIndic <- read_csv(file=sprintf('fullIndicAll_periodicReg_%sILI%s_analyzeDB.csv', code, code2), col_types=list(zipname=col_character()))
# import model fit data
modelfit <- read_csv(file=sprintf('summaryStats_periodicReg_%sallZip3Mods_ILI%s.csv', code, code2))
modelfit2 <- modelfit %>% mutate(zipname = substr.Right(gsub("X", "00", zip3), 3))
# import zip3-season combinations
combos <- read.csv(sprintf('zip3SeasonCombos_%sILI%s.csv', code, code2), header=T, colClasses=c(zipname="character"))
combos2 <- combos %>% mutate(id = paste0(season, zipname))

#### plot formatting ####################################
w = 9
h = 6
ct = 6
dir.create(sprintf('../graph_outputs/explore_epidurOutliers_%sILI%s', code, code2), showWarnings=FALSE)
setwd(sprintf('../graph_outputs/explore_epidurOutliers_%sILI%s', code, code2))

# #### LONG DURATIONS (GREATER THAN OR EQUAL TO 20 WEEKS) ####################################
# # examine ts of zip-seasons with long epidemic durations
# db.dur20 <- dbMetrics.g %>% filter(metric=="epi.dur" & burden>=20) %>% mutate(id.combo = paste0(season, zipname))
# zip3list1 <- db.dur20 %>% select(id.combo) %>% distinct %>% mutate(for.plot = seq_along(1:nrow(.))) # id all zip3s with long durations
# 
# # subset full data
# fi.dur20.all <- fullIndic %>% mutate(id.combo = paste0(season, zipname)) %>% filter(id.combo %in% zip3list1$id.combo) %>% filter(season!=1)
# data_plot <- right_join(fi.dur20.all, zip3list1, by='id.combo') %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01"))
# 
# #### subset season-zip3 combinations in db.dur20 only ####################################
# fi.dur20.seas <- fullIndic %>% mutate(id.combo = paste0(season, zipname)) %>% filter(id.combo %in% db.dur20$id.combo)
# zip3list2 <- fi.dur20.seas %>% select(id.combo) %>% distinct %>% mutate(for.plot = seq_along(1:nrow(.)))
# data_plot2 <- right_join(fi.dur20.seas, zip3list2, by="id.combo") %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01")) %>% filter(flu.week)
# 
# #### plot epidemic time series for zip3-season combinations with long durations ####################################
# indexes2 <- seq(1, max(data_plot2 %>% select(for.plot)), by=ct)
# 
# # ILI plots by season
# dir.create(sprintf('./over20', code, code2), showWarnings=FALSE)
# setwd(sprintf('./over20', code, code2))
# for(i in indexes2){
#   dummyplots <- ggplot(data_plot2 %>% filter(for.plot>= i & for.plot < i+ct) %>% mutate(is.epiweek = ifelse(epi.week, 0, NA)), aes(x=Thu.week, y=ili, group=id.combo)) +
#     theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
#     geom_line(aes(color = in.season)) + scale_color_brewer(palette="Set1") +
#     geom_line(aes(y = is.epiweek), color = 'black') + # appears if epi.week=T
#     geom_line(aes(y = epi.thresh), color = 'grey') +
#     facet_wrap(~id.combo, scales = "free")
#   # grab zip3s in plot for file name
#   ziplabels <- data_plot2 %>% select(id.combo) %>% distinct %>% slice(c(i, i+ct-1)) 
#   ggsave(sprintf("longEpiDur_seas_%sfits_ILI%s_%s-%s.png", code, code2, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
# }
# 
# 
# #### SHORT DURATIONS (LESS THAN OR EQUAL TO 5 WEEKS) ####################################
# # examine ts of zip-seasons with short epidemic durations
# db.dur5 <- dbMetrics.g %>% filter(metric=="epi.dur" & burden<=5) %>% mutate(id.combo = paste0(season, zipname))
# 
# # subset season-zip3 combinations in db.dur8 only
# fi.dur5.seas <- fullIndic %>% filter(season != 1) %>% mutate(id.combo = paste0(season, zipname)) %>% filter(id.combo %in% db.dur5$id.combo)
# zip3list3 <- fi.dur5.seas %>% select(id.combo) %>% distinct %>% mutate(for.plot = seq_along(1:nrow(.)))
# data_plot3 <- right_join(fi.dur5.seas, zip3list3, by="id.combo") %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01")) %>% filter(flu.week)
# 
# #### plot epidemic time series for zip3-season combinations with short durations ####################################
# indexes3 <- seq(1, max(data_plot3 %>% select(for.plot)), by=ct)
# 
# # ILI plots by season
# dir.create(sprintf('../under5', code, code2), showWarnings=FALSE)
# setwd(sprintf('../under5', code, code2))
# for(i in indexes3){
#   dummyplots <- ggplot(data_plot3 %>% filter(for.plot>= i & for.plot < i+ct) %>% mutate(is.epiweek = ifelse(epi.week, 0, NA)), aes(x=Thu.week, y=ili, group=id.combo)) +
#     theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
#     geom_line(aes(color = in.season)) + scale_color_brewer(palette="Set1") +
#     geom_line(aes(y = is.epiweek), color = 'black') + # appears if epi.week=T
#     geom_line(aes(y = epi.thresh), color = 'grey') +
#     facet_wrap(~id.combo, scales = "free")
#   # grab zip3s in plot for file name
#   ziplabels <- data_plot3 %>% select(id.combo) %>% distinct %>% slice(c(i, i+ct-1)) 
#   ggsave(sprintf("shortEpiDur_seas_%sfits_ILI%s_%s-%s.png", code, code2, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
# }
# 
# # all plots saved 9/16/15 morning


#### filter zip3-combos data ####################################
#### FILTERED LONG DURATIONS (GREATER THAN OR EQUAL TO 20 WEEKS) ####################################
# examine ts of zip-seasons with long epidemic durations
db.dur20.filt <- dbMetrics.g %>% filter(metric=="epi.dur" & burden>=20) %>% mutate(id.combo = paste0(season, zipname)) %>% filter(id.combo %in% combos2$id)

#### subset season-zip3 combinations in db.dur20 only ####################################
fi.dur20.seas <- fullIndic %>% mutate(id.combo = paste0(season, zipname)) %>% filter(id.combo %in% db.dur20.filt$id.combo)
zip3list2 <- fi.dur20.seas %>% select(zipname, id.combo) %>% distinct %>% mutate(for.plot = seq_along(1:nrow(.)))
zip3list2.stat <- left_join(zip3list2, modelfit2, by="zipname") %>% select(-zip3, -p.value, -df, -r.squared)
data_plot2 <- right_join(fi.dur20.seas, zip3list2.stat %>% select(-zipname), by="id.combo") %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01")) %>% filter(flu.week) %>% mutate(id.combo.lab = paste(id.combo, signif(adj.r.squared, digits=2)))

#### plot epidemic time series for zip3-season combinations with long durations ####################################
indexes2 <- seq(1, max(data_plot2 %>% select(for.plot)), by=ct)

# ILI plots by season
dir.create(sprintf('./over20filtered', code, code2), showWarnings=FALSE)
setwd(sprintf('./over20filtered', code, code2))
for(i in indexes2){
  dummyplots <- ggplot(data_plot2 %>% filter(for.plot>= i & for.plot < i+ct) %>% mutate(is.epiweek = ifelse(epi.week, 0, NA)), aes(x=Thu.week, y=ili, group=id.combo.lab)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = in.season)) + scale_color_brewer(palette="Set1") +
    geom_line(aes(y = is.epiweek), color = 'black') + # appears if epi.week=T
    geom_line(aes(y = epi.thresh), color = 'grey') +
    facet_wrap(~id.combo.lab, scales = "free")
  # grab zip3s in plot for file name
  ziplabels <- data_plot2 %>% select(id.combo) %>% distinct %>% slice(c(i, i+ct-1)) 
  ggsave(sprintf("longEpiDur_seas_%sfits_ILI%s_%s-%s.png", code, code2, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
}


#### FILTERED SHORT DURATIONS (LESS THAN OR EQUAL TO 5 WEEKS) ####################################
# examine ts of zip-seasons with short epidemic durations
db.dur5.filt <- dbMetrics.g %>% filter(metric=="epi.dur" & burden<=5) %>% mutate(id.combo = paste0(season, zipname)) %>% filter(id.combo %in% combos2$id)

# subset season-zip3 combinations in db.dur8 only
fi.dur5.seas <- fullIndic %>% filter(season != 1) %>% mutate(id.combo = paste0(season, zipname)) %>% filter(id.combo %in% db.dur5.filt$id.combo)
zip3list3 <- fi.dur5.seas %>% select(zipname, id.combo) %>% distinct %>% mutate(for.plot = seq_along(1:nrow(.)))
zip3list3.stat <- left_join(zip3list3, modelfit2, by="zipname") %>% select(-zip3, -p.value, -df, -r.squared)
data_plot3 <- right_join(fi.dur5.seas, zip3list3.stat %>% select(-zipname), by="id.combo") %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01")) %>% filter(flu.week) %>% mutate(id.combo.lab = paste(id.combo, signif(adj.r.squared, digits=2)))

#### plot epidemic time series for zip3-season combinations with short durations ####################################
indexes3 <- seq(1, max(data_plot3 %>% select(for.plot)), by=ct)

# ILI plots by season
dir.create(sprintf('../under5filtered', code, code2), showWarnings=FALSE)
setwd(sprintf('../under5filtered', code, code2))
for(i in indexes3){
  dummyplots <- ggplot(data_plot3 %>% filter(for.plot>= i & for.plot < i+ct) %>% mutate(is.epiweek = ifelse(epi.week, 0, NA)), aes(x=Thu.week, y=ili, group=id.combo)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = in.season)) + scale_color_brewer(palette="Set1") +
    geom_line(aes(y = is.epiweek), color = 'black') + # appears if epi.week=T
    geom_line(aes(y = epi.thresh), color = 'grey') +
    facet_wrap(~id.combo.lab, scales = "free")
  # grab zip3s in plot for file name
  ziplabels <- data_plot3 %>% select(id.combo) %>% distinct %>% slice(c(i, i+ct-1)) 
  ggsave(sprintf("shortEpiDur_seas_%sfits_ILI%s_%s-%s.png", code, code2, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
}
