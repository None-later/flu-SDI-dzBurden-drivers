## Name: Elizabeth Lee
## Date: 9/2/15
## Function: examine zip3-season time series with short and long peak timings; Does the time series appear to follow an expected epidemic pattern?
## Results: There's no population-related explanation for long or short times to epidemic
# Legend: black line at 0 means epi.week=T, other color designations refer to in.season variable

### disease burden metrics: number of weeks to peak after epidemic start
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
# LONG DURATIONS (GREATER THAN OR EQUAL TO 10 WEEKS)
####################################
# examine ts of zip-season combos with long time to peak durations
db.dur10 <- dbMetrics.g %>% filter(metric=="wks.to.peak" & burden>=10) %>% mutate(id.combo = paste0(season, zipname))

fi.dur10.seas <- fullIndic %>% filter(season != 1) %>% mutate(id.combo = paste0(season, zipname)) %>% filter(id.combo %in% db.dur10$id.combo)
zip3list2 <- fi.dur10.seas %>% select(id.combo) %>% distinct %>% mutate(for.plot = seq_along(1:nrow(.)))
data_plot2 <- right_join(fi.dur10.seas, zip3list2, by="id.combo") %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01")) %>% filter(flu.week)

####################################
# plot epidemic time series for zip3-season combinations with long durations
indexes2 <- seq(1, max(data_plot2 %>% select(for.plot)), by=ct)

# IR plots by season
setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_pktimeOutliers_%sIR/over10/IR', code))
for(i in indexes2){
  dummyplots <- ggplot(data_plot2 %>% filter(for.plot>= i & for.plot < i+ct) %>% mutate(is.epiweek = ifelse(epi.week, 0, NA)), aes(x=Thu.week, y=IR, group=id.combo)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = in.season)) + scale_color_brewer(palette="Set1") +
    geom_line(aes(y = is.epiweek), color = 'black') + # appears if epi.week=T
    facet_wrap(~id.combo, scales = "free")
  # grab zip3s in plot for file name
  ziplabels <- data_plot2 %>% select(id.combo) %>% distinct %>% slice(c(i, i+ct-1)) 
  ggsave(sprintf("longPkTime_seas_%sfits_IR_%s-%s.png", code, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
}

# ili proportion by season
setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_pktimeOutliers_%sIR/over10/iliProp', code))
for(i in indexes2){
  dummyplots <- ggplot(data_plot2 %>% filter(for.plot>= i & for.plot < i+ct) %>% mutate(is.epiweek = ifelse(epi.week, 0, NA)), aes(x=Thu.week, y=ili, group=id.combo)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = in.season)) + scale_color_brewer(palette="Set1") +
    geom_line(aes(y = is.epiweek), color = 'black') + # appears if epi.week=T
    facet_wrap(~id.combo, scales = "free")
  # grab zip3s in plot for file name
  ziplabels <- data_plot2 %>% select(id.combo) %>% distinct %>% slice(c(i, i+ct-1)) 
  ggsave(sprintf("longPkTime_seas_%sfits_iliProp_%s-%s.png", code, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
}

####################################
# SHORT DURATIONS (LESS THAN OR EQUAL TO 4 WEEKS)
####################################
# examine ts of zip-seasons with short epidemic durations
db.dur4 <- dbMetrics.g %>% filter(metric=="wks.to.peak" & burden<=4) %>% mutate(id.combo = paste0(season, zipname))

# subset season-zip3 combinations in db.dur8 only
fi.dur4.seas <- fullIndic %>% filter(season != 1) %>% mutate(id.combo = paste0(season, zipname)) %>% filter(id.combo %in% db.dur4$id.combo)
zip3list3 <- fi.dur4.seas %>% select(id.combo) %>% distinct %>% mutate(for.plot = seq_along(1:nrow(.)))
data_plot3 <- right_join(fi.dur4.seas, zip3list3, by="id.combo") %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01")) %>% filter(flu.week)

####################################
# plot epidemic time series for zip3-season combinations with short durations
indexes3 <- seq(1, max(data_plot3 %>% select(for.plot)), by=ct)

# IR plots by season
setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_pktimeOutliers_%sIR/under4/IR', code))
for(i in indexes3){
  dummyplots <- ggplot(data_plot3 %>% filter(for.plot>= i & for.plot < i+ct) %>% mutate(is.epiweek = ifelse(epi.week, 0, NA)), aes(x=Thu.week, y=IR, group=id.combo)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = in.season)) + scale_color_brewer(palette="Set1") +
    geom_line(aes(y = is.epiweek), color = 'black') + # appears if epi.week=T
    facet_wrap(~id.combo, scales = "free")
  # grab zip3s in plot for file name
  ziplabels <- data_plot3 %>% select(id.combo) %>% distinct %>% slice(c(i, i+ct-1)) 
  ggsave(sprintf("shortPkTime_seas_%sfits_IR_%s-%s.png", code, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
}

# ili proportion by season
setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_pktimeOutliers_%sIR/under4/iliProp', code))
for(i in indexes3){
  dummyplots <- ggplot(data_plot3 %>% filter(for.plot>= i & for.plot < i+ct) %>% mutate(is.epiweek = ifelse(epi.week, 0, NA)), aes(x=Thu.week, y=ili, group=id.combo)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = in.season)) + scale_color_brewer(palette="Set1") +
    geom_line(aes(y = is.epiweek), color = 'black') + # appears if epi.week=T    
    facet_wrap(~id.combo, scales = "free")
  # grab zip3s in plot for file name
  ziplabels <- data_plot3 %>% select(id.combo) %>% distinct %>% slice(c(i, i+ct-1)) 
  ggsave(sprintf("shortPkTime_seas_%sfits_iliProp_%s-%s.png", code, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
}

####################################
# 9/2/15
# population size of zip3s with long/short peak times vs. all popsizes
pop.dur10 <- fi.dur10.seas %>% select(season, zipname, pop) %>% distinct
# View(pop.dur10 %>% group_by(zipname) %>% summarise(counts = length(zipname)/2)) # how many times does a zip3 appear?
pop.fi <- fullIndic %>% filter(year==2006) %>% select(zipname, pop) %>% distinct
pop.dur4 <- fi.dur4.seas %>% select(season, zipname, pop) %>% distinct
# View(pop.dur4 %>% group_by(zipname) %>% summarise(counts = length(zipname)/2)) 

# how many long duration zip3s are in the short duration zip3 list?
sum(unique(pop.dur10$zipname) %in% unique(pop.dur4$zipname)) # 
pop.durboth <- tbl_df(data.frame(zipname = unique(pop.dur4$zipname), in.both = unique(pop.dur4$zipname) %in% unique(pop.dur10$zipname))) %>% filter(in.both) %>% arrange(zipname)
pop.durboth2 <- pop.fi %>% filter(zipname %in% pop.durboth$zipname) 

setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_pktimeOutliers_%sIR/', code))
png(filename='popDistComparison.png', width=w, height=h+3, units='in', res=1000)
par(mfrow=c(2,2))

hist(pop.dur10$pop, breaks=50, main='pktime>= 10 wks', xlab='population', xlim=c(0,3E6))
abline(v=quantile(pop.dur10$pop)[2:4], lwd=1, col='red')
hist(pop.fi$pop, breaks=50, main='all 2006 zip3s', xlab='population', xlim=c(0,3E6))
abline(v=quantile(pop.fi$pop)[2:4], lwd=1, col='red')
hist(pop.dur4$pop, breaks=50, main='pktime<= 4 wks', xlab='population', xlim=c(0,3E6))
abline(v=quantile(pop.dur4$pop, na.rm=T)[2:4], lwd=1, col='red')
hist(pop.durboth2$pop, breaks=50, main='2006 zip3s in short & long', xlab='population', xlim=c(0,3E6))
abline(v=quantile(pop.durboth2$pop)[2:4], lwd=1, col='red')
dev.off()

# all plots saved 9/2/15 afternoon
