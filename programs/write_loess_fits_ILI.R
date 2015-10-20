
## Name: Elizabeth Lee
## Date: 10/19/15
## Function: smooth ILI data using loess smoother during summer weeks, divide observed ili by smoothed loess fits for entire time series. Pass the processed ILI metric to write_loess_PeriodicReg_....R
## timetrend <- loess(ILI ~ t) during summer weeks 
## lm(ILI/timetrend ~ t + cos(2*pi*t/52.18) + sin(2*pi*t/52.18))

## Filenames: 
## Data Source: 
## Notes: code2 = 'Octfit' --> October to April is flu period, but we fit the seasonal regression from April to October (i.e., expanded definition of summer) in order to improve phase of regression fits
## 52.18 weeks per year in the regression model
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

rm(list = ls())
#### header ####################################
require(dplyr)
require(ggplot2)
require(broom)
require(tidyr)
setwd(dirname(sys.frame(1)$ofile))

#### import data ####################################
setwd('../Py_export')
ili_df <- read.csv('iliByallZip_allWeekly_totServ_totAge.csv', header=T, colClasses=c("week"="Date"))
# pop_df <- read.csv('popByallZip_allYearly_totAge.csv', header=T, colClasses=c("year"="character"))

#### set these! ################################
span.var <- 0.6 # 0.4, 0.6
degree.var <- 2
code.str <- sprintf('_span%s_degree%s', span.var, degree.var)

#### data cleaning ####################################
# date formatting in each dataset
ili_df2 <- ili_df %>% mutate(Thu.week = as.Date(week+4)) %>% 
  mutate(year = substr(as.character(Thu.week), 1, 4)) %>% 
  mutate(month = as.numeric(substr(as.character(Thu.week), 6, 7))) %>%   
  mutate(flu.week = (month >= 10 | month <= 4)) %>% 
  mutate(t = seq_along(1:nrow(.))) %>% 
  mutate(fit.week = (month >= 4 & month <= 10))

# gather ili data
ili_gather_df <- gather(ili_df2, zip3, ili, X2:X999, convert=FALSE)

#### process data for loess smoothing ####################################
# identify zip3s with too little ILI data during fitted week periods (Apr to Oct >= 50 NAs)
noILIdata <- ili_gather_df %>% filter(fit.week) %>% 
  filter(Thu.week < as.Date('2009-05-01')) %>% group_by(zip3) %>% 
  summarise(num.NA = sum(is.na(ili))) %>% mutate(incl.lm = num.NA < 50)
# # 10/19/15 distribution of number of NAs during fitting period for all zip3s
# hist(noILIdata$num.NA, breaks = 50)

# join datasets to incorporate indicators for including zip3 into loess
ILI_full_df <- right_join(ili_gather_df, (noILIdata %>% select(-num.NA)), by='zip3') %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01"))

# create new data for augment
newbasedata <- ILI_full_df %>% select(Thu.week, t) %>% unique %>% filter(Thu.week < as.Date('2009-05-01'))

#### perform loess regression ####################################
allLoessMods <- ILI_full_df %>% filter(!flu.week) %>% filter(Thu.week < as.Date('2009-05-01')) %>% 
  filter(incl.lm) %>% group_by(zip3) %>%
  do(fitZip3 = loess(ili ~ t, span = span.var, degree = degree.var, data = ., na.action=na.exclude))
allLoessMods_aug <- augment(allLoessMods, fitZip3, newdata= newbasedata)
# 10/20/15 tidy and glance aren't implemented for loess models
# allLoessMods_tidy <- tidy(allLoessMods, fitZip3)
# allLoessMods_glance <- glance(allLoessMods, fitZip3)

# after augment - join ILI data to fits
allLoessMods_fit_ILI <- right_join((allLoessMods_aug %>% select(-t)), (ILI_full_df %>% filter(Thu.week < as.Date('2009-05-01'))), by=c('Thu.week', 'zip3')) %>% mutate(week=as.Date(week, origin="1970-01-01")) %>% ungroup %>% mutate(ili.dt = ifelse(.fitted <= 1, ili, ili/.fitted)) %>% mutate(zipname = substr.Right(gsub("X", "00", zip3), 3)) 

#### write data to file ####################################
setwd('../R_export')
# write fitted and original loess smoothed ILI data 
write.csv(allLoessMods_fit_ILI, file=sprintf('loess%s_allZip3Mods_ILI.csv', code.str), row.names=FALSE)
# saved 10/20/15


