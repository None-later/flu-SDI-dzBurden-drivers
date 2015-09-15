
## Name: Elizabeth Lee
## Date: 9/15/15
## Function: write excess ILI by zip3 using raw cases, not IR; lm(ILI ~ t + t^2 + cos(2*pi*t/52) + sin(2*pi*t/52))
## Filenames: 
## Data Source: 
## Notes: Oct= October to April is flu period
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

####################################
# header
setwd('~/Dropbox/code')
source("dfsumm.R")
require(dplyr)
require(ggplot2)
require(broom)
require(tidyr)
setwd(dirname(sys.frame(1)$ofile))
####################################
# import data
setwd('../Py_export')
ili_df <- read.csv('iliByallZip_allWeekly_totServ_totAge.csv', header=T, colClasses=c("week"="Date"))
# pop_df <- read.csv('popByallZip_allYearly_totAge.csv', header=T, colClasses=c("year"="character"))
####################################
code2 = '_Oct'

####################################
# data cleaning
# date formatting in each dataset
ili_df2 <- ili_df %>% mutate(Thu.week = as.Date(week+4)) %>% 
  mutate(year = substr(as.character(Thu.week), 1, 4)) %>% 
  mutate(month = as.numeric(substr(as.character(Thu.week), 6, 7))) %>%   
  mutate(flu.week = (month >= 10 | month <= 4)) %>% mutate(t = seq_along(1:nrow(.)))

# gather ili data
ili_gather_df <- gather(ili_df2, zip3, ili, X2:X999, convert=FALSE)

####################################
# program
# identify zip3s with too little ILI data during non-flu periods (less than half of the datapoints)
noILIdata <- ili_gather_df %>% filter(!flu.week) %>% 
  filter(Thu.week < as.Date('2009-05-01')) %>% group_by(zip3) %>% 
  summarise(num.NA = sum(is.na(ili))) %>% mutate(incl.lm = num.NA <105)

# join datasets to incorporate indicators for including zip3 into lm
ILI_full_df <- right_join(ili_gather_df, (noILIdata %>% select(-num.NA)), by='zip3')

# create new data for augment
newbasedata <- ILI_full_df %>% select(Thu.week, t) %>% unique %>% filter(Thu.week < as.Date('2009-05-01'))

# perform regressions
allMods <- ILI_full_df %>% filter(!flu.week) %>% filter(Thu.week < as.Date('2009-05-01')) %>% filter(incl.lm) %>% group_by(zip3) %>%
  do(fitZip3 = lm(ili ~ t + I(t^2) + cos(2*pi*t/52) + sin(2*pi*t/52), data = ., na.action=na.exclude))
allMods_tidy <- tidy(allMods, fitZip3)
allMods_aug <- augment(allMods, fitZip3, newdata= newbasedata)
allMods_glance <- glance(allMods, fitZip3)

# after augment - join ILI data to fits
allMods_fit_ILI <- right_join((allMods_aug %>% select(-t)), (ILI_full_df %>% filter(Thu.week < as.Date('2009-05-01'))), by=c('Thu.week', 'zip3'))

# write to file (9/14/15)
setwd('../R_export')
# # write fitted and original IR data 
# write.csv(allMods_fit_ILI, file=sprintf('periodicReg_t2_allZip3Mods_ILI%s.csv', code2), row.names=FALSE)
# # write tidy coefficient dataset
# write.csv(allMods_tidy, file=sprintf('tidyCoef_periodicReg_t2_allZip3Mods_ILI%s.csv', code2), row.names=FALSE)
# # write summary statistics for all models
# write.csv(allMods_glance, file=sprintf('summaryStats_periodicReg_t2_allZip3Mods_ILI%s.csv', code2), row.names=FALSE)