
## Name: Elizabeth Lee
## Date: 5/21/15
## Function: write excess ILI by zip3; lm(IR ~ t + t^2 + cos(2*pi*t/52) + sin(2*pi*t/52))
## Filenames: 
## Data Source: 
## Notes: iliProp_df reports ILI as a proportion of all visits.
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")
rm(list=ls())
####################################
# header
require(dplyr)
require(ggplot2)
require(broom)
require(tidyr)
setwd(dirname(sys.frame(1)$ofile))

####################################
# import data
setwd('../Py_export')
# ili_df <- read.csv('iliByallZip_allWeekly_totServ_totAge.csv', header=T, colClasses=c("week"="Date"))
# viz_df <- read.csv('vizByallZip_allWeekly_totServ_totAge.csv', header=T, colClasses=c("week"="Date"))
iliProp_df <- read.csv('iliPropByallZip_allWeekly_totServ_totAge.csv', header=T, colClasses=c("week"="Date"))
pop_df <- read.csv('popByallZip_allYearly_totAge.csv', header=T, colClasses=c("year"="character"))

####################################
# data cleaning
# date formatting in each dataset
pop_df <- pop_df %>% mutate(year = substr(pop_df$year, 1, 4))
iliProp_df <- iliProp_df %>% mutate(Thu.week = as.Date(week+4)) %>% 
  mutate(year = substr(as.character(Thu.week), 1, 4)) %>% 
  mutate(month = as.numeric(substr(as.character(Thu.week), 6, 7))) %>%   
  mutate(flu.week = (month >= 11 | month <= 4)) %>% mutate(t = seq_along(1:nrow(.)))

# generate incidence ratio (ili/viz*pop/100000)
# gather ili and pop data for join
iliProp_gather_df <- gather(iliProp_df, zip3, ili, X2:X999, convert=FALSE)
pop_gather_df <- gather(pop_df, zip3, pop, X2:X999, convert=FALSE)
iliProp_pop_gather_df <- left_join(iliProp_gather_df, pop_gather_df, by=c("year", "zip3"))
# perform IR calculation 7/27/15: calculation is correct because ili is the ILI proportion of all visits
IR_fullgather_df <- iliProp_pop_gather_df %>% mutate(IR = ili*pop/100000) %>% mutate(week=as.Date(week, origin="1970-01-01")) %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01"))

####################################
# program

# identify zip3s with too little IR data during non-flu periods (less than half of the datapoints)
noIRdata <- IR_fullgather_df %>% filter(!flu.week) %>% 
  filter(Thu.week < as.Date('2009-05-01')) %>% group_by(zip3) %>% 
  summarise(num.NA = sum(is.na(IR))) %>% mutate(incl.lm = num.NA <105)

# join datasets to incorporate indicators for including zip3 into lm
IR_df <- right_join(IR_fullgather_df, (noIRdata %>% select(-num.NA)), by='zip3') 

# create new data for augment
newbasedata <- IR_df %>% select(Thu.week, t) %>% unique %>% filter(Thu.week < as.Date('2009-05-01'))

# perform regressions
allMods <- IR_df %>% filter(!flu.week) %>% filter(Thu.week < as.Date('2009-05-01')) %>% filter(incl.lm) %>% group_by(zip3) %>%
  do(fitZip3 = lm(IR ~ t + I(t^2) + cos(2*pi*t/52) + sin(2*pi*t/52), data = ., na.action=na.exclude))
allMods_tidy <- tidy(allMods, fitZip3)
allMods_aug <- augment(allMods, fitZip3, newdata= newbasedata)

# after augment - join IR data to fits
allMods_fit_IR <- right_join((allMods_aug %>% select(-t)), (IR_df %>% filter(Thu.week < as.Date('2009-05-01'))), by=c('Thu.week', 'zip3')) %>% mutate(week=as.Date(week, origin="1970-01-01")) %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01"))

# write to file (5/31/15)
setwd('../R_export')
# write fitted and original IR data 
write.csv(allMods_fit_IR, file='periodicReg_t2_allZip3Mods.csv', row.names=FALSE)
# write tidy coefficient dataset
write.csv(allMods_tidy, file='tidyCoef_periodicReg_t2_allZip3Mods.csv', row.names=FALSE)
