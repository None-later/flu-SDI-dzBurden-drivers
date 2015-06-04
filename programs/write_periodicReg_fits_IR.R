
## Name: Elizabeth Lee
## Date: 5/21/15
## Function: write excess ILI by zip3; lm(IR ~ t + cos(2*pi*t/52) + sin(2*pi*t/52))
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

####################################
# header
setwd('/home/elee/R/source_functions')
source("dfsumm.R")
require(dplyr)
require(ggplot2)
require(broom)
require(tidyr)

####################################
# import data
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/Py_export')
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
# perform IR calculation
IR_fullgather_df <- iliProp_pop_gather_df %>% mutate(IR = ili*pop/100000)

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
  do(fitZip3 = lm(IR ~ t + cos(2*pi*t/52) + sin(2*pi*t/52), data = ., na.action=na.exclude))
allMods_tidy <- tidy(allMods, fitZip3)
allMods_aug <- augment(allMods, fitZip3, newdata= newbasedata)

# after augment - join IR data to fits
allMods_fit_IR <- right_join((allMods_aug %>% select(-t)), (IR_df %>% filter(Thu.week < as.Date('2009-05-01'))), by=c('Thu.week', 'zip3'))

# write to file
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')
# write fitted and original IR data (5/31/15)
write.csv(allMods_fit_IR, file='periodicReg_allZip3Mods.csv', row.names=FALSE)
# write tidy coefficient dataset
write.csv(allMods_tidy, file='tidyCoef_periodicReg_allZip3Mods.csv', row.names=FALSE)


##############################################################
# ####################################
# # spare code
# # generate IR dataframe that removes the pandemic year and excludes flu season (from Nov to Apr)
# IR_no09_df <- IR_df[-which(IR_df$week>=as.Date('2009-05-01')),]
# IR_forlm_df <- IR_no09_df
# flumonths <- c(11, 12, 1, 2, 3, 4)
# IR_forlm_df[which(IR_forlm_df$month %in% flumonths), 4:938] <- NA
# 
# fit0 <- periodicRegression(IR_forlm_df[,i])
# plot(indexes, IR_forlm_df[,i], xlab='week', ylab='incid ratio', ylim=c(0, 0.05))
# # lines(fit0$fit, type='l')
# # lines(as.numeric(names(fit0$fit)),fit0$fit, type='l')
# lines(predict.lm(fit0))
# 
# fit1 <- periodicRegression(IR_no09_df[,i])
# plot(indexes, IR_no09_df[,i], xlab='week', ylab='incid ratio', ylim=c(0, 0.05))
# lines(fit1$fit, type='l')
# 
# test <- IR_final_df %>% do(augment(mod, newdata=(IR_forlm_df %>% filter(flu.week) %>% select(t))))
# 
# # grab fitted values for all t predictors
# mod <- lm(X200 ~ t + costerm + sinterm, data=(IR_forlm_df %>%
#                                                 filter(!flu.week) %>%
#                                                 filter(week < as.Date('2009-05-01'))), na.action=na.exclude) %>%
#   augment(., newdata=IR_forlm_df) %>% select(.fitted)

# ####################################
# # too large for R to store in memory
# #test augment by group (full)
# allMods_aug <- augment(allMods, fitZip3, newdata=(IR_df %>% filter(incl.lm))) # cannot allocate vector of size 2.3 Gb
# #test augment for only fluweek data
# allMods_aug_flu <- augment(allMods, fitZip3, newdata=(IR_df %>% filter(incl.lm) %>% filter(week < as.Date('2009-05-01')) %>% filter(flu.week))) # cannot allocate vector of size 1.0 Gb

# ####################################
# # augment test -- smaller data
# allMods.test <- IR_df %>% filter(zip3 == 'X200' | zip3 == 'X945') %>% filter(!flu.week) %>% filter(week < as.Date('2009-05-01')) %>% filter(incl.lm) %>% group_by(zip3) %>%
#   do(fitZip3 = lm(IR ~ t + cos(2*pi*t/52) + sin(2*pi*t/52), data = ., na.action=na.exclude))
# 
# allMods.test_tidy <- tidy(allMods.test, fitZip3)
# 
# newdata <- IR_df %>% select(week, t) %>% unique %>% filter(week < as.Date('2009-05-01'))
# 
# allMods.test_aug <- augment(allMods.test, fitZip3, newdata=newdata) 
# allMods.test_aug_NF <- augment(allMods.test, fitZip3) # only non-flu weeks

# # tests #
# allMods.test <- IR_df %>% filter(zip3 == 'X200' | zip3 == 'X945') %>% filter(!flu.week) %>% filter(week < as.Date('2009-05-01')) %>% filter(incl.lm) %>% group_by(zip3) %>%
#   do(fitZip3 = lm(IR ~ t + costerm + sinterm, data = ., na.action=na.exclude))

# testing <- augment(allMods.test, fitZip3, newdata=IR_df %>% filter(zip3 == 'X200' | zip3 == 'X945') %>% filter(week < as.Date('2009-05-01')))

# current.testing <- 
#   augment(allMods.test, fitZip3, newdata=(IR_df %>% filter(zip3 == 'X200' | zip3 == 'X945') %>% filter(week < as.Date('2009-05-01')) %>% filter(incl.lm) %>% group_by(zip3))) # all weeks

# ####################################
# # data cleaning
# # generate incidence ratio (ili/viz*pop/100000)
# ycounts <- count(iliProp_df, year) # equivalent: ycounts <- iliProp_df %>% group_by(year) %>% summarise(counts = n())
# pop_fulldf <- pop_df[rep(1:nrow(pop_df), times=ycounts$n),]
# 
# IRcalc_df <- iliProp_df[,2:ncol(pop_fulldf)] * pop_fulldf[,2:ncol(pop_fulldf)] / 100000
# dates_df <- data.frame(week=iliProp_df$week, year=iliProp_df$year, month=as.numeric(iliProp_df$month))
# IR_df <- bind_cols(dates_df, IRcalc_df)

# ####################################
# # test plots
# # I think week isn't right since it is a character variable
# p1 <- ggplot(IR_all_df, aes(x=t, y=X200)) + 
#   geom_point() + 
#   stat_smooth(data=(IR_all_df %>% filter(!flu.week) %>% filter(week<as.Date('2009-05-01'))), method="lm", formula = y ~ x + sin(2*pi*x/52) + cos(2*pi*x/52)) 
# print(p1)

# # I think the 2*pi only terms are best
# p2 <- ggplot(IR_all_df, aes(x=t, y=X945)) + 
#   geom_point() + 
#   stat_smooth(data=(IR_all_df %>% filter(!flu.week) %>% filter(week<as.Date('2009-05-01'))), method="lm", formula = y ~ x + sin(2*pi*x/52) + cos(2*pi*x/52) + sin(4*pi*x/52) + cos(4*pi*x/52)) 
# print(p2)
