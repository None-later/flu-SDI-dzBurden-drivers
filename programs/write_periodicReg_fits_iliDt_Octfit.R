
## Name: Elizabeth Lee
## Date: 10/20/15
## Function: perform periodic regression on the detrended relative ili (iliDt)
## iliDt metric defined: after using loess smoother to generate fitted values during summer weeks, we divided observed ili by smoothed loess fits for entire time series
## used write_loess_fits_ILI.R to generate iliDt data
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

#### header ################################
code <- ''
code2 <- '_Octfit'
span.var <- 0.4 # 0.4, 0.6
degree.var <- 2
code.str <- sprintf('_span%s_degree%s', span.var, degree.var)

#### import data ####################################
setwd('../R_export')
ILI_full_df <- read_csv(file=sprintf('loess%s_allZip3Mods_ILI.csv', code.str), col_types=list(ili=col_double(), .fitted=col_double(), .se.fit=col_double(), ili.dt=col_double(), zipname=col_character())) %>% rename(.fittedLoess = .fitted, .se.fitLoess = .se.fit)

#### process data for periodic regression ####################################
# create new data for augment
newbasedata <- ILI_full_df %>% select(Thu.week, t) %>% unique %>% filter(Thu.week < as.Date('2009-05-01'))

# perform periodic regression
allMods <- ILI_full_df %>% filter(fit.week) %>% filter(Thu.week < as.Date('2009-05-01')) %>% 
  filter(incl.lm) %>% group_by(zip3) %>%
  do(fitZip3 = lm(ili.dt ~ t + cos(2*pi*t/52.18) + sin(2*pi*t/52.18), data = ., na.action=na.exclude))
allMods_tidy <- tidy(allMods, fitZip3)
allMods_aug <- augment(allMods, fitZip3, newdata= newbasedata)
allMods_glance <- glance(allMods, fitZip3)

# after augment - join ILI data to fits
allMods_fit_ILI <- right_join((allMods_aug %>% select(-t)), (ILI_full_df %>% filter(Thu.week < as.Date('2009-05-01'))), by=c('Thu.week', 'zip3')) %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01")) %>% mutate(week=as.Date(week, origin="1970-01-01"))


#### write data to file ####################################
setwd('../R_export')
# write fitted and original IR data 
write.csv(allMods_fit_ILI, file=sprintf('periodicReg_%sallZip3Mods_iliDt%s%s.csv', code, code2, code.str), row.names=FALSE)
# write tidy coefficient dataset
write.csv(allMods_tidy, file=sprintf('tidyCoef_periodicReg_%sallZip3Mods_iliDt%s%s.csv', code, code2, code.str), row.names=FALSE)
# write summary statistics for all models
write.csv(allMods_glance, file=sprintf('summaryStats_periodicReg_%sallZip3Mods_iliDt%s%s.csv', code, code2, code.str), row.names=FALSE)
# write 10/20/15
