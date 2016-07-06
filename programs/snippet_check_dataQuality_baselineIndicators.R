# Elizabeth Lee
# 7/5/16
# Check data quality of baseline ILI modeling, which is used to create disease burden indicators. For instance, why do some zip3s have the conditions: has.epi = T & y = 0?

#### header ####################################
require(dplyr)
require(readr)
require(tidyr)
require(ggplot2)

#### header ####################################
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')

# periodicReg processing  ####################################
pdat <- read_csv('periodicReg_allZip3Mods_ilinDt_Octfit_span0.4_degree2.csv', col_types=list(zip3 = col_character(), ili = col_integer(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double()))

View(pdat %>% filter(!incl.lm & !is.na(ili))) 
View(pdat %>% filter(!incl.lm & !is.na(ili)) %>% group_by(zip3) %>% count)
# 60,000+ weekly entries with ili data have been excluded from analysis
# this includes data from 281 unique zip3s, although the number of seasons for each zip3 varies

View(pdat %>% filter(!incl.lm & !is.na(ili) & is.na(pop)))
View(pdat %>% filter(!incl.lm & !is.na(ili) & is.na(pop)) %>% group_by(zip3) %>% count)
# 2,185 of those weekly entries with ili data were excluded because there was no population data
# this includes data from 52 unique zip3s, although again, the number of seasons for each zip3 varies

View(pdat %>% filter(!incl.lm & is.na(pop)))
View(pdat %>% filter(!incl.lm & is.na(pop)) %>% group_by(zip3) %>% count)
# 23,000+ weekly entries did not have population data
# this includes data from 73 unique zip3s, where zip3s were missing pop data for almost all years or for only one year

# fullIndic processing ####################################
dat <- read_csv('fullIndicAll_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB.csv', col_types = "ccd_d____l_lddldld_idllll")

View(dat %>% filter(has.epi & ilin.dt == 0)) 
View(dat %>% filter(has.epi & epi.week & ilin.dt == 0))
# zip3 may have an epidemic but there are other weeks in that season where ilin.dt = 0 or ili < epi.thresh
# there are no weeks where ilin.dt = 0 in an epi.week

# dbMetrics processing ####################################
metdat <- read_csv('dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDBdist.csv', col_types = "icllcdcidd") %>% filter(season != 1)
metdat0 <- metdat %>% filter(has.epi & burden == 0)

metdat0 %>% group_by(metric) %>% count
View(metdat0 %>% group_by(zip3) %>% count)
# has.epi & ilin.dt == 0 are all with the same set of zip3s
# some zip3s have 5, others have 10 metrics where has.epi & ilin.dt == 0 -- how?

View(metdat %>% filter(zip3 == '023') %>% arrange(season))
# when some zip3s have 5 and other have 10 counts, it means they had one or two seasons (respectively) where has.epi = T & incl.analysis = F & burden = 0 

View(metdat %>% filter(!has.epi | !incl.analysis))
dim(metdat %>% filter(!has.epi | !incl.analysis) %>% filter(!(metric %in% c('wks.to.epi', 'wks.to.peak'))))
# almost 7000 entries met at least one of the following conditions: has.epi = F, incl.analysis = F
# almost 5000 of these entries affected dbMag + epidur metrics

metdat2 <- metdat %>% filter(!(metric %in% c('wks.to.epi', 'wks.to.peak')))
# rows are season-zip3 combinations
dim(metdat2 %>% filter(!has.epi)) # 4290 rows
dim(metdat2 %>% filter(!incl.analysis)) # 3340 rows
dim(metdat2 %>% filter(!has.epi & !incl.analysis)) # 2640 rows are missing both

metdat1 <- metdat %>% filter(has.epi & incl.analysis & burden == 0)
# there are no zip3s with these conditions, so there is no error here






