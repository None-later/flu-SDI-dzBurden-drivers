
## Name: Elizabeth Lee
## Date: 10/29/15 
## Function: process ilicnDt data for total ILI, removing locations with messy data
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
require(tidyr)
require(readr)
require(dplyr)

fparams <- list(metric = 'ilicnDt', span = 0.5, degree = 2)

#### read data ################################
setwd('../../R_export/dataSnapshot_NIHbigdata')

db.data <- read_csv(do.call(sprintf, c('dbMetrics_periodicReg_%s_Octfit_span%s_degree%s_analyzeDB.csv', fparams)), col_types = list('zip3' = col_character()))
indic.data <- read_csv(do.call(sprintf, c('fullIndicAll_periodicReg_%s_Octfit_span%s_degree%s_analyzeDB.csv', fparams)), col_types = list('zip3' = col_character()))
summ.data <- read_csv(do.call(sprintf, c('summaryStats_periodicReg_allZip3Mods_%s_Octfit_span%s_degree%s.csv', fparams)), col_types = list('zip3' = col_character()))

#### list of zip3s to include in figures ################################
zipIncl.summ <- summ.data %>% filter(sigma < 0.9) %>% select(zip3)
zipExcl.indic <- indic.data %>% filter(incl.lm & .fittedLoess < 1) %>% select(zip3) %>% unique
zipIncl <- zipIncl.summ %>% filter(!(zip3 %in% zipExcl.indic$zip3))

#### filter db data ################################
db <- db.data %>% filter(zip3 %in% zipIncl$zip3 & season > 1 & season < 10)
indic <- indic.data %>% filter(zip3 %in% zipIncl$zip3 & Thu.week >= as.Date('2001-10-01') & Thu.week <= as.Date('2009-09-30'))

#### create geo data ################################
abbr.data <- read.csv("state_abbreviations_FIPS.csv", header=T, colClasses='character')
coord.data <- read_csv("Coord3digits.csv", col_type = list("zip3" = col_character())) %>%
  mutate(zip3 = substr.Right(paste0("00", zip3), 3)) %>% 
  filter(zip3 %in% zipIncl$zip3) %>%
  select(zip3, STATE, pop, lat, long)
pop_agg5 <- left_join(coord.data, abbr.data, by = c("STATE" = "Abbreviation")) %>% 
  rename(state = STATE) %>%
  mutate(hhs = 0)

pop_agg5[which(pop_agg5$state == 'CT' | pop_agg5$state == 'ME' | pop_agg5$state == 'MA' | pop_agg5$state == 'NH' | pop_agg5$state == 'RI' | pop_agg5$state == 'VT'),]$hhs <- 1
pop_agg5[which(pop_agg5$state == 'NY' | pop_agg5$state == 'NJ' | pop_agg5$state == 'PR' | pop_agg5$state == 'VI'),]$hhs <- 2
pop_agg5[which(pop_agg5$state == 'DE' | pop_agg5$state == 'DC' | pop_agg5$state == 'MD' | pop_agg5$state == 'PA' | pop_agg5$state == 'VA' | pop_agg5$state == 'WV'),]$hhs <- 3
pop_agg5[which(pop_agg5$state == 'AL' | pop_agg5$state == 'FL' | pop_agg5$state == 'GA' | pop_agg5$state == 'KY' | pop_agg5$state == 'MS' | pop_agg5$state == 'NC' | pop_agg5$state == 'SC' | pop_agg5$state == 'TN'),]$hhs <- 4
pop_agg5[which(pop_agg5$state == 'IL' | pop_agg5$state == 'IN' | pop_agg5$state == 'MI' | pop_agg5$state == 'MN' | pop_agg5$state == 'OH' | pop_agg5$state == 'WI'),]$hhs <- 5
pop_agg5[which(pop_agg5$state == 'AR' | pop_agg5$state == 'LA' | pop_agg5$state == 'NM' | pop_agg5$state == 'OK' | pop_agg5$state == 'TX'),]$hhs <- 6
pop_agg5[which(pop_agg5$state == 'IA' | pop_agg5$state == 'KS' | pop_agg5$state == 'MO' | pop_agg5$state == 'NE'),]$hhs <- 7
pop_agg5[which(pop_agg5$state == 'CO' | pop_agg5$state == 'MT' | pop_agg5$state == 'ND' | pop_agg5$state == 'SD' | pop_agg5$state == 'UT' | pop_agg5$state == 'WY'),]$hhs <- 8
pop_agg5[which(pop_agg5$state == 'AZ' | pop_agg5$state == 'CA' | pop_agg5$state == 'HI' | pop_agg5$state == 'NV' | pop_agg5$state == 'AS' | pop_agg5$state == 'MP' | pop_agg5$state == 'FM' | pop_agg5$state == 'GU' | pop_agg5$state == 'MH' | pop_agg5$state == 'PW'),]$hhs <- 9
pop_agg5[which(pop_agg5$state == 'AK' | pop_agg5$state == 'ID' | pop_agg5$state == 'OR' | pop_agg5$state == 'WA'),]$hhs <- 10

geo.data <- pop_agg5 %>%
  rename(abbr = state) %>%
  rename(fullstate = State) %>%
  rename(hhsregion = hhs)

#### write data ################################
write_csv(db, do.call(sprintf, c('dbMetrics_visuals_%s_span%s_degree%s.csv', fparams)))
write_csv(indic, do.call(sprintf, c('fullIndic_visuals_%s_span%s_degree%s.csv', fparams)))
write_csv(geo.data, do.call(sprintf, c("zip3StIncl_coords_%s_span%s_degree%s.csv", fparams)))
# saved 10/29/15



