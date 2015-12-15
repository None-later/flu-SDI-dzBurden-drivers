
## Name: Elizabeth Lee
## Date: 12/15/15
## Function: explore ways to define the flu season with ilin.dt metric
# a) number of weeks during the flu period that exceed the epidemic threshold
# b) number of consecutive weeks during the flu period that exceed the epidemic threshold (as compared to consecutive weeks during the non-flu period)
# Result of analysis (span = 0.4, state-level): If a zip3-season combination has 5+ consecutive weeks above the epidemic threshold during the flu season, it has an epidemic

## Filenames: 
## Data Source: 
## Notes: 12/15/15: added switch between state and zip3
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

rm(list=ls())
#### header ####################################
require(dplyr)
require(ggplot2)
require(readr)
require(ISOweek)
setwd(dirname(sys.frame(1)$ofile))

#### set these! ####################################
code <- ''
code2 <- '_Octfit'

spatial <- list(scale = "state", stringcode = "State", stringabbr = "_st")
# spatial <- list(scale = "zip3", stringcode = "Zip3", stringabbr = "")
span.var <- 0.4 # 0.4
degree.var <- 2
code.str <- sprintf('_span%s_degree%s', span.var, degree.var)


#### data import ####################################
setwd('../R_export')

if (spatial$scale == 'zip3'){
  data <- read_csv(file=sprintf('periodicReg_%sall%sMods_ilinDt%s%s.csv', code, spatial$stringcode, code2, code.str), col_types=list(zip3 = col_character(), ili = col_integer(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double())) %>%
    rename(scale = zip3)
  
} else if (spatial$scale == 'state'){
  data <- read_csv(file=sprintf('periodicReg_%sall%sMods_ilinDt%s%s.csv', code, spatial$stringcode, code2, code.str), col_types=list(state = col_character(), ili = col_integer(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double())) %>%
    rename(scale = state)
}

# 1) add ISO week numbers; 2) add season numbers ; 3) add real zip3 names
data2 <- data %>% mutate(wknum = as.numeric(substr.Right(ISOweek(Thu.week), 2))) %>% mutate(season = ifelse(wknum<40, as.integer(substr(Thu.week, 3, 4)), as.integer(substr(Thu.week, 3, 4))+1)) 

# 1) include only zip3s where lm was performed; 2) set .fitted + 1.96*.se.fit as the epidemic threshold
data3 <- data2 %>% filter(incl.lm)  %>% mutate(epi.thresh = .fitted+(1.96*.se.fit)) 


## Try different analyses to determine flu season bounds
#### Analysis 2 ###########################
# Analysis 2: a zip3 had a flu epidemic if ILI.dt > epi.threshold for at least x consecutive weeks during the flu period
# maximum number of consecutive epidemics during flu weeks
zips.over.thresh_flu_consec <- data3 %>% filter(flu.week) %>% mutate(epidemic = ilin.dt>epi.thresh) %>% group_by(season, scale) %>% summarise(flu = rle.func(epidemic))
# maximum number of consecutive epidemics during non-flu weeks
zips.over.thresh_nonflu_consec <- data3 %>% filter(!flu.week) %>% mutate(epidemic = ilin.dt>epi.thresh) %>% group_by(season, scale) %>% summarise(nf = rle.func(epidemic))
zips.over.thresh_consec_flat <- full_join(zips.over.thresh_flu_consec, zips.over.thresh_nonflu_consec, by = c("season", "scale"))
zips.over.thresh_consec <- gather(zips.over.thresh_consec_flat, period, consec.weeks, 3:4)

# plot histograms for consecutive weeks
dir.create(sprintf('../graph_outputs/explore_fluSeasonDefinition_%silinDt%s%s', code, code2, code.str), showWarnings=FALSE)
setwd(sprintf('../graph_outputs/explore_fluSeasonDefinition_%silinDt%s%s', code, code2, code.str))

# single histogram for all seasons
consec.flu.nf.single <- ggplot(zips.over.thresh_consec, aes(x=consec.weeks, group=period)) +
  geom_histogram(aes(y=..density.., color=period), binwidth=1, alpha=0.5, position="dodge") +
  coord_cartesian(ylim=c(0, 0.25))
ggsave(sprintf("consecEpiWeeks_%silinDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), consec.flu.nf.single, width=4, height=4)

# compare consecutive epi weeks across flu and non-flu periods by season
# flu period plot
consec.flu <- ggplot(zips.over.thresh_consec %>% filter(period=="flu"), aes(x=consec.weeks, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=1) + 
  coord_cartesian(ylim=c(0, 0.3)) +
  facet_wrap(~season) + ggtitle("flu period")
ggsave(sprintf("consecEpiWeeks_fluPeriod_%silinDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), consec.flu, width=9, height=6)
# nonflu period plot
consec.nflu <- ggplot(zips.over.thresh_consec %>% filter(period=="nf"), aes(x=consec.weeks, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=1) + 
  coord_cartesian(ylim=c(0, 0.3)) +
  facet_wrap(~season) + ggtitle("non-flu period")
ggsave(sprintf("consecEpiWeeks_nfPeriod_%silinDt%s%s.png", code, code2, code.str, spatial$stringabbr), consec.nflu, width=9, height=6)

# summarise means and variance of consecutive flu weeks for each season
consec.summary <- zips.over.thresh_consec_flat %>% group_by(season) %>% summarise(., flu.mn = mean(flu), nflu.mn = mean(nf), flu.var = var(flu), nflu.var = var(nf))

# quantile for flu period
zips.over.thresh_consec %>% filter(period=="flu") %>% select(consec.weeks) %>% unlist %>% quantile(seq(0, 1, 0.05), na.rm=T)
# quantile for non flue period
zips.over.thresh_consec %>% filter(period=="nf") %>% select(consec.weeks) %>% unlist %>% quantile(seq(0, 1, 0.05), na.rm=T)
# span0.4, state
  # during flu period: 80-85% of zip3-season combinations have 4+ consecutive weeks above the epidemic threshold
  # during flu period: ~80% of zip3-season combinations have 5+ consecutive weeks above the epidemic threshold
  # during non-flu period: 80% of zip3-season combinations have 4 or fewer consecutive weeks above the epidemic threshold
  # during non-flu period: ~75% of zip3-season combinations have 5 or fewer consecutive weeks above the epidemic threshold

#### Analysis 3: 9/15/15 ###########################
# a) remove the zip-season combinations where # consecutive weeks that ILI.dt > epi.threshold is greater during the non-flu period than the flu period. We reason that zip3s with more or equivalent ILI.dt activity during the non-flu season than during the flu season have too much noise to detect the true flu activity
# b) (like analysis 2) a zip3 had a flu epidemic if ILI.dt > epi.threshold for at least x consecutive weeks during the flu period
# maximum number of consecutive epidemics during flu weeks
zips.over.thresh_flu_consec <- data3 %>% filter(flu.week) %>% mutate(epidemic = ilin.dt>epi.thresh) %>% group_by(season, scale) %>% summarise(flu = rle.func(epidemic))
# maximum number of consecutive epidemics during non-flu weeks
zips.over.thresh_nonflu_consec <- data3 %>% filter(!flu.week) %>% mutate(epidemic = ilin.dt>epi.thresh) %>% group_by(season, scale) %>% summarise(nf = rle.func(epidemic))
zips.over.thresh_consec_flat2 <- right_join(zips.over.thresh_flu_consec, zips.over.thresh_nonflu_consec, by = c("season", "scale")) %>% mutate(incl.analysis = flu > nf) # 4596/6464 zip3-seasons were T for incl.analysis
zips.over.thresh_consec2 <- zips.over.thresh_consec_flat2 %>% filter(incl.analysis) %>% gather(period, consec.weeks, 3:4) %>% select(-incl.analysis)

# single histogram for all seasons, zip3 noise removed
consec.flu.nf.single2 <- ggplot(zips.over.thresh_consec2, aes(x=consec.weeks, group=period)) +
  geom_histogram(aes(y=..density.., color=period), binwidth=1, alpha=0.5, position="dodge") +
  coord_cartesian(ylim=c(0, 0.3))
ggsave(sprintf("consecEpiWeeks_%silinDt%s%s%s_rmNoise.png", code, code2, code.str, spatial$stringabbr), consec.flu.nf.single2, width=4, height=4)

# quantile for flu period
zips.over.thresh_consec2 %>% filter(period=="flu") %>% select(consec.weeks) %>% unlist %>% quantile(seq(0, 1, 0.05), na.rm=T)
# quantile for non flu period
zips.over.thresh_consec2 %>% filter(period=="nf") %>% select(consec.weeks) %>% unlist %>% quantile(seq(0, 1, 0.05), na.rm=T)
# span = 0.4, state
  # during flu period: 95% of zip3-season combinations have 5+ consecutive weeks above the epidemic threshold
  # during non-flu period: 80% of zip3-season combinations have 5 or fewer consecutive weeks above the epidemic threshold

# 12/15/15: use five consecutive weeks as the lower threshold for state-level analyses
