
## Name: Elizabeth Lee
## Date: 10/26/15
## Function: write full datasets from periodic regression data of ilic.dt, in preparation for write_relativeDiseaseBurden_ILIc.R: Zip3-season combinations with equal or more consecutive weeks above the epidemic threshold in the non-flu period than the flu period are filtered out (see explore_fluSeasonDefinition_ILIc.R, Analysis 3 for additional details)

## Filenames: periodicReg_%sallZip3Mods_ilicDt_Octfit%s.csv
## Data Source: IMS Health 
## Notes: 9/15/15 - Refer to explore_fluSeasonDefinition_ILI.R for definition of "flu epidemic". Zip3s are considered to have experienced a flu epidemic if they had 4+ consecutive weeks above the epidemic threshold in the flu period. 
## 10-20-15 - Split program with write_relativeDiseaseBurden_ILI.R; data export files used in write_relativeDiseaseBurden_ILI.R
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

write_fullIndic_periodicReg_ilicDt <- function(span.var, degree.var){
  #### header ####################################
  require(dplyr)
  require(ggplot2)
  require(readr)
  require(ISOweek)
  require(tidyr)
  setwd(dirname(sys.frame(1)$ofile))
  
  #### local functions ####################################
  # return logical column; T if data should be considered as "flu season" (ie. data falls within period with maximum consecutive weeks)
  consider.flu.season <- function(x){
    rle.results = rle(x)
    max.consec = max(0, rle.results$lengths[which(rle.results$values)])
    dummy <- rep(F, length(x))
    pre.index <- (which(rle.results$values & rle.results$lengths==max.consec))[1]
    post.index <- tail((which(rle.results$values & rle.results$lengths==max.consec)), n=1)
    converted.pre.index <- ifelse(pre.index==1, 0, sum(rle.results$lengths[1:(pre.index-1)]))
    #   converted.post.index <- converted.pre.index + max.consec
    #   print(rle.results)
    #   print(pre.index)
    #   print(which(rle.results$values & rle.results$lengths==max.consec))
    converted.post.index <- ifelse(post.index, sum(rle.results$lengths[1:post.index]), NA)
    if(!is.na(converted.pre.index)){
      dummy[(converted.pre.index+1):converted.post.index] <- T
    }
    return(dummy)
  }
  
  #### set these! ####################################
  code <-"" # linear time trend term
  code2 <- "_Octfit"
  # span.var <- 0.4 # 0.4, 0.6
  # degree.var <- 2
  code.str <- sprintf('_span%s_degree%s', span.var, degree.var)
  
  #### data processing (based on explore_fluSeasonDefinition_ILI.R) ####################################
  setwd('../R_export')
  data <- read_csv(file=sprintf('periodicReg_%sallZip3Mods_ilicDt%s%s.csv', code, code2, code.str), col_types=list(zip3 = col_character(), ili = col_integer(), pop = col_integer(), cov_z.y = col_double(), alpha_z.y = col_double(), ILIc = col_double(), cov_below5 = col_logical(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilic.dt = col_double()))
  
  
  # 1) add ISO week numbers; 2) add season numbers ; 3) add real zip3 names
  data2 <- data %>% mutate(wknum = as.numeric(substr.Right(ISOweek(Thu.week), 2))) %>% mutate(season = ifelse(wknum<40, as.integer(substr(Thu.week, 3, 4)), as.integer(substr(Thu.week, 3, 4))+1)) 
  
  # 1) include only zip3s where lm was performed; 2) set .fitted + 1.96*.se.fit as the epidemic threshold; 3) identify which weeks are epi weeks
  data3 <- data2 %>% filter(incl.lm)  %>% mutate(epi.thresh = .fitted+(1.96*.se.fit)) %>% mutate(epi.week = ilic.dt>epi.thresh)
  
  ## See explore_fluSeasonDefinition_IR.R for derivation of flu season definition
  # 9/15/15: filter out zip3-season combinations with equivalent or more ILI activity in the previous non-flu season than flu season (season 1 will use subsequent non-flu season)
  dummy.flu <- data3 %>% filter(flu.week) %>% group_by(season, zip3) %>% summarise(consec.flu.epiweeks = rle.func(epi.week))
  dummy.nf <- data3 %>% filter(!flu.week) %>% group_by(season, zip3) %>% summarise(consec.nf.epiweeks = rle.func(epi.week)) %>% ungroup %>% mutate(season=season+1)
  # 9/15/15 for season 1, use season 1 consec.nf.epiweeks, which occur after the season 1 flu period
  dummy.nf2 <- bind_rows((dummy.nf %>% filter(season==2) %>% ungroup %>% mutate(season=1)), dummy.nf)
  # summarize season-zip3 combinations that have epidemics (4+ consecutive epidemic weeks)
  zip3s_with_epi <- full_join(dummy.flu, dummy.nf2, by=c("season", "zip3")) %>% mutate(incl.analysis = consec.flu.epiweeks > consec.nf.epiweeks) %>% mutate(has.epi = (consec.flu.epiweeks>=4))
  
  # join summary data to full dataset (adds has.epi and incl.analysis indicators)
  data4 <- right_join(data3, zip3s_with_epi %>% select(-contains("consec.")), by=c("season", "zip3"))
  # 9/15/15: in.season indicator: must meet flu.week, has.epi, incl.analysis, and consecutive epi.week criteria (FLU PERIOD DATA ONLY)
  data5 <- data4 %>% filter(flu.week & has.epi & incl.analysis) %>% group_by(season, zip3) %>% mutate(in.season = consider.flu.season(epi.week))
  data6 <- left_join(data4, (data5 %>% ungroup %>% select(Thu.week, zip3, in.season)), by = c("Thu.week", "zip3")) %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01")) %>% filter(has.epi & incl.analysis)
  
  # save to file 
  print(sprintf('writing full indicators to file %s', code.str))
  # these data are used in "write_relativeDiseaseBurden_ilicDt.R" for further processing of disease burden metrics
  write.csv(data5, file = sprintf('fullIndicFlu_periodicReg_%silicDt%s%s_analyzeDB.csv', code, code2, code.str), row.names=FALSE)
  write.csv(data6, file = sprintf('fullIndicAll_periodicReg_%silicDt%s%s_analyzeDB.csv', code, code2, code.str), row.names=FALSE)
  
}



