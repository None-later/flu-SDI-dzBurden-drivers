
## Name: Elizabeth Lee
## Date: 4/19/17
## Function: 2009 pandemic version: write full datasets from periodic regression data of ilin.dt, in preparation for write_relativeDiseaseBurden_ILIn.R: Zip3-season combinations with equal or more consecutive weeks above the epidemic threshold in the non-flu period than the flu period are filtered out (see explore_fluSeasonDefinition_ILIn.R, Analysis 3 for additional details)

## Filenames: periodicReg_%sall%sMods_ilinDt_Octfit%s.csv
## Data Source: IMS Health 
## Notes: 12/15/15 - Refer to explore_fluSeasonDefinition_ilinDt.R for definition of "flu epidemic". States are considered to have experienced a flu epidemic if they had 5+ consecutive weeks above the epidemic threshold in the flu period. 
## 10-20-15 - Split program with write_relativeDiseaseBurden_ILI.R; data export files used in write_relativeDiseaseBurden_ILI.R
## 12-10-15 - add spatial scale option (zip3 or state)
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

write_fullIndic_periodicReg_ilinDt <- function(span.var, degree.var, spatial){
  print(deparse(sys.call()))
  
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
    max.consec = max(0, rle.results$lengths[which(rle.results$values)]) # which(rle.results$values) works because values are boolean
    dummy <- rep(F, length(x))
    pre.index <- (which(rle.results$values & rle.results$lengths==max.consec))[1] # rle index of first value in longest consecutive run of TRUE
    post.index <- tail((which(rle.results$values & rle.results$lengths==max.consec)), n=1) # rle index of last value in longest consec run of TRUE
    converted.pre.index <- ifelse(pre.index==1, 0, sum(rle.results$lengths[1:(pre.index-1)])) # vector index - 1 of first value in longest consec run of TRUE
    #   converted.post.index <- converted.pre.index + max.consec
    #   print(rle.results)
    #   print(pre.index)
    #   print(which(rle.results$values & rle.results$lengths==max.consec))
    converted.post.index <- ifelse(post.index, sum(rle.results$lengths[1:post.index]), NA) # vector index of last value in longest consec run of TRUE
    if(!is.na(converted.pre.index)){
      dummy[(converted.pre.index+1):converted.post.index] <- T
    }
    return(dummy) # full vector of T/F
  }
  
  #### set these! ####################################
  code <-"" # linear time trend term
  code2 <- "_Octfit"
  
  # # uncomment when running script separately
  # spatial <- list(scale = "county", stringcode = "County", stringabbr = "", serv = "_totServ", servToggle = "", age = "_totAge", ageToggle = "", panToggle = "_2009p")
  # span.var <- 0.4 # 0.4, 0.6
  # degree.var <- 2
  
  code.str <- sprintf('_span%s_degree%s', span.var, degree.var)
  
  #### data processing (based on explore_fluSeasonDefinition_ILI.R) ####################################
  setwd('../R_export')
  
  if (spatial$scale == 'county'){
    data <- read_csv(file=sprintf('periodicReg_%sall%sMods_ilinDt%s%s%s%s%s.csv', code, spatial$stringcode, code2, spatial$servToggle, spatial$ageToggle, spatial$panToggle, code.str), col_types=list(fips = col_character(), ili = col_double(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double())) %>%
      rename(scale = fips)
    num.weeks <- 2 # 8/19/16 reduced because incl.analysis excludes counties that are too noisy anyways # see explore_fluSeasonDefinition_ilinDt.R
  }
  
  # 1) add ISO week numbers; 2) 4/19/17 add pandemic season numbers where season begins in July (reg season begins Oct); 3) 4/19/17 assign 2009p flu.week from August through January TRUE
  data2 <- data %>% 
    mutate(wknum = as.numeric(substr.Right(ISOweek(Thu.week), 2))) %>% 
    mutate(season = ifelse(wknum<40, as.integer(substr(Thu.week, 3, 4)), as.integer(substr(Thu.week, 3, 4))+1)) %>% 
    mutate(pseason = ifelse(wknum<28, as.integer(substr(Thu.week, 3, 4)), as.integer(substr(Thu.week, 3, 4))+1)) %>% 
    mutate(pflu.week =(month >= 8 | month <= 1))
  
  # 1) include only zip3s where lm was performed; 2) set .fitted + 1.96*.se.fit as the epidemic threshold; 3) identify which weeks are epi weeks
  # 8/20/16 if ILIn >= .fitted (incl.lm2), epi.thresh = 0 and epi.week = FALSE
  data3 <- data2 %>% 
    filter(incl.lm) %>% 
    mutate(epi.thresh = ifelse(incl.lm2, .fitted+(1.96*.se.fit), 0)) %>% 
    mutate(epi.week = ilin.dt>epi.thresh)

  # 4/23/17: flu.week for pre-pandemic onset estimation
  dummy.flupre09 <- data3 %>% filter(flu.week & season < 10) %>% group_by(season, scale) %>% summarise(consec.flu.epiweeks = rle.func(epi.week)) %>% ungroup
  # 4/19/17: pflu.week for pandemic onset estimation
  dummy.flu2009 <- data3 %>% filter(pflu.week & pseason == 10) %>% group_by(pseason, scale) %>% summarise(consec.flu.epiweeks = rle.func(epi.week)) %>% ungroup %>% rename(season = pseason)

  # summarize season-zip3 combinations that have epidemics (num.weeks+ consecutive epidemic weeks)
  # 4/19/17: incl.analysis is all TRUE for pandemic version
  zip3s_with_epi <- bind_rows(dummy.flupre09, dummy.flu2009) %>% mutate(incl.analysis = TRUE) %>% mutate(has.epi = (consec.flu.epiweeks>=num.weeks))

  # join summary data to full dataset (adds has.epi and incl.analysis indicators)
  data4 <- right_join(data3, zip3s_with_epi %>% select(-contains("consec.")), by=c("season", "scale"))
  # 9/15/15: in.season indicator: must meet pflu.week, has.epi, incl.analysis, and consecutive epi.week criteria (FLU PERIOD DATA ONLY)
  data5.pre09 <- data4 %>% filter(flu.week & has.epi & incl.analysis & season < 10) %>% group_by(season, scale) %>% mutate(in.season = consider.flu.season(epi.week)) %>% ungroup
  
  data5.09 <- data4 %>% filter(pflu.week & has.epi & incl.analysis & pseason == 10) %>% group_by(pseason, scale) %>% mutate(in.season = consider.flu.season(epi.week)) %>% ungroup
  data5 <- bind_rows(data5.pre09, data5.09)

  data6 <- left_join(data4, (data5 %>% select(Thu.week, scale, in.season)), by = c("Thu.week", "scale")) %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01")) # rm filter(incl.analysis)

  # rename variable "scale" to zip3 or state
  data5_write <- scaleRename(spatial$scale, data5)
  data6_write <- scaleRename(spatial$scale, data6)

  # save to file
  print(sprintf('writing full indicators to file %s', code.str))
  # these data are used in "write_relativeDiseaseBurden_ilinDt.R" for further processing of disease burden metrics
  write.csv(data5_write, file = sprintf('fullIndicFlu_periodicReg_%silinDt%s%s%s%s%s_analyzeDB%s.csv', code, code2, spatial$servToggle, spatial$ageToggle, spatial$panToggle, code.str, spatial$stringabbr), row.names=FALSE)
  write.csv(data6_write, file = sprintf('fullIndicAll_periodicReg_%silinDt%s%s%s%s%s_analyzeDB%s.csv', code, code2, spatial$servToggle, spatial$ageToggle, spatial$panToggle, code.str, spatial$stringabbr), row.names=FALSE)

}



