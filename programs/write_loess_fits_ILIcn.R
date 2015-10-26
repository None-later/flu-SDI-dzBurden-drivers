
## Name: Elizabeth Lee
## Date: 10/25/15
## Function: smooth ILIc data using loess smoother during summer weeks, divide observed ili by smoothed loess fits for entire time series. Pass the processed ILI metric to write_loess_PeriodicReg_....R
## timetrend <- loess(ILIc ~ t) during summer weeks 
## ILIcDt <- ILIc/timetrend
## lm(ILIcDt ~ t + cos(2*pi*t/52.18) + sin(2*pi*t/52.18))

## Filenames: 
## Data Source: 
## Notes: code2 = 'Octfit' --> October to April is flu period, but we fit the seasonal regression from April to October (i.e., expanded definition of summer) in order to improve phase of regression fits
## 52.18 weeks per year in the regression model
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

write_loess_fits_ILIcn <- function(span.var, degree.var, metric){
  #### header ####################################
  require(dplyr)
  require(ggplot2)
  require(broom)
  require(tidyr)
  require(readr)
  setwd(dirname(sys.frame(1)$ofile))
  
  #### import data ####################################
  setwd('../R_export')
  ilic_df <- read_csv('ilicByallZip_allWeekly_totServ_totAge.csv', col_types = list("zip3" = col_character(), ili = col_integer(), pop = col_integer(), cov_z.y = col_double(), alpha_z.y = col_double(), ILIc = col_double(), cov_below5 = col_logical()))
  
  #### set these! ################################
#   span.var <- 0.4 # 0.4, 0.6
#   degree.var <- 2
  code.str <- sprintf('_span%s_degree%s', span.var, degree.var)
  
  #### data cleaning ####################################
  # create new data for augment
  newbasedata <- ilic_df %>% select(Thu.week, t) %>% unique %>% filter(Thu.week < as.Date('2009-05-01'))
  
  #### perform loess regression ####################################
  allLoessMods <- ilic_df %>% filter(fit.week) %>% filter(Thu.week < as.Date('2009-05-01')) %>% 
    filter(incl.lm) %>%       
    mutate(ILIcn = ILIc/pop*10000) %>%
    group_by(zip3) %>%
    do(fitZip3 = loess(ILIcn ~ t, span = span.var, degree = degree.var, data = ., na.action=na.exclude))
    
  allLoessMods_aug <- augment(allLoessMods, fitZip3, newdata= newbasedata)
 
    # after augment - join ILI data to fits
  allLoessMods_fit_ILI <- right_join((allLoessMods_aug %>% ungroup %>% select(-t)), (ilic_df %>% filter(Thu.week < as.Date('2009-05-01'))), by=c('Thu.week', 'zip3')) %>% 
    mutate(week=as.Date(week, origin="1970-01-01")) %>% 
    mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01")) %>% 
    mutate(ilicn.dt = ifelse(.fitted <= 1, ILIcn, ILIcn/.fitted)) 

  
  
  #### write data to file ####################################
  setwd('../R_export')
  # write fitted and original loess smoothed ILI data 
  write.csv(allLoessMods_fit_ILI, file=sprintf('loess%s_allZip3Mods_ILIcn.csv', code.str), row.names=FALSE)
  
  
}



