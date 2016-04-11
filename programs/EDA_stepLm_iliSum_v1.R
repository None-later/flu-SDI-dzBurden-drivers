
## Name: Elizabeth Lee
## Date: 2/23/16
## Function: EDA stepwise regression - to get a preview of which covariates may be important
## Filenames: dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr) # clean_data_functions dependencies
require(RColorBrewer); require(ggplot2) # export_inlaData_st dependencies
require(MASS)

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr <- "stepLm_iliSum_v1"
seasons <- 2:9

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_data_functions.R") # functions to clean original data sources
source("source_variableSelection_st.R") # functions for variable selection analyses
source("source_export_inlaData_st.R") # functions to export data and plots related to model


#### FILEPATHS #################################
setwd('../reference_data')
path_pop_st <- paste0(getwd(), "/pop_st_Census_00-10.csv")
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")

setwd("../R_export")
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))
path_imsCov_st <- paste0(getwd(), "/physicianCoverage_IMSHealth_state.csv")

# put all paths in a list to pass them around in functions
path_list <- list(path_pop_st = path_pop_st,
                  path_abbr_st = path_abbr_st,
                  path_latlon_st = path_latlon_st,
                  path_response_st = path_response_st,
                  path_imsCov_st = path_imsCov_st)

#### FUNCTIONS ################################
plot_diagnostics <- function(modelData, parlist, varslist, BLcode){
  w <- parlist$w; h <- parlist$h; res <- parlist$res; seasons <- parlist$seasons
  
  for (s in seasons){
    md <- modelData %>% 
      filter(season == s) %>%
      dplyr::select(one_of(eval(varslist[[s-1]]))) %>%
      na.omit
    
    bm <- lm(y ~ ., data = md)
    
    png(sprintf("bestfit_%s_S%s.png", BLcode, s), width = w, height = h, res = res, units = "in")
    par(mfrow = c(1, 2))
    plot(md$y, bm$fitted.values, xlab = sprintf("Observed, S%s", s), ylab = "Fitted")
    plot(bm$fitted.values, bm$residuals, xlab = "Fitted", ylab = "Residuals")
    dev.off()
  }
  
}


#### PLOT FORMATTING ################################
w <- 6; h <- 6; res <- 300
w2 <- 8; h2 <- 5
paramlist <- list(w = w2, h = h2, res = res, seasons = seasons)

#### MAIN #################################
#### Import and process data ####
# main data
modData <- stepLm_iliSum_v1(path_list) # with all available cleaned variables

# remove seasons
modData2 <- modData %>%
  dplyr::select(y, contains("_"))

#### process season data ####
s23var <- c("y", "O_imscoverage", "O_careseek", "O_insured", "X_poverty", "X_income", "X_elderly", "X_hcaccess", "X_physaccess", "X_flight")
s45var <- c("y", "O_imscoverage", "O_careseek", "O_insured", "X_poverty", "X_income", "X_mcaid", "X_elderly", "X_hcaccess", "X_physaccess", "X_flight")
s67var <- c("y", "O_imscoverage", "O_careseek", "O_insured", "X_poverty", "X_income", "X_mcaid", "X_hcaccess", "X_physaccess", "X_commute", "X_flight")
s8var <- c("y", "O_imscoverage", "O_careseek", "O_insured", "X_poverty", "X_income", "X_mcaid", "X_elderly", "X_hcaccess", "X_physaccess", "X_commute", "X_flight")
s9var <- c("y", "O_imscoverage", "O_careseek", "O_insured", "X_poverty", "X_income", "X_elderly", "X_hcaccess", "X_commute", "X_flight")

svars <- list(s23var, s23var, s45var, s45var, s67var, s67var, s8var, s9var)

#### modeling ################################
#### starting with measurement error variables at baseline ####
for (s in seasons){
  print(sprintf("********* SEASON %s, meas vars at BL ********", s))
  
  md <- modData %>%
    filter(season == s) %>%
    dplyr::select(one_of(eval(svars[[s-1]]))) %>%
    na.omit
  
  steps <- stepAIC(lm(y ~ O_imscoverage + O_careseek + O_insured, data = md), 
                   scope = list(upper = lm(y ~ ., data = md)), direction = "forward", trace=1)
  }

#### starting with no variables at baseline ####
for (s in seasons){
  print(sprintf("********* SEASON %s, no vars at BL ********", s))
  
  md <- modData %>%
    filter(season == s) %>%
    dplyr::select(one_of(eval(svars[[s-1]]))) %>%
    na.omit
  
  steps <- stepAIC(lm(y ~ 1, data = md), 
                   scope = list(upper = lm(y ~ ., data = md)), direction = "forward", trace=1)
}

#### exploratory figures ################################
dir.create("../graph_outputs/EDA_stepLm_iliSum_v1", showWarnings = FALSE)
setwd("../graph_outputs/EDA_stepLm_iliSum_v1")

for (s in seasons){
  print(sprintf("**** SAVING SEASON %s CROSS-CORRELATION PLOTS ****", s))
  
  md <- modData %>%
    filter(season == s) %>%
    dplyr::select(one_of(eval(svars[[s-1]]))) %>%
    na.omit
  
  png(sprintf("crosscorr_iliSum_S%s.png", s), width = w, height = h, units = "in", res = res)
  plot(md)
  dev.off()
}



#### examine best model fits ################################

#### starting with measurement error variables at baseline ####
best2m <- c("y", "O_imscoverage", "O_careseek", "O_insured")
best3m <- c("y", "O_imscoverage", "O_careseek", "O_insured", "X_income")
best4m <- c("y", "O_imscoverage", "O_careseek", "O_insured", "X_physaccess")
best5m <- c("y", "O_imscoverage", "O_careseek", "O_insured", "X_hcaccess")
best6m <- c("y", "O_imscoverage", "O_careseek", "O_insured", "X_hcaccess", "X_physaccess")
best7m <- c("y", "O_imscoverage", "O_careseek", "O_insured", "X_income", "X_physaccess")
best8m <- c("y", "O_imscoverage", "O_careseek", "O_insured", "X_poverty", "X_income", "X_mcaid", "X_elderly", "X_commute")
best9m <- c("y", "O_imscoverage", "O_careseek", "O_insured", "X_poverty", "X_elderly", "X_hcaccess", "X_flight")

bvarsm <- list(best2m, best3m, best4m, best5m, best6m, best7m, best8m, best9m)
blcode <- "BLmeas"

plot_diagnostics(modData, paramlist, bvarsm, blcode)


#### starting with no variables at baseline ####
best2o <- c("y")
best3o <- c("y")
best4o <- c("y", "O_imscoverage", "X_physaccess")
best5o <- c("y", "X_hcaccess")
best6o <- c("y", "O_imscoverage", "O_careseek", "X_poverty", "X_hcaccess", "X_physaccess")
best7o <- c("y", "X_poverty", "X_income", "X_physaccess")
best8o <- c("y", "X_income", "X_mcaid")
best9o <- c("y", "O_imscoverage", "O_careseek", "O_insured", "X_poverty", "X_elderly", "X_flight")

bvarso <- list(best2o, best3o, best4o, best5o, best6o, best7o, best8o, best9o)
blcode <- "BLnone"

plot_diagnostics(modData, paramlist, bvarso, blcode)


