
## Name: Elizabeth Lee
## Date: 6/2/16
## Function: functions to perform EDA for variable selection generally -- functions for both cty and st levels
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

require(dplyr); require(tidyr); require(GGally) 
require(INLA)

#### functions for pairwise comparison ################################
pairs_scatterplotMatrix <- function(full_df){
  # return scatterplot matrix of all variables pooled across states & seasons
  print(match.call())
  
  datOnly <- full_df %>%
    select(logy, logE, contains("_"))
  
  pairPlt <- ggpairs(datOnly) +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
  
  return(pairPlt)
}
################################

pairs_corrMatrix <- function(full_df){
  # return correlation matrix for all variables pooled across states & seasons
  print(match.call())
  
  datOnly <- full_df %>% 
    select(logy, logE, contains("_"))
  
  return(ggcorr(datOnly, method = c("pairwise", "spearman"), label = TRUE))
}


#### functions for single variable modeling ################################
subset_singleVariable_data <- function(full_df, s, covariate){
  # subset data for single response & covariate modeling
  print(match.call())
  
  # subset data according to season and covariate in function arguments
  mod_df <- full_df %>%
    filter(season == s & !is.na(lat) & !is.na(lon)) %>%
#     mutate(logy = centerStandardize(logy)) %>%
    rename_(varInterest = covariate) %>%
    select(fips, season, y, logy, logE, varInterest) %>%
    mutate(ID = seq_along(fips))
  
  return(mod_df)
}
################################

model_singleVariable_inla <- function(mod_df, respCode, s, covariate){
  # inla model for single response and covariate, output fixed effect coeff
  print(match.call())
  
  formula <- y ~ varInterest + f(ID, model = "iid")
  
  mod <- inla(formula, family = "gaussian", data = mod_df, 
              control.family = list(link = 'log'),
              control.predictor = list(compute = TRUE), # compute summary statistics on fitted values
              control.compute = list(dic = TRUE),
              # verbose = TRUE,
              offset = logE) # offset of expected cases
  
  names(mod$summary.fixed) <- c("mean", "sd", "q_025", "q_5", "q_975", "mode", "kld")
  modOutput <- tbl_df(mod$summary.fixed) %>%
    mutate(RV = rownames(mod$summary.fixed)) %>%
    filter(RV == "varInterest") %>%
    select(RV, mode, q_025, q_975) 
  
  # data to save
  coefRow <- list(respCode = respCode, singleCov = covariate, season = s, exportDate = as.character(Sys.Date()), coefMode = modOutput$mode, coefQ025 = modOutput$q_025, coefQ975 = modOutput$q_975, DIC = mod$dic$dic)
  
  return(coefRow)
}

#### functions for single variable coef plotting ################################
plot_singleVarCoef_time <- function(coefDat){
  # plot all coef modes, Q.025 - Q0.975 over time
  print(match.call())
  
  figure <- ggplot(coefDat, aes(x = season, y = coefMode, group = singleCov)) +
    geom_pointrange(aes(ymin = coefQ025, ymax = coefQ975)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~singleCov, scales = "free_y") +
    ylab("coefMode (95%CI)")
  
  return(figure)
}