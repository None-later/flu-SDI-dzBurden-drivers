# 12/22/16: separate files for calculate_residuals functions
# load this function when response variable is nonzero y: poisson or negative binomial likelihood

################################

calculate_residuals <- function(fitDat, nonzeroOnly){
  # calculations that depend on fitted values for non-zero values: raw residuals, std residuals, 95% CI for fitted values
  print(match.call())

  if(nonzeroOnly){
    if(is.null(fitDat$y1)){
      returnDat <- fitDat %>%
        mutate(y1 = ifelse(y>0, y, NA)) %>% # 12/22/16 modified for cleanR_epiDur_cty
        mutate(yhat_resid = (y1-mean)/sd) %>%
        mutate(yhat_rawresid = (y1-mean)) %>%
        rename(LB = q_025, UB = q_975)
    } else{
      returnDat <- fitDat %>%
        mutate(yhat_resid = (y1-mean)/sd) %>%
        mutate(yhat_rawresid = (y1-mean)) %>%
        rename(LB = q_025, UB = q_975)
    }
  } else{
    returnDat <- fitDat %>%
    mutate(yhat_resid = (y-mean)/sd) %>%
    mutate(yhat_rawresid = (y-mean)) %>%
    rename(LB = q_025, UB = q_975)
    }
  
  return(returnDat)
}