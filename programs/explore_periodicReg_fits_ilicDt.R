
## Name: Elizabeth Lee
## Date: 10/26/15
## Function: 1. explore periodic regression fits of detrended ilic (ilic/fitted.loess) through plotting
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")


explore_periodicReg_fits_ilicDt <- function(span.var, degree.var){
  #### header ####################################
  require(dplyr)
  require(ggplot2)
  require(readr)
  setwd(dirname(sys.frame(1)$ofile))
  
  #### set these! ####################################
  code <-"" # linear time trend term
  code2 <- "_Octfit" # fit = Apr to Oct and fluseason = Oct to Apr
  
  # span.var <- 0.4 # 0.4, 0.6
  # degree.var <- 2
  code.str <- sprintf('_span%s_degree%s', span.var, degree.var)
  
  #### plot formatting ################################
  w <- 9; h <- 6
  num <- 6
  
  #### import data ################################
  setwd('../R_export')
  data <- read_csv(file=sprintf('periodicReg_%sallZip3Mods_ilicDt%s%s.csv', code, code2, code.str), col_types=list(zip3 = col_character(), ili = col_integer(), pop = col_integer(), cov_z.y = col_double(), alpha_z.y = col_double(), ILIc = col_double(), cov_below5 = col_logical(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilic.dt = col_double()))
  
  fitdata <- read_csv(file=sprintf('summaryStats_periodicReg_%sallZip3Mods_ilicDt%s%s.csv', code, code2, code.str), col_types=list(zip3 = col_character()))
  
  #### initial time series plots ################################
  dir.create(sprintf('../graph_outputs/explore_periodicReg_%sfits_ilicDt%s%s', code, code2, code.str), showWarnings = FALSE)
  setwd(sprintf('../graph_outputs/explore_periodicReg_%sfits_ilicDt%s%s', code, code2, code.str))
  
#   zip3list <- data %>% filter(incl.lm) %>% select(zip3) %>% distinct %>% mutate(for.plot = seq_along(1:nrow(.)))
#   data_plot <- right_join(data, zip3list, by="zip3")
#   indexes <- seq(1, max(data_plot %>% select(for.plot)), by=num)
#   
#   for(i in indexes){
#     dummyplots <- ggplot(data_plot %>% filter(for.plot>= i & for.plot < i+num), aes(x=week, y=ilic.dt, group=zip3)) +
#       theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
#       geom_line(aes(color = flu.week)) + scale_color_discrete(name='flu.week (fit: Apr to Oct)') + 
#       geom_line(aes(y = .fitted), color = 'black') + 
#       geom_ribbon(aes(ymin = .fitted-(1.96*.se.fit), ymax = .fitted+(1.96*.se.fit), alpha=0.7), fill = 'green') +
#       scale_alpha_continuous(name='', breaks=c(0.7), labels=c('95% CI fit')) + 
#       facet_wrap(~zip3, scales="free_y")
#     # grab zip3s in plot for file name
#     ziplabels <- data_plot %>% select(zip3) %>% distinct %>% slice(c(i, i+num-1)) 
#     ggsave(sprintf("periodicReg_%sfits_ilicDt%s%s_%s-%s.png", code, code2, code.str, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
#   } 
  
  #### 10/20/15 residual vs fitted ################################
  dir.create('./diagnostics', showWarnings = FALSE)
  setwd('./diagnostics')
  
#   data_plot2 <- data_plot %>% filter(fit.week) %>% mutate(resid = ilic.dt - .fitted) %>% mutate(data95indic = ifelse(ilic.dt > .fitted-(1.96*.se.fit) & ilic.dt < .fitted+(1.96*.se.fit), TRUE, FALSE))
#   indexes2 <- indexes
#   
#   for (i in indexes2){
#     dummyplots <- ggplot(data_plot2 %>% filter(for.plot>= i & for.plot < i+num), aes(x=.fitted, y=resid, group=zip3)) +
#       geom_point(aes(colour = data95indic)) +
#       scale_colour_discrete(name = 'In 95%CI of model fit') + # Do 95% of observed data fit within the 95%CI?
#       theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
#       facet_wrap(~zip3, scales="free") +
#       xlab('Fitted Values (fit.week = T)') +
#       ylab('Residuals (obs - fitted)')
#     # grab zip3s in plot for file name
#     ziplabels <- data_plot %>% select(zip3) %>% distinct %>% slice(c(i, i+num-1)) 
#     ggsave(sprintf("periodicReg_%sresidualsVsFitted_ilicDt%s%s_%s-%s.png", code, code2, code.str, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
#   } 
  
  #### 10/26/15 histogram of adjusted R^2 ################################
  quantD <- fitdata %>% summarise(q05 = quantile(adj.r.squared, 0.05), q25 = quantile(adj.r.squared, 0.25), q50 = quantile(adj.r.squared, 0.5), q75 = quantile(adj.r.squared, 0.75), q95 = quantile(adj.r.squared, 0.95))
    
  adjR2plot <- ggplot(fitdata, aes(x = adj.r.squared)) +
    geom_histogram() + 
    geom_vline(data = quantD, aes(xintercept = c(q05, q25, q50, q75, q95), colour = 'red'))
  ggsave(sprintf("periodicReg_%sAdjR2_ilicDt%s%s.png", code, code2, code.str), adjR2plot, width=w, height=h)
  
}



