
## Name: Elizabeth Lee
## Date: 9/14/15
## Function: explore loess regression fits of raw ILI through plotting; the loess fits will serve as a baseline to process the raw ILI data, removing time trends
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")
rm(list = ls())
#### header ####################################
require(dplyr)
require(ggplot2)
require(readr)
setwd(dirname(sys.frame(1)$ofile))

#### set these! ####################################
span.var <- 0.6 # 0.4, 0.6
degree.var <- 2
code.str <- sprintf('_span%s_degree%s', span.var, degree.var)

#### plot formatting ################################
w <- 9; h <- 6

#### import data ################################
setwd('../R_export')
data <- read_csv(file=sprintf('loess%s_allZip3Mods_ILI.csv', code.str), col_types=list(ili=col_double(), .fitted=col_double(), .se.fit=col_double(), ili.dt=col_double(), zipname=col_character()))

#### loess fit plots ################################
dir.create(sprintf('../graph_outputs/explore_loess_fits_ILI/fits%s', code.str), showWarnings = FALSE)
setwd(sprintf('../graph_outputs/explore_loess_fits_ILI/fits%s', code.str))

zip3list <- data %>% filter(incl.lm) %>% select(zip3) %>% distinct %>% mutate(for.plot = seq_along(1:nrow(.)))
data_plot <- right_join(data, zip3list, by="zip3") %>% mutate(Thu.week=as.Date(Thu.week, origin="1970-01-01"))
indexes <- seq(1, max(data_plot %>% select(for.plot)), by=6)

for(i in indexes){
  dummyplots <- ggplot(data_plot %>% filter(for.plot>= i & for.plot < i+6), aes(x=Thu.week, y=ili, group=zipname)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = flu.week)) + scale_color_discrete(name='flu.week (fit: May to Sept)') + 
    geom_line(aes(y = .fitted), color = 'black') + 
    geom_ribbon(aes(ymin = .fitted-(1.96*.se.fit), ymax = .fitted+(1.96*.se.fit), alpha=0.7), fill = 'green') +
    scale_alpha_continuous(name='', breaks=c(0.7), labels=c('95% CI fit')) + 
    facet_wrap(~zipname, scales="free_y")
  # grab zip3s in plot for file name
  ziplabels <- data_plot %>% select(zipname) %>% distinct %>% slice(c(i, i+5)) 
  ggsave(sprintf("loess%s_fits_ILI_%s-%s.png", code.str, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
} # saved 10/19/15

# 10/20/15 overlaps with periodicReg_fits_iliDt/
#### ili.dt plots ################################
dir.create(sprintf('../iliDt%s', code.str), showWarnings = FALSE)
setwd(sprintf('../iliDt%s', code.str))

for(i in indexes[1:10]){
  dummyplots <- ggplot(data_plot %>% filter(for.plot>= i & for.plot < i+6), aes(x=Thu.week, y=ili.dt, group=zipname)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = fit.week)) + 
    facet_wrap(~zipname, scales="free_y")
  # grab zip3s in plot for file name
  ziplabels <- data_plot %>% select(zipname) %>% distinct %>% slice(c(i, i+5)) 
  ggsave(sprintf("iliDt%s_data_%s-%s.png", code.str, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
} # saved 10/19/15

