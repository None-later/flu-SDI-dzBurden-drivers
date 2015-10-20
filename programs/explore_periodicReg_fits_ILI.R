
## Name: Elizabeth Lee
## Date: 9/14/15
## Function: 1. explore periodic regression fits of raw ILI through plotting, 2. examine in season classifications
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
# code <- "t2sa_" # semi-annual periodicity
# code <- "t2_" # parabolic time trend term
code <- "t4_" # quartic time trend
# code <-"" # linear time trend term

# code2 <- "_Oct" # fluseason = Oct to Apr
code2 <- "_Octfit" # fit = Apr to Oct and fluseason = Oct to Apr
#### plot formatting ################################
w <- 9; h <- 6

#### import data ################################
setwd('../R_export')
data <- read_csv(file=sprintf('periodicReg_%sallZip3Mods_ILI%s.csv', code, code2), col_types=list(ili=col_double(), .fitted=col_double(), .se.fit=col_double()))

#### initial time series plots ################################
dir.create(sprintf('../graph_outputs/explore_periodicReg_%sfits_ILI%s', code, code2), showWarnings = FALSE)
setwd(sprintf('../graph_outputs/explore_periodicReg_%sfits_ILI%s', code, code2))

zip3list <- data %>% filter(incl.lm) %>% select(zip3) %>% distinct %>% mutate(for.plot = seq_along(1:nrow(.))) %>% mutate(zipname = substr.Right(gsub("X", "00", zip3), 3))
data_plot <- right_join(data, zip3list, by="zip3")
indexes <- seq(1, max(data_plot %>% select(for.plot)), by=6)

# for(i in indexes){
#   dummyplots <- ggplot(data_plot %>% filter(for.plot>= i & for.plot < i+6), aes(x=week, y=ili, group=zipname)) +
#     theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
#     geom_line(aes(color = flu.week)) + scale_color_discrete(name='flu.week (fit: Apr to Oct)') + 
#     geom_line(aes(y = .fitted), color = 'black') + 
#     geom_ribbon(aes(ymin = .fitted-(1.96*.se.fit), ymax = .fitted+(1.96*.se.fit), alpha=0.7), fill = 'green') +
#     scale_alpha_continuous(name='', breaks=c(0.7), labels=c('95% CI fit')) + 
#     facet_wrap(~zipname, scales="free_y")
#   # grab zip3s in plot for file name
#   ziplabels <- data_plot %>% select(zipname) %>% distinct %>% slice(c(i, i+5)) 
#   ggsave(sprintf("periodicReg_%sfits_ILI%s_%s-%s.png", code, code2, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
# } # saved 10/19/15

#### 10/20/15 residual vs fitted ################################
dir.create('./diagnostics', showWarnings = FALSE)
setwd('./diagnostics')

data_plot2 <- data_plot %>% filter(fit.week) %>% mutate(resid = ili - .fitted) %>% mutate(data95indic = ifelse(ili > .fitted-(1.96*.se.fit) & ili < .fitted+(1.96*.se.fit), TRUE, FALSE))
indexes2 <- indexes

for (i in indexes2){
  dummyplots <- ggplot(data_plot2 %>% filter(for.plot>= i & for.plot < i+6), aes(x=.fitted, y=resid, group=zipname)) +
    geom_point(aes(colour = data95indic)) +
    scale_colour_discrete(name = 'In 95%CI of model fit') + # Do 95% of observed data fit within the 95%CI?
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    facet_wrap(~zipname, scales="free") +
    xlab('Fitted Values (fit.week = T)') +
    ylab('Residuals (obs - fitted)')
    # grab zip3s in plot for file name
    ziplabels <- data_plot %>% select(zipname) %>% distinct %>% slice(c(i, i+5)) 
    ggsave(sprintf("periodicReg_%sresidualsVsFitted_ILI%s_%s-%s.png", code, code2, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
} # saved 10/20/15


