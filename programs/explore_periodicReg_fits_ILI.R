
## Name: Elizabeth Lee
## Date: 9/14/15
## Function: explore periodic regression fits of raw ILI through plotting
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

####################################
# header
setwd('~/Dropbox/code')
source("GeneralTools.R")
require(dplyr)
require(ggplot2)
require(readr)
setwd(dirname(sys.frame(1)$ofile))
####################################
# set these!
# code = "t2sa_" # semi-annual periodicity
code = "t2_" # parabolic time trend term
# code="" # linear time trend term
code2 = "_Oct"
####################################
setwd('../R_export')
data <- read_csv(file=sprintf('periodicReg_%sallZip3Mods_ILI%s.csv', code, code2), col_types=list(ili=col_double(), .fitted=col_double(), .se.fit=col_double()))

dir.create(sprintf('../graph_outputs/explore_periodicReg_%sfits_ILI%s', code, code2), showWarnings=FALSE)
setwd(sprintf('../graph_outputs/explore_periodicReg_%sfits_ILI%s', code, code2))

zip3list <- data %>% filter(incl.lm) %>% select(zip3) %>% distinct %>% mutate(for.plot = seq_along(1:nrow(.))) %>% mutate(zipname = substr.Right(gsub("X", "00", zip3), 3))
data_plot <- right_join(data, zip3list, by="zip3")
indexes <- seq(1, max(data_plot %>% select(for.plot)), by=6)

for(i in indexes){
  dummyplots <- ggplot(data_plot %>% filter(for.plot>= i & for.plot < i+6), aes(x=week, y=ili, group=zipname)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = flu.week)) + scale_color_discrete(name='flu.week (fit to FALSE)') + 
    geom_line(aes(y = .fitted), color = 'black') + 
    geom_ribbon(aes(ymin = .fitted-(1.96*.se.fit), ymax = .fitted+(1.96*.se.fit), alpha=0.7), fill = 'green') +
    scale_alpha_continuous(name='', breaks=c(0.7), labels=c('95% CI fit')) + 
    facet_wrap(~zipname, scales="free_y")
  # grab zip3s in plot for file name
  ziplabels <- data_plot %>% select(zipname) %>% distinct %>% slice(c(i, i+5)) 
  ggsave(sprintf("periodicReg_%sfits_ILI%s_%s-%s.png", code, code2, ziplabels[1,], ziplabels[2,]), dummyplots, width=9, height=6)
}

