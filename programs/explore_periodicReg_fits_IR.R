
## Name: Elizabeth Lee
## Date: 5/22/15
## Function: explore periodic regression fits of IR through plotting
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

####################################
# header
setwd('/home/elee/R/source_functions')
source("dfsumm.R")
require(dplyr)
require(ggplot2)
require(readr)

####################################
# local functions
substr.Right <- function(x, numchar){
  return(substr(x, nchar(x)-(numchar-1), nchar(x)))
}
####################################
# set these!
# code = "t2sa_" # semi-annual periodicity
# code = "t2_" # parabolic time trend term
code="" # linear time trend term
####################################
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/R_export')
data <- read_csv(file=sprintf('periodicReg_%sallZip3Mods.csv', code), col_types=list(ili=col_double(), .fitted=col_double(), .se.fit=col_double(), IR=col_double(), pop=col_integer()))

setwd(sprintf('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/explore_periodicReg_%sfits_IR', code))

zip3list <- data %>% filter(incl.lm) %>% select(zip3) %>% distinct %>% mutate(for.plot = seq_along(1:nrow(.))) %>% mutate(zipname = substr.Right(gsub("X", "00", zip3), 3))
data_plot <- right_join(data, zip3list, by="zip3")
indexes <- seq(1, max(data_plot %>% select(for.plot)), by=6)

for(i in indexes){
  dummyplots <- ggplot(data_plot %>% filter(for.plot>= i & for.plot < i+6), aes(x=week, y=IR, group=zipname)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = flu.week)) + scale_color_discrete(name='flu.week (fit to FALSE)') + 
    geom_line(aes(y = .fitted), color = 'black') + 
    geom_ribbon(aes(ymin = .fitted-(1.96*.se.fit), ymax = .fitted+(1.96*.se.fit), alpha=0.7), fill = 'green') +
    scale_alpha_continuous(name='', breaks=c(0.7), labels=c('95% CI fit')) + 
    coord_cartesian(ylim=c(0, 0.2)) + 
    facet_wrap(~zipname)
  # grab zip3s in plot for file name
  ziplabels <- data_plot %>% select(zipname) %>% distinct %>% slice(c(i, i+5)) 
  ggsave(sprintf("periodicReg_%sfits_IR_%s-%s.png", code, ziplabels[1,], ziplabels[2,]), dummyplots, width=9, height=6)
}

# "", "t2_", "t2sa: save 5/31/15 (redo with Thu.week)
