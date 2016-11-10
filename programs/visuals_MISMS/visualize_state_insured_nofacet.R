
## Name: Elizabeth Lee
## Date: 11/4/16
## Function: create ts for insurance coverage at state level for MISMS talk
## Filenames: 
## Data Source:
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")
rm(list = ls())
#### header #################################
# .Rprofile sets wd 'Dropbox (Bansal Lab)/code' and sources "GeneralTools.R" in .myenv
require(ggplot2)
require(dplyr)
require(readr)
require(RColorBrewer)
require(grid)
require(ggthemes)
setwd(dirname(sys.frame(1)$ofile)) 
setwd("../")
source("source_clean_data_functions.R")

#### import data ################################
insDat <- cleanO_cpsasecInsured_st()

#### plot formatting ################################
w <- 6; h <- 2.5
mar <- seq(0, 4)

#### time series by state ################################
setwd(dirname(sys.frame(1)$ofile)) 
setwd('../../graph_outputs/visuals_MISMS2016')

tsplots <- ggplot(data = insDat , aes(x = year, y = insured*100)) +
  geom_line(aes(group = fips), colour = "grey", size = 1) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) +
  scale_y_continuous(name = "insurance coverage") +
  scale_x_continuous(breaks = 2002:2009) +
  theme_bw(base_size = 14, base_family = "") +
  # theme(panel.grid = element_blank(), plot.margin = unit(mar, "mm"), panel.margin = unit(mar, "mm"), legend.position = "bottom", axis.title.x = element_blank()) 
  theme(panel.grid = element_blank(), plot.margin = unit(mar, "mm"), panel.margin = unit(mar, "mm"), legend.position = "bottom", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) 

# print(tsplots)
ggsave("insured_ts_state_nofacet_crop.png", tsplots, width = w, height = h, dpi = 300)
