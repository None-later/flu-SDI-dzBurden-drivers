
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

#### set path lists ################################
setwd('../reference_data')
path_pop_st <- paste0(getwd(), "/pop_st_Census_00-10.csv")
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")

path_list <- list(path_pop_st = path_pop_st,
                  path_abbr_st = path_abbr_st,
                  path_latlon_st = path_latlon_st)

setwd(dirname(sys.frame(1)$ofile)) 
setwd("../../R_export")
path_imsCov_st <- paste0(getwd(), "/physicianCoverage_IMSHealth_state.csv")

#### import data ################################
covDat <- read_csv(path_imsCov_st)
popDat <- clean_pop_st(path_list)
fullDat <- left_join(covDat, popDat, by = c("year", "state"="abbr")) %>%
  mutate(careseek = sampViz/pop)

#### plot formatting ################################
w <- 6; h <- 2.5
mar <- seq(0, 4)

#### time series by state ################################
setwd(dirname(sys.frame(1)$ofile)) 
setwd('../../graph_outputs/visuals_MISMS2016')

tsplots <- ggplot(data = fullDat , aes(x = year, y = careseek)) +
  geom_line(aes(group = fips), colour = "grey", size = 1) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) +
  scale_y_continuous(name = "care seeking") +
  scale_x_continuous(breaks = 2002:2009) +
  theme_bw(base_size = 14, base_family = "") +
  # theme(panel.grid = element_blank(), plot.margin = unit(mar, "mm"), panel.margin = unit(mar, "mm"), legend.position = "bottom", axis.title.x = element_blank()) 
  theme(panel.grid = element_blank(), plot.margin = unit(mar, "mm"), panel.margin = unit(mar, "mm"), legend.position = "bottom", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) 

# print(tsplots)
ggsave("careseek_ts_state_nofacet_crop.png", tsplots, width = w, height = h, dpi = 300)
