
## Name: Elizabeth Lee
## Date: 11/5/15
## Function: visualize time series for percent of positive samples that are H3
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(tidyr)
require(readr)
require(ggplot2)
require(dplyr)
require(grid)
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program

#### import data ################################
setwd("../../../../CDC_Source/Import_Data")
cdcdat <- read_csv("all_cdc_source_data.csv", n_max = 871, col_types = paste0("iiiidiiiiiiii", paste0(rep("_", 25), collapse = ''))) %>%
  mutate(num_pos = num_samples * perc_pos/100) %>%
  mutate(num_H3 = ifelse(is.na(a_H3) & is.na(a_H3N2), NA, a_H3+a_H3N2)) %>%
  filter(wk>=40 | wk<=20)
cdc2 <- cdcdat %>%
  group_by(season) %>%
  summarise(seas_H3 = sum(num_H3, na.rm=T), seas_numPos = sum(num_pos, na.rm=T)) %>%
  mutate(perc_H3 = seas_H3/seas_numPos*100)
           
#### plot formatting ################################
h <- 4.5; w <- 6.5; dp <- 450
mar <- rep(0, 4)
fontsz <- 15
par(mar = mar, oma = mar)
yr1 <- 1997:2013
yr2 <- paste0("-", substr.Right(1998:2014, 2))
seasonlabs <- paste0(yr1, yr2)
labskip <-4

#### plot ################################
setwd(dirname(sys.frame(1)$ofile))
setwd("../../graph_outputs/visuals_NIHbigdata")

H3plt <- ggplot(cdc2, aes(x = season, y = perc_H3)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(name = 'H3 among confirmed flu samples (%)') +
  scale_x_continuous(name = 'Flu Season', breaks = seq(-2, 14, by=labskip), labels = seasonlabs[seq(1, length(seasonlabs), by=labskip)]) +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm")) +
  coord_cartesian(ylim = c(0, 52))
ggsave("H3_season_bars.png", H3plt, width = w, height = h, dpi = dp)
print(H3plt)