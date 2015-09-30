
## Name: Elizabeth Lee
## Date: 9/22/15
## Function: EDA - IMS Health physician coverage; plot state-level choropleths by year
## Filenames: R_export/physicianCoverage_IMSHealth_state.csv
## Data Source: IMS Health (Farid Khan)
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
# .Rprofile sets wd 'Dropbox (Bansal Lab)/code' and sources "GeneralTools.R" in .myenv
require(ggplot2)
require(dplyr)
require(readr)
require(RColorBrewer)
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program

#### import data ################################
setwd('../reference_data')
abbr.data <- read.csv("state_abbreviations_FIPS.csv", header=T, colClasses='character')
abbr.data$State <- tolower(abbr.data$State)

setwd('../R_export') # example with relative file paths
data <- read_csv("physicianCoverage_IMSHealth_state.csv")
data2 <- left_join(data, abbr.data, by=c("state" = "Abbreviation"))

#### plot formatting ################################
w = 9
h = 6

#### choropleth by year ################################
# import choropleth state map from ggplot2
states_map <- map_data("state")
# plot choropleths by year in a single figure
setwd('../graph_outputs/EDA_physicianCoverage_IMSHealth')
cov.choro <- ggplot(data2, aes(map_id = State, group = year)) +
  geom_map(aes(fill = covAdjProv), map = states_map, color = "black") +
  scale_fill_gradient(low = "light yellow", high = "red", guide = "legend", breaks = seq(0, 0.5, 0.05)) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme_minimal() +
  theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank()) +
  facet_wrap(~year) + 
  ggtitle('Effective Physician Coverage during Flu Season')
ggsave("adjPhysCov_st_0209.png", width=w, height=h)
# exported 9/22/15

