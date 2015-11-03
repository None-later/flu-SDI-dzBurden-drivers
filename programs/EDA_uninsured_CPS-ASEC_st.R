
## Name: Elizabeth Lee
## Date: 9/22/15
## Function: EDA - IMS Health physician coverage; plot state-level time series and choropleths by year
## Filenames: SQL_export/HI_CPS-ASEC_state_0209.csv
## Data Source: CPS-ASEC
## Notes: SQL_scripts/HI_state_0209.sql
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
setwd('../SQL_export') 
ui_data <- read_csv("HI_CPS-ASEC_state_0209.csv", col_types = list(state_id=col_character()))
data2 <- ui_data %>% mutate(state = tolower(name))

#### plot formatting ################################
w = 9
h = 5

#### choropleth by year ################################
# import choropleth state map from ggplot2
states_map <- map_data("state")
# plot choropleths by year in a single figure
dir.create('../graph_outputs/EDA_uninsured_CPS-ASEC_st', showWarnings = FALSE)
setwd("../graph_outputs/EDA_uninsured_CPS-ASEC_st")

ui.choro <- ggplot(data2, aes(map_id = state, group = year)) +
  geom_map(aes(fill = pctui), map = states_map, color = "black") +
  scale_fill_gradient(name = "Uninsured (%)", low = "light yellow", high = "red", guide = "legend", breaks = seq(4, 26, 3)) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme_minimal() +
  theme(text = element_text(size = 18), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom") +
  facet_wrap(~year, nrow = 2) # + 
  # ggtitle('Percent uninsured')
ggsave("pctui_st_0209.png", width=w, height=h, dpi = 300)
# exported 9/29/15

#### time series by state ################################
ui.ts <- ggplot(data2, aes(x = year, y = pctui, group = name)) +
  theme(axis.text = element_text(size=18), axis.title = element_text(size=16, face="bold")) +
  geom_linerange(aes(ymin = pctui-pctui_moe, ymax = pctui+pctui_moe), colour = 'black', size = 1) +
  geom_line(colour = 'black', size = 1) +
  geom_hline(yintercept = c(20, 10), color = c('red', 'green'), size = 0.5) +
  ylab("Percent uninsured") +
  facet_wrap(~name)
ggsave("pctui_st_AL-WY.png", width=12, height=12, dpi = 300)
# exported 9/29/15

