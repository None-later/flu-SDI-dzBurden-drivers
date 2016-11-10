
## Name: Elizabeth Lee
## Date: 10/29/15
## Function: create ts for physician coverage for NIH poster
## Filenames: R_export/physicianCoverage_IMSHealth_state.csv
## Data Source: IMS Health (Farid Khan)
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
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program

fparams <- list(metric = 'ilicnDt', span = 0.5, degree = 2)
#### import data ################################
setwd('../../R_export/dataSnapshot_NIHbigdata')
geo.data <- read_csv(do.call(sprintf, c("zip3StIncl_coords_%s_span%s_degree%s.csv", fparams)), col_types = list("zip3" = col_character()))

data2 <- read_csv("physicianCoverage_IMSHealth_state.csv") %>%
 filter(state %in% geo.data$abbr)
data3 <- left_join(data2 %>% select(-for.plot), geo.data %>% select(abbr, fullstate, hhsregion) %>% unique, by = c("state" = "abbr")) %>%
  mutate(hhslab = paste("Region", hhsregion)) %>%
  mutate(fullstate = ifelse(fullstate == "District of Columbia", "D.C.", fullstate))
st.lvls <- data3 %>% 
  arrange(hhsregion, fullstate) %>%
  select(fullstate) %>%
  unique
data4 <- data3 %>%
  mutate(fullstate.fac = factor(fullstate, levels = st.lvls$fullstate))

#### plot formatting ################################
w <- 6; h <- 2.7
mar <- seq(0, 4)

#### time series by state ################################
dir.create('../../graph_outputs/visuals_MISMS2016', showWarnings = FALSE)
setwd('../../graph_outputs/visuals_MISMS2016')

tsplots <- ggplot(data = data4 , aes(x = year, y = covAdjProv*100)) +
  geom_line(aes(group = fullstate.fac), colour = "grey", size = 1) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) +
  scale_y_continuous(name = "medical claims coverage") +
  scale_x_continuous(breaks = 2002:2009) +
  theme_bw(base_size = 14, base_family = "") +
  theme(panel.grid = element_blank(), plot.margin = unit(mar, "mm"), panel.margin = unit(mar, "mm"), legend.position = "bottom", axis.title.x = element_blank()) 
  # guides(colour = FALSE) +
  # facet_wrap(~fullstate.fac)
# print(tsplots)
ggsave("physCov_ts_state_nofacet.png", tsplots, width = w, height = h, dpi = 300)
