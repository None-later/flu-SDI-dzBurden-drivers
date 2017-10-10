
## Name: Elizabeth Lee
## Date: 10/5/17
## Function: examine correlations between ilinDt and irDt v
## Filenames: 
## Data Source: IMS Health
## Notes:
##
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(tidyverse)
require(ggthemes)

setwd(dirname(sys.frame(1)$ofile))
herepath <- getwd()
dir.create("../graph_outputs/explore_dbMetricsRelationships_ilinDt_irDt", showWarnings = FALSE)
setwd("../graph_outputs/explore_dbMetricsRelationships_ilinDt_irDt")
exportPath <- getwd()

setwd(dirname(sys.frame(1)$ofile))
setwd("../../scales/R_export/")
scalespath <- getwd()

#### import data ################################
ilinDt <- read_csv(paste0(herepath, "/../R_export/dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_cty.csv")) %>%
  filter(metric == "ilinDt.sum") %>%
  filter(season %in% 3:9) %>%
  select(fips, season, burden) %>%
  rename(y_ilinDt = burden) %>%
  mutate(y1_ilinDt = log(y_ilinDt+1))

irDt <- read_csv(paste0(scalespath, "/dbMetrics_periodicReg_irDt_Octfit_span0.4_degree2_analyzeDB_cty_dzBurdenMS.csv")) %>% # grab from scales processing
  filter(metric == "irDt.sum") %>%
  filter(season %in% 3:9) %>%
  select(fips, season, burden) %>%
  rename(y_irDt = burden) %>%
  mutate(y1_irDt = log(y_irDt+1))

fullDat <- full_join(ilinDt, irDt, by = c("fips", "season")) %>%
    mutate(season = factor(season, levels = 3:9, labels = c("2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09")))

#### plot iliSum response - burden ################################
# scatterOut <- ggplot(fullDat, aes(x = y_ilinDt, y = y_irDt)) +
#   geom_point(aes(colour = season), alpha = 0.5) +
#   scale_colour_tableau() +
#   theme_bw() +
#   theme(legend.position = "bottom")
# ggsave(paste0(exportPath, "/explore_dbMetricsRelationships_ilinDt_irDt_y.png"), width = 6, height = 4, dpi = 300)


#### plot iliSum response - model input ################################
scatterOut2 <- ggplot(fullDat, aes(x = y1_ilinDt, y = y1_irDt)) +
  geom_point(aes(colour = season), alpha = 0.5) +
  scale_colour_tableau() +
  scale_x_continuous("Original model input") +
  scale_y_continuous("New visits-adjusted measure") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(paste0(exportPath, "/explore_dbMetricsRelationships_ilinDt_irDt_y1.png"), scatterOut2, width = 6, height = 4, dpi = 300)

cor.test(fullDat$y1_ilinDt, fullDat$y1_irDt, method = "spearman")
                                                                               