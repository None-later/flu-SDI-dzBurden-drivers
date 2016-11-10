
## Name: Elizabeth Lee
## Date: 9/8/16
## Function: Choropleth map of 2007-08 data for yObs -- for ISDS abstract
## Filenames: physicianCoverage_IMSHealth_state.csv, dbMetrics_periodicReg_ilinDt_Octfit_span0.4_degree2_analyzeDB_st.csv
## Data Source: IMS Health
## Notes: V3: multi-region outputs
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(readr) # clean_data_functions dependencies
require(RColorBrewer); require(ggplot2); require(ggcounty); require(scales); require(classInt)
setwd(dirname(sys.frame(1)$ofile))
setwd("../")
source("source_export_inlaDiagnostics.R")

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStr <- "6a_iliSum_v6-1"

#### functions #################################
plot_coefDistr_season <- function(plotDat, path_plotExport_coefDistr){
  # plot all coef mean & 95%CI over time
  print(match.call())
  
  # plot formatting
  w <- 6; h <- 8; dp <- 250
  
  # plot fixed effects
  plotOutput <- ggplot(plotDat, aes(x = season, y = mean, group = RV)) +
    geom_pointrange(aes(ymin = LB, ymax = UB, colour = signif)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~RV, nrow = 3, scales = "free_y") +
    scale_y_continuous("posterior mean (95%CI)") +
    scale_colour_manual(limits = c(TRUE, FALSE), values = c("red", "#000080")) +
    guides(colour = FALSE) +
    theme_bw() +
    theme(axis.title.x=element_blank(), text=element_text(size=14), axis.text.x=element_text(angle=45, vjust=1, hjust=1))
  ggsave(path_plotExport_coefDistr, plotOutput, height = h, width = w, dpi = dp)
  
}

#### import data #################################
setwd(dirname(sys.frame(1)$ofile))
setwd(sprintf("../../R_export/inlaModelData_export/%s", modCodeStr))
path_csvExport <- getwd()

# grab list of files names
setwd(path_csvExport)
readfile_list <- grep("summaryStats_", list.files(), value = TRUE)
fullDf <- tbl_df(data.frame(modCodeStr = c(), dbCodeStr = c(), season = c(), RV = c(), effectType = c(), likelihood = c(), mean = c(), sd = c(), q_025 = c(), q_5 = c(), q_975 = c()))

# bind to single dataset
for (infile in readfile_list){
  seasFile <- read_csv(infile, col_types = "ccd_cccddddd__")
  fullDf <- bind_rows(fullDf, seasFile)
}

#### clean data #################################
ecolList <- c("humidity", "child", "adult", "poverty", "hospaccess", "popdensity", "flight", "housdensity", "vaxcovI", "vaxcovE", "H3")
ecolNames <- c("humidity", "child pop", "adult pop", "poverty", "hospitals", "pop density", "air travel", "household size", "infant vacc", "elderly vacc", "H3 circulation")

measList <- c("insured", "careseek", "imscoverage")
measNames <- c("insurance coverage", "care-seeking", "medical claims coverage")

seasList <- as.character(2:9)
seasNames <- c("2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09")

coefDf <- fullDf %>%
  filter(effectType == 'fixed') %>%
  clean_RVnames(.) %>%
  mutate(LB = mean-(2*sd), UB = mean+(2*sd)) %>%
  mutate(signif = ifelse(UB < 0 | LB > 0, TRUE, FALSE)) %>%
  mutate(season = factor(as.character(season), levels = seasList, labels = seasNames))

ecolDat <- coefDf %>%
  filter(RV %in% ecolList) %>%
  mutate(RV = factor(RV, levels = ecolList, labels = ecolNames))

measDat <- coefDf %>%
  filter(RV %in% measList) %>%
  mutate(RV = factor(RV, levels = measList, labels = measNames))

#### export plots #################################
setwd(dirname(sys.frame(1)$ofile))
setwd("../../graph_outputs/visuals_MISMS2016")
path_plotExport <- getwd()

# coefficients: ecological
path_plotExport_meas <- paste0(path_plotExport, sprintf("/MISMS_meas_season_%s.png", modCodeStr))
plot_coefDistr_season(measDat, path_plotExport_meas)



