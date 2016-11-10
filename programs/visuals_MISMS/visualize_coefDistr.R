
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
modCodeStr <- "7a_iliSum_v5-1"

#### functions #################################
plot_coefDistr_RV <- function(plotDat, path_plotExport_coefDistr){
  # plot all coef mean & 95%CI over RV
  print(match.call())
  
  # plot formatting
  w <- 8; h <- 3; dp <- 250
  
  # plot fixed effects
  plotOutput <- ggplot(plotDat, aes(x = RV, y = mean)) +
    geom_pointrange(aes(ymin = LB, ymax = UB, colour = signif)) +
    geom_hline(yintercept = 0) +
    scale_y_continuous("posterior mean (95%CI)") +
    scale_colour_manual(limits = c(TRUE, FALSE), values = c("red", "#000080")) +
    guides(colour = FALSE) +
    theme_bw() + 
    theme(axis.title.x=element_blank(), axis.text.x=element_text(angle=45, vjust=1, hjust=1), axis.text=element_text(size=12))
  ggsave(path_plotExport_coefDistr, plotOutput, height = h, width = w, dpi = dp)
  
}
#################################

plot_coefDistr_RV_labels <- function(plotDat, path_plotExport_coefDistr){
  # plot all coef mean & 95%CI over RV
  print(match.call())
  
  # plot formatting
  w <- 8; h <- 4; dp <- 250
  
  # plot fixed effects
  plotOutput <- ggplot(plotDat, aes(x = RV, y = mean)) +
    geom_pointrange(aes(ymin = LB, ymax = UB, colour = signif)) +
    geom_hline(yintercept = 0) +
    scale_y_continuous("posterior mean (95%CI)") +
    scale_colour_manual(limits = c(TRUE, FALSE), values = c("red", "#000080")) +
    guides(colour = FALSE) +
    theme_bw() + 
    xlab("<------------------------- ecological mechanisms -------------------------><-- capture effects -->") +
    theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1), axis.text=element_text(size=12))
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
measNames <- c("insurance coverage", "care-seeking", "med claims coverage")

fullList <- c(ecolList, measList)
fullNames <- c(ecolNames, measNames)

coefDf <- fullDf %>%
  filter(effectType == 'fixed') %>%
  clean_RVnames(.) %>%
  mutate(LB = mean-(2*sd), UB = mean+(2*sd)) %>%
  mutate(signif = ifelse(UB < 0 | LB > 0, TRUE, FALSE))

ecolDat <- coefDf %>%
  filter(RV %in% ecolList) %>%
  mutate(RV = factor(RV, levels = ecolList, labels = ecolNames))

fullDat <- coefDf %>%
  filter(RV %in% fullList) %>%
  mutate(RV = factor(RV, levels = fullList, labels = fullNames))

#### export plots #################################
setwd(dirname(sys.frame(1)$ofile))
setwd("../../graph_outputs/visuals_MISMS2016")
path_plotExport <- getwd()

# coefficients: ecological
path_plotExport_ecol <- paste0(path_plotExport, sprintf("/MISMS_ecol_%s.png", modCodeStr))
plot_coefDistr_RV(ecolDat, path_plotExport_ecol)

# coefficients: full
path_plotExport_full <- paste0(path_plotExport, sprintf("/MISMS_full_%s.png", modCodeStr))
plot_coefDistr_RV_labels(fullDat, path_plotExport_full)

