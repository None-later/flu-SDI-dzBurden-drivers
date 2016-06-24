
## Name: Elizabeth Lee
## Date: 5/6/16
## Function: Plot zip3 choro for outcome
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(readr)
require(ggplot2)
require(dplyr)
require(RColorBrewer)
require(scales)
require(broom)

setwd(dirname(sys.frame(1)$ofile))
source("source_EDA_plots.R")

#### SET THESE! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
dbCode <- grep("ili+", strsplit(dbCodeStr, "_")[[1]], value=T)
dbMetric <- "iliPeak" # "iliSum", "iliPeak"
dbLabel <- "Peak Intensity" # "Seasonal Intensity", "Peak Intensity"

#### import data ################################
# zip3 shapefile
setwd("../reference_data/USzip3_shapefiles_VDSTech")
zip.shp <- readShapeSpatial("zip3_cl", verbose = TRUE, repair = TRUE) 

# zip3 disease burden data
setwd(dirname(sys.frame(1)$ofile))
setwd("../R_export")
path_response_zip3 <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB.csv", dbCodeStr))
if (dbMetric == "iliSum"){
  importDat <- read_csv(path_response_zip3, col_types = "icllcd") %>%
    filter(metric == sprintf("%s.sum", dbCode) & season != 1) %>%
    rename(id = zip3)
} else if (dbMetric == "iliPeak"){
  importDat <- read_csv(path_response_zip3, col_types = "icllcd") %>%
    filter(metric == sprintf("%s.peak", dbCode) & season != 1) %>%
    rename(id = zip3)
}
seasList <- importDat %>% distinct(season) %>% arrange(season) %>% unlist
zip.shp.df <- tidy(zip.shp, region = "ZIP")


#### plot formatting ################################
choroParams <- list(spatial = "zip3", code = "dbMetric", lab = dbLabel, src = "IMS", seas = seasList)
choroplotParams <- list(h = 5, w = 8, dp = 300)

#### plot setup ################################
setwd(dirname(sys.frame(1)$ofile))
dir.create(sprintf("../graph_outputs/EDA_IMS_burden_%s_zip3", dbMetric), showWarnings = FALSE)
setwd(sprintf("../graph_outputs/EDA_IMS_burden_%s_zip3", dbMetric))

#### prepare data for plotting ################################
choroplots_zip3_1yr(importDat, zip.shp.df, choroParams, choroplotParams)







