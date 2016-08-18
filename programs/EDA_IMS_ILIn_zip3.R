
## Name: Elizabeth Lee
## Date: 8/18/16
## Function: Exploratory analysis to compare downscaled county data to raw ILI data. Plot zip3 choro for ILI per population during rough flu season.
## Filenames: R_export/loess_span0.4_degree2_allZip3Mods_ILin.csv
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr)
require(tidyr)
require(readr)
require(ggplot2)
require(RColorBrewer)
require(scales)
require(maptools)
require(broom)

setwd(dirname(sys.frame(1)$ofile))
source("source_EDA_plots.R")

#### SET THESE! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
dbMetric <- "ILIn" # "iliSum", "iliPeak"
dbLabel <- "ILI per 100,000" # "Seasonal Intensity", "Peak Intensity"

#### import data ################################
# zip3 shapefile
setwd("../reference_data/USzip3_shapefiles_VDSTech")
zip.shp <- readShapeSpatial("zip3_cl", verbose = TRUE, repair = TRUE) 

# zip3 ILIn data
setwd(dirname(sys.frame(1)$ofile))
setwd("../R_export")
path_response_zip3 <- paste0(getwd(), sprintf("/loess_span0.4_degree2_allZip3Mods_ILIn.csv", dbCodeStr))
importDat <- read_csv(path_response_zip3, col_types = "cD___iili_iild__") %>%
  filter(incl.lm) %>%
  filter(month >= 11 | month <= 3) %>%
  mutate(season = ifelse(month >= 11, year-2000+1, year-2000)) %>%
  filter(season != 1) 

dbDat <- importDat %>%
  group_by(season, zip3) %>%
  summarise(ILIn = sum(ILIn, na.rm = TRUE)) %>%
  ungroup %>%
  gather(metric, burden, ILIn) %>%
  rename(id = zip3)

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
choroplots_zip3_1yr(dbDat, zip.shp.df, choroParams, choroplotParams)







