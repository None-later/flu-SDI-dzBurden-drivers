
## Name: Elizabeth Lee
## Date: 6/24/16
## Function: Write clean reference zip3 shapefile based on VDS Tech shapefile
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
require(tidyr)
require(RColorBrewer)
require(maptools)
require(scales)
require(broom)
require(rgeos)
require(cleangeo)

#### import data ################################
setwd(dirname(sys.frame(1)$ofile))
setwd("../reference_data/USzip3_shapefiles_VDSTech")
zip.shp <- readShapeSpatial("zip3", verbose = TRUE, repair = TRUE) 

#### clean shp ################################
# http://gis.stackexchange.com/questions/113964/fixing-orphaned-holes-in-r
# get a report of orphaned hole or geometry validity issues for sp object
report <- clgeo_CollectionReport(zip.shp)
summary <- clgeo_SummaryReport(report)
issues <- report[report$valid == FALSE,]
# get indexes that have orphaned holes
nv <- clgeo_SuspiciousFeatures(report)
toclean <- zip.shp[nv,]
# try to clean orphaned holes
sub.shp.cl <- clgeo_Clean(toclean, print.log = TRUE)
# check if errors remain
report.clean <- clgeo_CollectionReport(sub.shp.cl)
summary.clean <- clgeo_SummaryReport(report.clean)

# clean the entire sp object
zip.shp2 <- clgeo_Clean(zip.shp, print.log = TRUE)
report.clean <- clgeo_CollectionReport(zip.shp2)
summary.clean <- clgeo_SummaryReport(report.clean)

# write new zip3 shapefile
writeSpatialShape(zip.shp2, "zip3_cl")

