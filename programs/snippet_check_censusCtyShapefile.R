
## Name: Elizabeth Lee
## Date: 6/8/16
## Function: Examine presence of holes in Census 2010 county shapefile 
## Filenames: reference_data/UScounty_shapefiles/
## Data Source: Census 2010 county shapefiles
## Notes: code adapted from Manuscripts/Spatial_Big_Data_Opinion/plotting_code/zip3_choro_2008.R and Anne/Spatial_clustering/clean_VDSTech_zip3_shp.R
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
require(spdep)

#### import data ################################
setwd(dirname(sys.frame(1)$ofile))
setwd("../reference_data/UScounty_shapefiles")
cty.shp <- readShapeSpatial("gz_2010_us_050_00_500k", verbose = TRUE, repair = TRUE) 

#### examine geometry of shp ################################
# http://gis.stackexchange.com/questions/113964/fixing-orphaned-holes-in-r
# get a report of orphaned hole or geometry validity issues for sp object
report <- clgeo_CollectionReport(cty.shp)
summary <- clgeo_SummaryReport(report)
issues <- report[report$valid == FALSE,]
# 6/8/16 conclusion: there don't appear to be any geometry issues with the shapefile

#### examine disconnected subgraphs ################################
# See "Notes and Code for Small Area NYC Pedestrian Injury Spatiotemporal Analyses With INLA" in Mendeley
# Section 1.1 Create the Adjacency Matrix Graph

adjMx <- poly2nb(cty.shp) # 3221 regions, 9 regions had no links: 68 546 547 549 1226 1876 2976 3148 3202
# these region numbers refer to the map and not the nb list object
nolinks <- which(card(adjMx) == 0) # IDs for areas with no links, indexed to nb object: 69  547  548  550 1227 1877 2977 3149 3203
attr(adjMx, "region.id")[which(card(adjMx)==0)]
# e.g. adjMx[[1]] returns a list of neighbors to region 1

cty.nolinks <- cty.shp[nolinks,]
plot(cty.nolinks) # regions with no neighbors
plot(cty.shp, col = "red", add = TRUE) # full shapefile
# 6/8/18 conclusion: the disconnected components appear to be territories, parts of Alaska, and Hawaii. There are very few disconnected components in the continental US.

#### test ways to subset polygons dataframe to US states ################################
# this appears to work
cty.nolinks <- cty.shp[nolinks,]
test <- poly2nb(cty.nolinks)

# try state-specific syntax
stlist <- c("02", "06") # doesn't work if these are factors
somestates <- cty.shp[cty.shp@data$STATE %in% stlist,]
test2 <- poly2nb(somestates)
plot(somestates)

# try for only continental states
statesOnly <- read_csv("/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/reference_data/state_abbreviations_FIPS.csv", col_types = "__c", col_names = c("stateID"), skip = 1) 
continental <- statesOnly %>% filter(!(stateID %in% c("02", "15"))) %>% unlist
cty.statesonly <- cty.shp[cty.shp@data$STATE %in% continental,]
test3 <- poly2nb(cty.statesonly) # 3 regions with no links: 1226, 1876, 2976
plot(cty.statesonly)
# 6/8/16: remove territories from neighbors list in INLA because they have no neighbors and data is limited: source_prepare_inlaData_cty.R/read_shapefile_cty function

# examine the 3 regions with no links
nolinks <- which(card(test3)==0) # 1193 1843 2943
remaining_nolinks <- cty.shp[nolinks,]
remaining_nolinks@data # York County ME, Niagara County NY, Poquoson city VA
plot(remaining_nolinks)

# It seems that these regions should have neighbors. Check neighbors of some known counties
dc <- which(cty.statesonly@data$STATE == '11')
nb.dc <- test3[[dc]] # dc neighbor map ids
cty.statesonly[nb.dc,]@data
# counties neighboring DC seem appropriate

home <- which (cty.statesonly@data$STATE == '06' & cty.statesonly@data$COUNTY == '001')
nb.home <- test3[[home]]
cty.statesonly[nb.home,]@data
# alameda county neighbors seem appropriate
