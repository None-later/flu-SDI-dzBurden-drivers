## Name: Elizabeth Lee
## Date: 3/7/17
## Function: Check Moran's I for state-level seasonal intensity
## Filenames: reference_data/USstate_shapefiles/
## Data Source: 
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

setwd(dirname(sys.frame(1)$ofile))
source("source_clean_response_functions_st.R")
source("source_export_msFigs.R")

#### SET THESE #################################
modCodeStr <- "10a_iliSum_v2-2" # no spatial autocorr
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"

#### FILEPATHS #################################
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_st <- paste0(getwd(), "/state_latlon.csv")

setwd("../R_export")
path_response_st <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_st.csv", dbCodeStr))

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_st = path_latlon_st,
                  path_response_st = path_response_st)

#### CUSTOM FUNCTIONs #################################
# FUNCTION TO REMOVE NA's IN sp DataFrame OBJECT
#   x           sp spatial DataFrame object
#   margin      Remove rows (1) or columns (2) 
sp.na.omit <- function(x, margin=1) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  
  if(margin == 1) {  
    na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
    cat("DELETING ROWS: ", na.index, "\n") 
    return( x[-na.index,]  ) 
  }
  if(margin == 2) {  
    na.index <- unique(as.data.frame(which(all(is.na(x@data)),arr.ind=TRUE))[,margin])
    cat("DELETING COLUMNS: ", na.index, "\n") 
    return( x[,-na.index]  ) 
  }
}

#### import shape data ################################
setwd(dirname(sys.frame(1)$ofile))
setwd("../reference_data/USstate_shapefiles")
st.shp <- readShapeSpatial("gz_2010_us_040_00_500k", verbose = TRUE, repair = TRUE) 
st.shp@data <- st.shp@data[,-c(3,4,5)]

#### examine geometry of shp ################################
# http://gis.stackexchange.com/questions/113964/fixing-orphaned-holes-in-r
# get a report of orphaned hole or geometry validity issues for sp object
report <- clgeo_CollectionReport(st.shp)
summary <- clgeo_SummaryReport(report)
issues <- report[report$valid == FALSE,]
# 

#### examine disconnected subgraphs ################################
# See "Notes and Code for Small Area NYC Pedestrian Injury Spatiotemporal Analyses With INLA" in Mendeley
# Section 1.1 Create the Adjacency Matrix Graph

adjMx <- poly2nb(st.shp) # 
# these region numbers refer to the map and not the nb list object
nolinks <- which(card(adjMx) == 0) # IDs for areas with no links, indexed to nb object: 17, 20, 30
attr(adjMx, "region.id")[which(card(adjMx)==0)]
# e.g. adjMx[[1]] returns a list of neighbors to region 1

st.nolinks <- st.shp[nolinks,]
plot(st.nolinks) # regions with no neighbors
plot(st.shp, col = "red", add = TRUE) # full shapefile

# create spatial polygons frame with only continental US states
haslinks <- which(card(adjMx) > 0)
st.shp.cl <- st.shp[haslinks,]
plot(st.shp.cl, col = "red", add = TRUE)

#### prepare state data for morans I test ################################
# grab order of states from from spatial polygons dataframe
st.order <- as.character(st.shp.cl@data$STATE)

#### import model-related data ################################
# 1) import residuals for modcodestr
# import fitted data (on the scale of log(y))
fitDat <- read_csv(string_fit_fname(modCodeStr), col_types = "__d_c_dd______") %>%
  rename(fit_logy = mean, fit_sd = sd) %>%
  select(season, fips_st, fit_logy, fit_sd)

# 2) import state-level response variable data
respDat <- cleanR_iliSum_shift1_st(path_list) %>%
  filter(fips_st %in% st.order) %>%
  mutate(obs_logy = log(y1), logE = log(E)) %>%
  select(season, fips_st, abbr_st, obs_logy, logE) 

# 3) calculate residuals
residDat <- left_join(respDat, fitDat, by = c("season", "fips_st")) %>%
  mutate(obs_rr = obs_logy-logE, fit_rr = fit_logy-logE) %>%
  mutate(resid = (obs_logy-fit_logy)/fit_sd)

#### conduct morans I test ################################
seasons <- 3:9
for (s in seasons){
  seasDat <- residDat %>% 
    filter(season == s) %>%
    mutate(fips_st = factor(fips_st, levels = st.order)) %>%
    arrange(fips_st) %>%
    mutate(fips_st = as.character(fips_st))
  row.names(seasDat) <- row.names(st.shp.cl@data)
  
  st.shp.cl3 <- spCbind(st.shp.cl, seasDat)
  # st.shp.cl3 <- sp.na.omit(st.shp.cl2, margin = 1)
  st.nb <- poly2nb(st.shp.cl3, queen=FALSE)
  
  mI <- moran.test(st.shp.cl3@data$obs_rr, nb2listw(st.nb), randomisation = TRUE, alternative = "greater", na.action = na.pass)
  print(paste("season", s))
  print(mI)
}
# 

