## Name: Elizabeth Lee
## Date: 11/2/15
## Function: export air traffic flow data for jflowmap visualization
### disease burden metrics: total ili summed across epidemic weeks, cumulative difference in ili and baseline, cumulative difference in ili and epidemic threshold, rate of ili at epidemic peak, epidemic duration, weeks from Nov1 to epidemic start, weeks from epidemic start to peak ili week 


## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")
rm(list = ls())
#### header ####################################
require(dplyr)
require(readr)
require(igraph)
require(ggplot2)
require(stringr)
setwd(dirname(sys.frame(1)$ofile))

#### params ################################
set.seed(11)
numMets <- 50

#### import transport data ################################
setwd("../../covariate_data/transport_airtrafficMetroID_network")
airdat  <- read_delim("air_traffic_edgelist.txt", delim = '\t', col_names = FALSE) %>%
  rename(Met1 = X1, Met2 = X2, avgPass = X3)

uqMets <- airdat %>% select(Met1) %>% unique %>%
  filter(Met1 != 105) %>% unlist 
sampMets <- sample(uqMets, numMets, replace = FALSE) %>% sort

airdat.samp <- airdat %>% filter(Met1 %in% sampMets & Met2 %in% sampMets)

#### import latlong data ################################
coord <- read_delim("masterairmetrocodes.txt", delim = '\t', col_names = FALSE, n_max = 379) 
coord2 <- coord %>% 
  rename(Met = X4, Lat = X6, Lon = X7, MSA = X3)  %>%
  mutate(MSA = substring(MSA, 1, nchar(MSA)-4)) %>%
  select(Met, MSA, Lat, Lon) %>%
  filter(Met %in% sampMets)
coord3 <- coord2[!duplicated(coord2["Met"]),]

setwd(dirname(sys.frame(1)$ofile))
setwd("./jflowmap/input_files")
write_csv(airdat.samp, "airtrafficSampleFlows.csv")
write_csv(coord3, "metidCoords.csv")

