
## Name: Elizabeth Lee
## Date: 10/29/15 
## Function: create zipcode maps with disease burden for NIH poster -- zoomed in area near CA Bay Area (zip3s = 940, 941, 943, 944, 945, 946, 947, 948, 949, 950, 951)
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
require(tidyr)
require(readr)
require(dplyr)
require(zipcode)
require(ggmap)
require(grid)
require(ggplot2)

focusarea <- as.character(940:950)
#### plot params ################################
w <- 10; h <- 10
mar <- c(0,0,0,0)


#### get map ################################
bg.map <- get_map(location = "San Francisco", zoom = 10, maptype = "toner", color = "color")


#### plot map ################################
dir.create('../../graph_outputs/visuals_NIHbigdata', showWarnings = FALSE)
setwd('../../graph_outputs/visuals_NIHbigdata')

geo.map <- ggmap(bg.map) + 
  theme_minimal(base_size = 24, base_family = "") +
  theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), plot.margin = unit(mar, "mm"), panel.margin = unit(mar, "mm"))

ggsave('zip3_CA.png', geo.map, width = w, height = h, dpi = 450)


