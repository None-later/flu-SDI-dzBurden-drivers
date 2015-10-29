
## Name: Elizabeth Lee
## Date: 10/29/15 
## Function: create zipcode maps with disease burden for NIH poster
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

fparams <- list(metric = 'ilicnDt', span = 0.5, degree = 2)

#### plot params ################################
w <- 9; h <- 10
mar <- c(0,0,0,0)


#### read data ################################
setwd('../../R_export/dataSnapshot_NIHbigdata')
db.data <- read_csv(do.call(sprintf, c('dbMetrics_visuals_%s_span%s_degree%s.csv', fparams)), col_types = list('zip3' = col_character()))
coord.data <- read_csv('Coord3digits.csv', col_types = list('zip3' = col_character())) %>%
  mutate(zip3 = substr.Right(paste0('00', zip3), 3)) %>%
  select(zip3, STATE, lat, long, pop)

plot.data <- left_join(db.data, coord.data, by = 'zip3') 
sumplot.data <- plot.data %>%
  filter(metric == 'ilicnDt.sum') %>%
  mutate(burden.discr = cut(burden, breaks = c(seq(0, 60, by = 15), 80, 275))) %>%
  mutate(burden.discr = factor(burden.discr, levels=rev(levels(burden.discr))))
pkplot.data <- plot.data %>% 
  filter(metric == 'ilicnDt.peak') %>%
  mutate(burden.discr = cut(burden, breaks = seq(0, 12, by = 3))) %>%
  mutate(burden.discr = factor(burden.discr, levels = rev(levels(burden.discr))))
  

#### get map ################################
bg.map <- get_map(location = "United States", zoom = 4, maptype = "toner-background", color = "color")

#### plot map ################################
dir.create('../../graph_outputs/visuals_NIHbigdata', showWarnings = FALSE)
setwd('../../graph_outputs/visuals_NIHbigdata')
for (s in 2:9){
  
  # sum ILI plots
  dummy.data <- sumplot.data %>% filter(season == s)
  sum.plot <- ggmap(bg.map) + 
    geom_point(data = dummy.data, aes(x = long, y = lat, fill = burden.discr, size = pop), pch = 21, colour = 'black') +
    scale_fill_brewer(name = 'seasonal burden intensity', palette = 'RdYlBu', guide = 'legend', drop = F) +
    scale_size_continuous(name = 'zip3 population size') +
    theme_minimal(base_size = 16, base_family = "") +
    theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), plot.margin = unit(mar, "mm"), panel.margin = unit(mar, "mm"), legend.position = "bottom")
  
  ggsave(do.call(sprintf, c('%s_sum_span%s_degree%s_S%s.pdf', fparams, s)), sum.plot, width = w, height = h, dpi = 450)
  
  # pk ILI plots
  dummy.data2 <- pkplot.data %>% filter(season == s)
  pk.plot <- ggmap(bg.map) + 
    geom_point(data = dummy.data2, aes(x = long, y = lat, fill = burden.discr, size = pop), pch = 21, colour = 'black') +
    scale_fill_brewer(name = 'peak burden intensity', palette = 'RdYlBu', guide = 'legend', drop = F) +
    scale_size_continuous(name = 'zip3 population size') +
    theme_minimal(base_size = 16, base_family = "") +
    theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), plot.margin = unit(mar, "mm"), panel.margin = unit(mar, "mm"), legend.position = "bottom")
  
  ggsave(do.call(sprintf, c('%s_peak_span%s_degree%s_S%s.pdf', fparams, s)), pk.plot, width = w, height = h, dpi = 450)
}

