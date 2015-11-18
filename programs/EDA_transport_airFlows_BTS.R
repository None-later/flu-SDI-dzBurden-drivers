
## Name: Elizabeth Lee
## Date: 11/17/15
## Function: time series and choropleth EDA for air flow into a given airport destination, data by month. Domestic flights only
## Filenames: clean_transport_BTS0014_T100D_Market_All_Carrier.csv, clean_transport_openflights_airportDatabase.csv
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(tidyr)
require(readr)
require(dplyr)
require(ggmap)
require(ggplot2)
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program

#### import data ################################
setwd("../../../../Sandra Goldlust's Work/Shared_Data/SG_covariate_data/Cleaned_Data")
airDat <- read_csv("clean_transport_BTS0014_T100D_Market_All_Carrier.csv", col_types = "ddccccccccii")
locDat <- read_csv("clean_transport_openflights_airportDatabase.csv", col_types = "ccccdd")

setwd(dirname(sys.frame(1)$ofile))
setwd('../../../Census/Source_Data')
popDat <- read_csv("CO-EST00INT-TOT.csv") %>%
  mutate(cty = substr.Right(paste0("00", COUNTY), 3)) %>% 
  mutate(st = substr.Right(paste0("0", STATE), 2)) %>%
  filter(cty == '000') %>% 
  mutate(POPESTIMATE2011 = POPESTIMATE2010, POPESTIMATE2012 = POPESTIMATE2010, POPESTIMATE2013 = POPESTIMATE2010, POPESTIMATE2014 = POPESTIMATE2010) %>%

  select(STNAME, contains("POPESTIMATE"), st) %>%
  rename(st_fips = st)
  

#### plot formatting ################################
num <- 6
w2 <- 9; h2 <- 9; dp <- 300
w <-9; h <- 6

#### clean data ################################
airDat2 <- airDat %>% 
  select(PASSENGERS, DISTANCE, UNIQUE_CARRIER, ORIGIN, ORIGIN_STATE_FIPS, DEST, DEST_STATE_FIPS, YEAR, MONTH) %>%
  filter(PASSENGERS > 0) %>%
  group_by(DEST, YEAR, MONTH) %>%
  summarise(dest_st_fips = max(DEST_STATE_FIPS), pass_to_dest = sum(PASSENGERS), max_incoming_dist = max(DISTANCE)) %>%
  ungroup

popDat2 <- popDat %>%
  gather("var", "stPopEst", 2:16) %>%
  mutate(var = as.character(var)) %>%
  mutate(year = as.numeric(substr.Right(var, 4)))

travDat <- left_join(airDat2, locDat, by = c("DEST" = "IATA_FAA")) %>%
  filter(!is.na(lat))
fullDat <- left_join(travDat, popDat2, by = c("dest_st_fips" = "st_fips", "YEAR" = "year")) %>%
  filter(!is.na(STNAME)) %>%
  select(-var) %>%
  mutate(passNorm_to_dest = pass_to_dest/stPopEst*100000) %>%
  mutate(date = as.Date(paste(YEAR, MONTH, "1", sep = '-')))

uqst <- fullDat %>% ungroup %>% 
  select(STNAME) %>% distinct %>% arrange(STNAME) %>%
  mutate(for.plot = seq_along(1:nrow(.)))
fullDat2 <- fullDat %>% 
  left_join(uqst, by = "STNAME") %>%
  mutate(flu.month = ifelse(MONTH %in% c(11, 12, 1, 2, 3, 4), T, F)) %>%
  mutate(log_pass = log(pass_to_dest), log_pop = log(stPopEst))
indexes <- seq(1, max(fullDat2 %>% select(for.plot) %>% distinct %>% unlist, na.rm=T), by=num)
dates <- fullDat2 %>% select(date) %>% unique %>% arrange
monthnum <- fullDat2 %>% select(MONTH) %>% unique %>% arrange(MONTH) %>% unlist
yearlist <- fullDat2 %>% select(YEAR) %>% unique %>% arrange(YEAR) %>% unlist

#### plotting ################################
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
dir.create(sprintf('../graph_outputs/EDA_transport_airFlows_BTS'), showWarnings=FALSE) # create directory if not exists
setwd('../graph_outputs/EDA_transport_airFlows_BTS')

# #### time series ################################
dir.create('./ts', showWarnings = FALSE)
setwd('./ts')
# for(i in indexes){
#   dplots <- ggplot(fullDat2 %>% filter(for.plot>= i & for.plot < i+num), aes(x=date, y=pass_to_dest)) +
#     theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
#     geom_line(aes(colour = DEST)) +
#     scale_y_continuous(name = "Passengers") +
#     guides(colour = "none") +
#     facet_wrap(~STNAME, scales="free")
#   dplots2 <- ggplot(fullDat2 %>% filter(for.plot>= i & for.plot < i+num), aes(x=date, y=passNorm_to_dest)) +
#     theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
#     geom_line(aes(colour = DEST)) +
#     scale_y_continuous(name = "Passengers per 100K (pop in ST dest)") +
#     guides(colour = "none") +
#     facet_wrap(~STNAME)
#   
#   labs <- fullDat2 %>% filter(for.plot>= i & for.plot < i+num) %>% select(STNAME) %>% distinct %>% slice(c(i, i+num-1))  %>% unlist
#   ggsave(sprintf("passengers_BTS_T100D_destAirport_%s-%s.png", labs[1], labs[2]), dplots, width = w2, height = h2, dpi = dp)
#   ggsave(sprintf("passNorm_BTS_T100D_destAirport_%s-%s.png", labs[1], labs[2]), dplots2, width = w2, height = h2, dpi = dp)
#   
# } # 11/17/15
# 
# #### map ################################
dir.create('../maps', showWarnings = FALSE)
setwd('../maps')
# bg.map <- get_map(location = "United States", zoom = 4, maptype = "roadmap", color = "bw", crop = FALSE)
# 
# for (m in monthnum){
#   mplots <- ggmap(bg.map) + 
#     geom_point(data = fullDat2 %>% filter(MONTH == m), aes(x = lon, y = lat, color = sqrt(pass_to_dest)), size = 1) +
#     scale_color_gradient(low = "#67a9cf", high = "#ef8a62") +
#     theme_minimal() +
#     theme(text = element_text(size = 12), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom", panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#     # coord_cartesian(xlim = c(-124.7844079, -66.9513812), ylim = c(24.7433195, 49.3457868)) +
#     facet_wrap(~YEAR, nrow=3)
#   ggsave(sprintf("passengers_BTS_T100D_destAirport_mo%s.png", m), mplots, width = w, height = h, dpi = dp)
#   
#   mplots2 <- ggmap(bg.map) + 
#     geom_point(data = fullDat2 %>% filter(MONTH == m), aes(x = lon, y = lat, color = sqrt(passNorm_to_dest)), size = 1) +
#     scale_color_gradient(low = "#67a9cf", high = "#ef8a62") +
#     theme_minimal() +
#     theme(text = element_text(size = 12), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom", panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#     # coord_cartesian(xlim = c(-124.7844079, -66.9513812), ylim = c(24.7433195, 49.3457868)) +
#     facet_wrap(~YEAR, nrow=3)
#   ggsave(sprintf("passNorm_BTS_T100D_destAirport_mo%s.png", m), mplots2, width = w, height = h, dpi = dp)
# } # 11/17/15


#### scatter bw access & population variables ################################
dir.create(sprintf('../scatterPop'), showWarnings=FALSE)
setwd('../scatterPop')
for (y in yearlist){
  dummy <- fullDat2 %>% filter(YEAR == y)
  scatters <- ggplot(dummy, aes(x = pass_to_dest,  y = stPopEst)) +
    geom_point(color = 'black') +
    theme_bw(base_size = 12, base_family = "") +
    scale_y_log10(limits = c(1, 30000000)) + scale_x_log10(limits = c(1, 30000000)) +
    # coord_cartesian(xlim = c(1, 30000000), ylim = c(1, 30000000)) +
    facet_wrap(~MONTH, nrow = 3)
  ggsave(sprintf("scatter_transport_airFlows_dest_%s.png", y), scatters, width = w, height = h, dpi = dp)
} # 11/18/15


