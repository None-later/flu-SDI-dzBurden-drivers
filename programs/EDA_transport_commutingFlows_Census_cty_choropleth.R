
## Name: Elizabeth Lee
## Date: 
## Function: 
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(tidyr)
require(ggplot2)
require(dplyr)
require(choroplethrMaps)
  
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program

#### read data ################################
setwd("../../../../Sandra Goldlust's Work/Shared_Data/SG_covariate_data/Cleaned_Data")
str <- c(rep("c", 10), "i", rep("_", 9))
transDat00 <- read_csv("clean_transport_Census00.csv", col_types = paste0(str, collapse=''))
str <- c(rep("c", 10), "icc", rep("_", 27))
transDat90 <- read_csv("clean_transport_Census90.csv", col_types = paste0(str, collapse=''), na = '\\N')

setwd(dirname(sys.frame(1)$ofile))
setwd('../../../Census/Source_Data')
popDat <- read_csv("CO-EST00INT-TOT.csv") %>%
  mutate(cty = substr.Right(paste0("00", COUNTY), 3)) %>% 
  mutate(st = substr.Right(paste0("0", STATE), 2)) %>%
  mutate(fips = paste0(st, cty)) %>%
  select(fips, POPESTIMATE2000)

#### data cleaning ################################
processDat <- function(transData){
  fullDat <- transData %>%
    mutate(Res_fips = paste0(Res_ST, Res_CO), Wrk_fips = substr.Right(paste0(Wrk_ST, Wrk_CO), 5)) %>%
    filter(substring(Wrk_ST, 1, 1) == '0') %>%
    group_by(Wrk_fips) %>%
    summarise(rawvalue = sum(Count, na.rm=T)) %>%
    left_join(popDat, by = c("Wrk_fips" = "fips"))  %>% 
    mutate(region = as.numeric(Wrk_fips)) %>%
    mutate(normvalue = rawvalue/POPESTIMATE2000*1000)
  return(fullDat)
}

fullDat00 <- processDat(transDat00)
fullDat90 <- processDat(transDat90)

#### plot formatting ################################
h <- 5; w <- 8; dp <- 300

# #### choropleth ################################
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
dir.create(sprintf('../graph_outputs/EDA_transport_commutingFlows_Census_cty'), showWarnings=FALSE) # create directory if not exists
setwd('../graph_outputs/EDA_transport_commutingFlows_Census_cty')

choro00 <- county_choropleth(fullDat00 %>% rename(value = normvalue), legend = "Into Wk county per 1000") 
ggsave("commutingInflowsNorm_Census_2000_cty.png", choro00, width = w, height = h, dpi = dp)
choro00r <- county_choropleth(fullDat00 %>% rename(value = rawvalue), legend = "Into Wk county") 
ggsave("commutingInflows_Census_2000_cty.png", choro00r, width = w, height = h, dpi = dp)

choro90 <- county_choropleth(fullDat90 %>% rename(value = normvalue), legend = "Into Wk county per 1000") 
ggsave("commutingInflowsNorm_Census_1990_cty.png", choro90, width = w, height = h, dpi = dp)
choro90r <- county_choropleth(fullDat00 %>% rename(value = rawvalue), legend = "Into Wk county") 
ggsave("commutingInflows_Census_1990_cty.png", choro90r, width = w, height = h, dpi = dp)

