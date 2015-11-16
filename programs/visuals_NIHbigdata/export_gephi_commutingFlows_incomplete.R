## Name: Elizabeth Lee
## Date: 11/2/15
## Function: export data for gephi commuting viz
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

#### import transport data ################################
setwd("../../../../../Sandra Goldlust's Work/Shared_Data/SG_covariate_data/Cleaned_Data")
# income data
transDat <- read_csv("clean_transport_ACS0610.csv", col_types = 'cccciicccc') %>%
  rename(res_st = state_id_residence_2digit) %>%
  rename(res_cty = county_id_residence_3digit) %>%
  rename(wk_st = state_id_workplace_3digit) %>%
  rename(wk_cty = county_id_workplace_3digit) %>%
  mutate(res_fips = paste0(res_st, res_cty)) %>%
  mutate(wk_fips = paste0(substr.Right(wk_st, 2), wk_cty)) %>%
  filter(wk_cty != '000') # filter out ppl who work abroad

#### import population data ################################
setwd(dirname(sys.frame(1)$ofile))
setwd('../../../../Census/Source_Data')
popDat <- read_csv("CO-EST00INT-TOT.csv") %>%
  mutate(cty = substr.Right(paste0("00", COUNTY), 3)) %>% 
  mutate(st = substr.Right(paste0("0", STATE), 2)) %>%
  mutate(fips = paste0(st, cty))

# merge trans and popdat
fullDat <- left_join(transDat, popDat, by = c("wk_fips" = "fips")) 

#### prepare to convert to graph object ################################
addlatlong_base <- fullDat %>% select(state_residence, county_residence) %>%
  unique %>%
  mutate(county_residence = str_replace_all(county_residence, "[^[:alnum:]]", " ")) %>% 
  mutate(st = tolower(state_residence), cty = substring(tolower(county_residence), 1, nchar(county_residence)-7)) %>%
  mutate(jointname = paste(st, cty))

prepDat <- fullDat %>%
  select(res_fips, wk_fips, Number, CENSUS2010POP)

latlong <- map_data("county") %>% 
  mutate(jointname = paste(region, subregion))


