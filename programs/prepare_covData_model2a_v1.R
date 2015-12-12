
## Name: Elizabeth Lee
## Date: 12/9/15
## Function: prepare covariate data for input into jags model 2a; seasonal intensity ~ pop density + uninsured pop + impoverished pop; center & standardize covariate data
## Filenames: 
## Data Source: mysql db: Flu-Drivers
## Notes: remember to tunnel into the server first; SAIPE_poverty table: "*_universe" means the population size from which the "*_counts" and "*_percent" are derived
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(ggplot2)
require(DBI); require(RMySQL)
require(readr)
setwd(dirname(sys.frame(1)$ofile)) 

#### connect to database ################################
con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
dbListTables(con)

#### import pop density data ################################
dbListFields(con, "Census_popdensity_county")
# sel.head.popdensity <- "Select * from Census_popdensity_county limit 5" # view data
sel.statement.popdensity <- "SELECT year, fips_st, State, Abbreviation, pop, popDens_land AS popDensity, housDens_land AS housingDensity FROM Census_popdensity_county WHERE type = 'state'"
popdensity <- dbGetQuery(con, sel.statement.popdensity)

#### import uninsured data ################################
dbListFields(con, "HI_CPSasec_state")
# sel.head.uninsured <- "Select * from HI_CPSasec_state limit 5"
sel.statement.uninsured <- "SELECT year, state_id AS fips_st, name AS State, Total*1000 AS pop, Percent_not_covered AS uninsured_perc, Error_percent_not_covered AS uninsured_perc_err FROM HI_CPSasec_state WHERE type = 'state'"
uninsured <- dbGetQuery(con, sel.statement.uninsured)

#### import poverty data ################################
dbListFields(con, "SAIPE_poverty")
# sel.head.poverty <- "SELECT * from SAIPE_poverty limit 5"
sel.statement.poverty <- "SELECT year, state_id AS fips_st, name AS State, all_poverty_universe AS pop, all_poverty_percent AS inPoverty_perc, all_poverty_percentLB90 AS inPoverty_perc_LB90, all_poverty_percentUB90 AS inPoverty_perc_UB90 FROM SAIPE_poverty WHERE type = 'state'"
poverty <- dbGetQuery(con, sel.statement.poverty)

#### disconnect from database ################################
dbDisconnect(con)

#### clean and join covariate data together ################################
dummy1 <- popdensity %>% select(-pop, -State, -Abbreviation) %>% full_join(uninsured %>% select(-pop), by = c("year", "fips_st"))
dummy2 <- dummy1 %>% full_join(poverty %>% select(-State, -pop), by = c("year", "fips_st"))

## popDensity and housingDensity exist only for 2000 and 2010, fill in interim years according to closest year
assign00 <- 2001:2005; assign10 <- 2006:2009
fill_popDensity <- function(cov_data, popdensity_data, CensusYr, AssignYrs){
  popdensCensus <- popdensity %>% filter(year == CensusYr) %>% select(fips_st, popDensity, housingDensity)
  covHalf <- cov_data %>% filter(year %in% AssignYrs) %>%
    select(-popDensity, -housingDensity) %>%
    left_join(popdensCensus, by = "fips_st") %>%
    select(year, fips_st, popDensity, housingDensity, State, uninsured_perc, uninsured_perc_err, inPoverty_perc, inPoverty_perc_LB90, inPoverty_perc_UB90)
  return(covHalf)
}

firsthalf <- fill_popDensity(dummy2, popdensity, 2000, assign00)
secondhalf <- fill_popDensity(dummy2, popdensity, 2010, assign10)
remainder <- dummy2 %>% filter(year %in% c(2000, 2010))

covData <- bind_rows(firsthalf, secondhalf, remainder) %>%
  arrange(year, fips_st) %>% 
  filter(year %in% 2002:2009)


#### add state reference ################################
setwd("../reference_data")
abbrDat <- read_csv("state_abbreviations_FIPS.csv", col_types = "_cc") %>%
  rename(fips_st = FIPS, abbr_st = Abbreviation)
covData2 <- left_join(covData, abbrDat, by = "fips_st")

#### write data to file ################################
setwd("../R_export/jagsModelData_import")
write_csv(covData2, "covData_2a_popDens_pov_insur.csv")
# export 12/12/15
