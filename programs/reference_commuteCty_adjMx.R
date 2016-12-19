
## Name: Elizabeth Lee
## Date: 12/18/16
## Function: create adjacency matrix for county commuting neighbors
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(dplyr); require(tidyr); require(ggplot2); require(readr); require(DBI); require(RMySQL)
require(igraph); require(spdep)

setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
source("source_clean_response_functions_cty.R")

#### import pop data ################################
popDat <- clean_pop_cty_plain() %>%
  filter(year == 2008) %>%
  select(fips, pop)

#### import commuting data ################################
con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
dbListTables(con)

# mysql data from ACS 2006-10
dbListFields(con, "transport_ACS0610_iconv")
# sel.statement <- "SELECT * from transport_ACS0610_iconv limit 5"
sel.statement0610 <- "SELECT county_id_residence_3digit, county_id_workplace_3digit, state_id_residence_2digit, state_id_workplace_3digit, Number FROM transport_ACS0610_iconv"
origDat <- dbGetQuery(con, sel.statement0610)

dbDisconnect(con)

#### clean commuting data ################################
commDat1 <- tbl_df(origDat) %>% 
  mutate(domesticWork = ifelse(county_id_workplace_3digit != "000", TRUE, FALSE)) %>% # work out of US
  filter(domesticWork) %>%
  mutate(fips_wrk = substr.Right(paste0(state_id_workplace_3digit, county_id_workplace_3digit), 5)) %>%
  mutate(fips_res = substr.Right(paste0(state_id_residence_2digit, county_id_residence_3digit), 5)) %>% # substr.Right should be redundant
  filter(fips_res != fips_wrk) %>%
  filter(state_id_residence_2digit != '72' & state_id_workplace_3digit != '072') %>% # rm Puerto Rico data
  select(fips_res, fips_wrk, Number) %>%
  rename(commuters = Number)

#### merge commuting and pop data ################################
fullDat <- full_join(commDat1, commDat1, by = c("fips_res" = "fips_wrk", "fips_wrk" = "fips_res")) %>%
  rename(fips1 = fips_res, fips2 = fips_wrk) %>% 
  rename(commuters_12 = commuters.x, commuters_21 = commuters.y) %>%
  left_join(popDat, by = c("fips1" = "fips")) %>%
  rename(pop1 = pop) %>%
  left_join(popDat, by = c("fips2" = "fips")) %>%
  rename(pop2 = pop) %>%
  mutate(commuters_12 = ifelse(is.na(commuters_12), 0, commuters_12)) %>%
  mutate(commuters_21 = ifelse(is.na(commuters_21), 0, commuters_21))


#### explore commuting patterns ################################
fullDat2 <- fullDat %>%
  mutate(commTot = commuters_12+commuters_21) %>%
  mutate(commTot_norm = commTot/(pop1+pop2)) %>%
  mutate(commDiff = abs(commuters_12-commuters_21)) %>%
  mutate(commDiff_norm = commDiff/(pop1+pop2))

hist(fullDat2$commTot, breaks=10000)
hist(fullDat2$commTot_norm, xlim = c(0, 0.01), breaks=10000)
hist(fullDat2$commDiff, breaks=300)
hist(fullDat2$commDiff_norm, breaks=300)

#### check number of connected components ################################
edgels <- as.matrix(fullDat2 %>% select(fips1, fips2))
g <- graph_from_edgelist(edgels, directed = FALSE)
count_components(g) # 1 connected component
deg <- degree(g, mode = "all")
hist(deg/2) # degree distribution
mean(deg/2) # undirected mean degree 62.88

summDeg <- fullDat2 %>% group_by(fips1) %>% count
mean(summDeg$n) # undirected mean degree 62.88

#### remove duplicate edges in prep for export ################################
fipsIDs <- fullDat2 %>% 
  distinct(fips1) %>% 
  arrange(fips1) %>%
  mutate(graphIdx = seq_along(fips1)) %>%
  rename(fips = fips1)

fullDat3 <- fullDat2 %>% 
  left_join(fipsIDs, by = c("fips1"="fips")) %>%
  rename(id1 = graphIdx) %>%
  left_join(fipsIDs, by = c("fips2"="fips")) %>%
  rename(id2 = graphIdx)

edgels <- as.matrix(fullDat3 %>% select(id1, id2))
g <- graph_from_edgelist(edgels, directed = FALSE)
g_cl <- simplify(g)
count_components(g_cl)

#### write graph & ids to file ################################
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
setwd("../reference_data/countyCommuter_graph/")

# write graph to file
path_graph <- paste0(getwd(), "/US_countyCommuter_edgelist.txt")
write_graph(g_cl, path_graph, format = "edgelist")

# write fips and graph Ids to file
path_idCrosswalk <- paste0(getwd(), "/US_countyCommuter_graph_index.csv")
write_csv(fipsIDs, path_idCrosswalk)

path_full <- paste0(getwd(), "/US_countyCommuter_fullData.csv")
write_csv(fullDat3, path_full)
# exported 12/19/16

#### test graph object ################################
comm_adjMx <- as_adjacency_matrix(g_cl, type = "both")

