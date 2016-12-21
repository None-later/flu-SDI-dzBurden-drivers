
## Name: Elizabeth Lee
## Date: 12/19/16
## Function: create adjacency matrix for state flight neighbors
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
popDat <- clean_pop_st_plain() %>%
  filter(year == 2008) %>%
  select(fips_st, pop)

#### import flight data ################################
con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
  dbListTables(con)
  
  dbListFields(con, "transport_BTS0014_T100D_Market_All_Carrier")
  # sel.statement <- "SELECT * from transport_BTS0014_T100D_Market_All_Carrier limit 5"
  sel.statement <- "SELECT PASSENGERS as pass, ORIGIN, ORIGIN_STATE_FIPS as fips_origin, DEST, DEST_STATE_FIPS as fips_dest, YEAR as year, MONTH as month from transport_BTS0014_T100D_Market_All_Carrier where (PASSENGERS > 0 and (MONTH <= 4 or MONTH >= 11))"
  origDat <- dbGetQuery(con, sel.statement)
  
dbDisconnect(con)

#### clean flight data ################################
rmSt <- c("72", "75", "78", "02", "15")

fDat1 <- tbl_df(origDat) %>% 
  mutate(season = ifelse(month <= 4, as.integer(substr.Right(year, 2)), as.integer(substr.Right(year, 2))+1)) %>%
  filter(season == 8) %>%
  filter(fips_origin != fips_dest) %>%
  filter(!(fips_origin %in% rmSt) & !(fips_dest %in% rmSt)) %>%  
  group_by(fips_dest, ORIGIN) %>%
  summarise(pass = sum(pass, na.rm = TRUE), fips_origin = first(fips_origin)) %>%
  ungroup %>%
  group_by(fips_origin, fips_dest) %>% 
  summarise(pass = sum(pass, na.rm = TRUE)) %>% 
  ungroup

#### merge flight and pop data ################################
fullDat <- full_join(fDat1, fDat1, by = c("fips_origin" = "fips_dest", "fips_dest" = "fips_origin")) %>%
  rename(fips1 = fips_origin, fips2 = fips_dest) %>% 
  rename(pass_12 = pass.x, pass_21 = pass.y) %>%
  left_join(popDat, by = c("fips1" = "fips_st")) %>%
  rename(pop1 = pop) %>%
  left_join(popDat, by = c("fips2" = "fips_st")) %>%
  rename(pop2 = pop) %>%
  mutate(pass_12 = ifelse(is.na(pass_12), 0, pass_12)) %>%
  mutate(pass_21 = ifelse(is.na(pass_21), 0, pass_21))


#### explore commuting patterns ################################
fullDat2 <- fullDat %>%
  mutate(fTot = pass_12+pass_21) %>%
  mutate(fTot_norm = fTot/(pop1+pop2)) %>%
  mutate(fDiff = abs(pass_12-pass_21)) %>%
  mutate(fDiff_norm = fDiff/(pop1+pop2))

hist(fullDat2$fTot, breaks=10000)
hist(fullDat2$fTot_norm, xlim = c(0, 0.01), breaks=10000)
hist(fullDat2$fDiff, breaks=300)
hist(fullDat2$fDiff_norm, breaks=300)

#### look at only top 30% of flight passenger connections bw states ################################
quantile(fullDat2$fTot, seq(0, 1, by = 0.05))
fullDat2 <- fullDat2 %>%
  filter(fTot >= quantile(fTot, 0.70)) # connects states with > 1000 passengers traveling between them on an average flu season day

#### remove duplicate edges in prep for export ################################
fipsIDs <- fullDat2 %>% 
  distinct(fips1) %>% 
  arrange(fips1) %>%
  mutate(graphIdx_st = seq_along(fips1)) %>%
  rename(fips_st = fips1)

fullDat3 <- fullDat2 %>% 
  left_join(fipsIDs, by = c("fips1"="fips_st")) %>%
  rename(id1 = graphIdx_st) %>%
  left_join(fipsIDs, by = c("fips2"="fips_st")) %>%
  rename(id2 = graphIdx_st)

edgels <- as.matrix(fullDat3 %>% select(id1, id2))
g <- graph_from_edgelist(edgels, directed = FALSE)
g_cl <- simplify(g)
count_components(g_cl)
deg_cl <- degree(g_cl, mode = "all")
hist(deg_cl)

#### write graph & ids to file ################################
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
setwd("../reference_data/stateFlightpassenger_graph/")

# write graph to file
path_graph <- paste0(getwd(), "/US_statePassenger_edgelist.txt")
write_graph(g_cl, path_graph, format = "edgelist")

# write fips and graph Ids to file
path_idCrosswalk <- paste0(getwd(), "/US_statePassenger_graph_index.csv")
write_csv(fipsIDs, path_idCrosswalk)

path_full <- paste0(getwd(), "/US_statePassenger_fullData.csv")
write_csv(fullDat3, path_full)
# exported 12/21/16

#### test graph object ################################
f_adjMx <- as_adjacency_matrix(g_cl, type = "both")

