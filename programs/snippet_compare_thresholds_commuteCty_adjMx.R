
## Name: Elizabeth Lee
## Date: 12/21/16
## Function: compare thresholds for adjustments to county commuting neighbors adjacency matrix; forked from reference_commuteCty_adjMx.R
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

#### test thresholds ################################
# 50% quantile
fullDat3 <- fullDat2 %>% 
  filter(commTot >= quantile(commTot, .7))

#### remove duplicate edges in prep for export ################################
fipsIDs <- fullDat3 %>%
  distinct(fips1) %>%
  arrange(fips1) %>%
  mutate(graphIdx = seq_along(fips1)) %>%
  rename(fips = fips1)

fullDat4 <- fullDat3 %>%
  left_join(fipsIDs, by = c("fips1"="fips")) %>%
  rename(id1 = graphIdx) %>%
  left_join(fipsIDs, by = c("fips2"="fips")) %>%
  rename(id2 = graphIdx)

#### check number of connected components ################################
edgels <- as.matrix(fullDat4 %>% select(fips1, fips2))
g <- graph_from_edgelist(edgels, directed = FALSE)
count_components(g) # 1 connected component
deg <- degree(g, mode = "all")

summDeg <- fullDat4 %>% group_by(fips1) %>% count
mean(summDeg$n) # undirected mean degree 31.72

g_cl <- simplify(g)
count_components(g_cl)
deg_cl <- degree(g_cl, mode = "all")
hist(deg_cl)
mean(deg_cl)

# checks
which(deg_cl == max(deg_cl)) # 48201 at 563 edgies
sort(neighbors(g_cl, '48201'))
neighbors(g_cl, '48453')

# 12/17/16
# I compared network connections after taking the top 50% and top 30% of commuter flows (by traveler) across the entire dataset. Even after subsetting the data in this way, there were many non-adjacent cross-state commutes for many counties.
# The ACS survey question asks respondents for their principal workplace location during a given reference week (by greatest number of hours spent). It's possible that many workers are based in a home state but travel elsewhere for work during the week.
# Conclusion: This county commuting data presents much more cross-county connectivity than "close-to-home" commutes