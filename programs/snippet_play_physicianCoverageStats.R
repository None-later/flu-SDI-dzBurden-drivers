# 4/10/17


require(DBI); require(RMySQL)
require(dplyr); require(tidyr); require(readr); require(ggplot2)
setwd(dirname(sys.frame(1)$ofile))
source("source_clean_data_functions.R")
################################

cw <- cw_zip3_cty() 
popDat <- clean_pop_cty_plain()

# import physician coverage data
con <- dbConnect(RMySQL::MySQL(), group = "rmysql-fludrivers")
dbListTables(con)

dbListFields(con, "IMS_physicianCoverage_zip3")
# sel.statement <- "Select * from IMS_physicianCoverage_zip3 limit 5"
sel.statement <- "SELECT year, zip3, adjProviderCoverage, sampViz, sampProv FROM IMS_physicianCoverage_zip3"
dummy <- dbGetQuery(con, sel.statement)

dbDisconnect(con)

covDat <- dummy %>%
  full_join(cw, by = "zip3") %>%
  group_by(fips, year) %>%
  summarise(zOverlaps = length(zip3), adjProviderCoverage = weighted.mean(adjProviderCoverage, proportion, na.rm = TRUE), sampViz = weighted.mean(sampViz, proportion, na.rm = TRUE), sampProv = weighted.mean(sampProv, proportion, na.rm = TRUE)) %>% 
  ungroup %>%
  filter(!is.na(fips)) %>% 
  mutate(adjProviderCoverage = ifelse(is.na(adjProviderCoverage), 0, adjProviderCoverage)) %>% # 9/27/16 for glm: 0 if NA
  mutate(visitsPerProvider = ifelse(is.na(sampViz), 0, sampViz/sampProv)) %>% # 9/27/16 for glm: 0 if NA
  left_join(popDat, by = c("fips", "year")) %>%
  mutate(visitsPerPop = ifelse(is.na(sampViz), 0, sampViz/pop)) %>%
  mutate(covBin = cut(adjProviderCoverage, breaks=bks, include.lowest=TRUE, ordered_result=TRUE)) 

# import careseek data
csDat <- cleanO_imsCareseekTot_cty()
hist(csDat$visitsPerPopT, breaks = 50)

################################
# explore careseeking by different ims coverage bins
hist(dummy$adjProviderCoverage)
bks <- quantile(dummy$adjProviderCoverage, seq(0,1, .25), na.rm=TRUE)

covbinP <- ggplot(covDat, aes(x=visitsPerPop, y=..density..)) +
  geom_histogram(bins=100) +
  facet_wrap(~covBin, scales="free")
print(covbinP)
  
################################

zipDat <- tbl_df(dummy) %>%
  filter(!is.na(adjProviderCoverage))



