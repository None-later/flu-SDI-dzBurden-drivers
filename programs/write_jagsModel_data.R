
## Name: Elizabeth Lee
## Date: 9/28/15
## Function: Process and write JAGS model data for import into main code (e.g. main_model_1a_iliSum.R)
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
# .Rprofile sets wd 'Dropbox (Bansal Lab)/code' and sources "GeneralTools.R" in .myenv
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
require(tidyr)
require(dplyr)

#### set these ################################
code <- 't2_'
code2 <- '_Oct'
modcode <- '1a_iliSum'

#### import data ################################
setwd('../R_export')
# zip3-season combos to include
comboData <- read.csv(sprintf('zip3SeasonCombos_%sILI%s.csv', code, code2), header=T, colClasses=c(zipname="character"))
combos <- comboData %>% mutate(id = paste0(season, zipname))
# disease burden metrics
dbMetrics.g <- read.csv(sprintf('dbMetrics_periodicReg_%sILI%s_analyzeDBdist.csv', code, code2), header=T, colClasses=c(zipname="character", metric="character"))
dbData <- dbMetrics.g %>% mutate(id.combo = paste0(season, zipname)) %>% filter(id.combo %in% combos$id) %>% select(season, zipname, id.combo, metric, burden)
# zip3 provider coverage data
pcovData <- read.csv('physicianCoverage_IMSHealth_zip3.csv', header=T, colClasses=c(zip3="character"))
pcov <- pcovData %>% mutate(season = as.numeric(substr.Right(year, 1))) %>% mutate(id.combo = paste0(season, zip3)) %>% filter(id.combo %in% combos$id) 

setwd('../Py_export')
# population data
pop_df <- read.csv('popByallZip_allYearly_totAge.csv', header=T, colClasses=c("year"="character"))
pop_df2 <- pop_df %>% mutate(year = substr(pop_df$year, 1, 4))
pop_gather_df <- gather(pop_df2, zip3, pop, X2:X999, convert=FALSE) %>% mutate(zip3 = as.character(zip3))
popData <- pop_gather_df %>% mutate(zipname = substr.Right(paste0("00", substr(zip3, 2, nchar(zip3))), 3)) %>% mutate(season = as.numeric(substr.Right(year, 1))) %>% mutate(id.combo = paste0(season, zipname)) %>% filter(id.combo %in% combos$id)

#### change to directory for writing ################################
dir.create('../R_export/jagsModelData_import', showWarnings = FALSE) # create directory if not exists
setwd('../R_export/jagsModelData_import')

#### 1a_iliSum ################################
if (modcode=='1a_iliSum'){
  # process
  dbData2 <- dbData %>% filter(metric=='ili.sum' & season != 1)
  pcov2 <- pcov %>% select(id.combo, covAdjProv)
  popData2 <- popData %>% select(id.combo, pop)
  # merge
  dummy1 <- left_join(dbData2, pcov2, by='id.combo')
  modeldata <- left_join(dummy1, popData2, by='id.combo')
}

#### write model data to file ################################
write.csv(modeldata, sprintf('modelData_%s.csv', modcode), row.names=F)
