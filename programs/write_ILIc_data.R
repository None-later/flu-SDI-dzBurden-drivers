
## Name: Elizabeth Lee
## Date: 10/24/15
## Function: write ilic data: ilic_{z, w} = ili_{z, w} / alpha_{z, y} / effPhysCov_{z, y}
## ilic --> number of ili cases in zip z in week w, correcting for constant care-seeking rates across zip3s and scaling up for physician coverage; scaling up assumes that the ili/physician ratio is the same for the reported and unreported cases
## alpha_{z, y} = (viz_{z, y}/numPhys_{z, y}) / (\bar{viz_y}/\bar{phys_y}) --> correction for general care-seeking behavior

## Filenames: physician_coverage/DX_Coverage_by_Flu_Season_20150620.csv; Py_export/iliByallZip_allWeekly_totServ_totAge.csv
## Data Source: IMS Health ili dataset and physician coverage dataset
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header ####################################
rm(list = ls())
require(dplyr)
require(ggplot2)
require(tidyr)
setwd(dirname(sys.frame(1)$ofile))

#### import data ################################
setwd('../Py_export')
ili_df <- read.csv('iliByallZip_allWeekly_totServ_totAge.csv', header=T, colClasses=c("week"="Date"))
# viz_df <- read.csv('vizByallZip_allWeekly_totServ_totAge.csv', header=T, colClasses=c("week"="Date"))
pop_df <- read.csv('popByallZip_allYearly_totAge.csv', header=T, colClasses=c("year"="character"))

setwd('../R_export')
cov_df <- read_csv('physicianCoverage_IMSHealth_zip3.csv', col_types = list("zip3" = col_character()))

#### data cleaning: ILI data ####################################
# date formatting in ili dataset
ili_df2 <- ili_df %>% mutate(Thu.week = as.Date(week+4)) %>% 
  mutate(year = as.numeric(substr(as.character(Thu.week), 1, 4))) %>% 
  mutate(month = as.numeric(substr(as.character(Thu.week), 6, 7))) %>%   
  mutate(flu.week = (month >= 11 | month <= 4)) %>% 
  mutate(t = seq_along(1:nrow(.))) %>%
  mutate(fit.week = (month >= 4 & month <= 10))

# gather ili data
ili_gather_df <- gather(ili_df2, zip3, ili, X2:X999, convert=FALSE) %>% 
  mutate(week = as.Date(week, origin="1970-01-01")) %>%
  mutate(Thu.week = as.Date(Thu.week, origin="1970-01-01")) %>% 
  mutate(zip3 = substr.Right(sub('X', '00', zip3), 3)) 

# gather pop data
pop_gather_df <- gather(pop_df, zip3, pop, X2:X999, convert=FALSE) %>%
  mutate(zip3 = substr.Right(sub('X', '00', zip3), 3)) %>%
  mutate(year = as.numeric(substr(as.character(year), 1, 4)))

# merge ili & pop data
iliDat <- left_join(ili_gather_df, pop_gather_df, by = c('zip3', 'year'))

#### data cleaning: physician coverage data ####################################
# drop columns in cov data & generate alpha.numer
cov_df2 <- cov_df %>% select(zip3, year, avgUnvrs, sampViz, covAdjProv) %>%
  filter(!is.na(sampViz + avgUnvrs)) %>% 
  mutate(alpha.numer = sampViz/avgUnvrs) %>%
  rename(cov_z.y = covAdjProv)

# calculate \bar{viz_y}/\bar{phys_y})
mn_viz_phys <- cov_df2 %>% 
  group_by(year) %>%
  summarise(bar.viz_y = mean(sampViz), bar.phys_y = mean(avgUnvrs)) %>%
  mutate(alpha.denom = bar.viz_y/bar.phys_y) %>%
  ungroup

# calculate alpha_{z, y}
alphaDat_Full <- left_join(cov_df2, mn_viz_phys, by = 'year') %>%
  mutate(alpha_z.y = alpha.numer/alpha.denom) %>%
  rename(viz_z.y = sampViz) %>%
  rename(phys_z.y = avgUnvrs)

### copy cov & alpha data from 2002 to 2001 weeks because coverage data begins in 2002 ###
dummy2001Dat <- alphaDat_Full %>% filter(year == 2002) %>%
  mutate(year = 2001)
alphaDat_Full2 <- bind_rows(alphaDat_Full, dummy2001Dat)
alphaDat_Full2 <- arrange(alphaDat_Full2, zip3, year)

# shorten dataset
alphaDat <- alphaDat_Full2 %>% select(zip3, year, cov_z.y, alpha_z.y) 

#### data cleaning: generate ilic data ####################################
# identify zip3s with too little ILI data during fitted week periods (Apr to Oct >= 50 NAs)
noILIdata <- iliDat %>% filter(fit.week) %>% 
  filter(Thu.week < as.Date('2009-05-01')) %>% group_by(zip3) %>% 
  summarise(num.NA = sum(is.na(ili))) %>% mutate(incl.lm = num.NA < 50)

# join datasets to incorporate indicators for including zip3 into lm
iliDat2 <- right_join(iliDat, (noILIdata %>% select(-num.NA)), by='zip3')

# create ilic metric
ilicDat <- left_join(iliDat2, alphaDat, by = c("zip3", "year")) %>%
  mutate(ILIc = ili/alpha_z.y/cov_z.y)

#### write Data to file ####################################
write.csv(alphaDat_Full2, file = 'vizPhysRatio_zipYear_corrections.csv', row.names=F)
write.csv(ilicDat, file = 'ilicByallZip_allWeekly_totServ_totAge.csv', row.names=F)









