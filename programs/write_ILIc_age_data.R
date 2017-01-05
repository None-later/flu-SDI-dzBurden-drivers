
## Name: Elizabeth Lee
## Date: 1/4/17
## Function: write ilic data for age groups: ilic_{z, w} = ili_{z, w} / alpha_{z, y} / effPhysCov_{z, y}
## ilic --> number of ili cases in zip z in week w, correcting for constant care-seeking rates across zip3s and scaling up for physician coverage; scaling up assumes that the ili/physician ratio is the same for the reported and unreported cases
## alpha_{z, y} = (viz_{z, y}/numPhys_{z, y}) / (\bar{viz_y}/\bar{phys_y}) --> correction for general care-seeking behavior
## Filenames: physician_coverage/DX_Coverage_by_Flu_Season_20150620.csv; Py_export/iliByallZip_allWeekly_totServ_totAge.csv
## Data Source: IMS Health ili dataset and physician coverage dataset
## Notes: 1/4/17 remove pop data from zip3 level
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header ####################################
rm(list = ls())
require(dplyr)
require(ggplot2)
require(tidyr)
require(readr)
setwd(dirname(sys.frame(1)$ofile))

#### set these! ####################################
# 11/10/16 new age group toggle
agegroup <- "adult" # "child", "adult"

#### import data ################################
setwd('../Py_export')
ili_df <- read.csv(sprintf('iliByallZip_allWeekly_totServ_%s.csv', agegroup), header=T, colClasses=c("week"="Date"))

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
iliDat <- gather(ili_df2, zip3, ili, X2:X999, convert=FALSE) %>% 
  mutate(week = as.Date(week, origin="1970-01-01")) %>%
  mutate(Thu.week = as.Date(Thu.week, origin="1970-01-01")) %>% 
  mutate(zip3 = substr.Right(sub('X', '00', zip3), 3)) %>%
  mutate(ili = ifelse(is.na(ili), 0, ili)) # 7/15/16: replace NAs with 0

#### data cleaning: physician coverage data ####################################
# drop columns in cov data & generate alpha.numer
cov_df2 <- cov_df %>% select(zip3, year, avgUnvrs, sampViz, covAdjProv) %>%
  filter(!is.na(sampViz + avgUnvrs)) %>% 
  mutate(alpha.numer = sampViz/avgUnvrs) %>%
  rename(cov_z.y = covAdjProv) %>%
  mutate(cov_below5 = cov_z.y < 0.05)

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
alphaDat <- alphaDat_Full2 %>% select(zip3, year, cov_z.y, alpha_z.y, cov_below5) 

#### data cleaning: write ilic data ####################################
# create ilic metric
ilicDat <- left_join(iliDat, alphaDat, by = c("zip3", "year")) %>%
  mutate(ILIc = ili/alpha_z.y/cov_z.y)

#### write Data to file ####################################
write.csv(ilicDat, file = sprintf('ilicByallZip3_allWeekly_totServ_%s.csv', agegroup), row.names=F)
# exported 7/16/16








