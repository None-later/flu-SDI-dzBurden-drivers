
## Name: Elizabeth Lee
## Date: 8/5/15
## Function: Check that original flu data and physician coverage data for the sample visits are comparable. We notice in zip3-level IR time series that there are average time trends even though we wouldn't expect flu incidence to change in those ways over time. Consequently, we are examining the possibility that the IR normalization with patient visits is not appropriate and that a normalization with the new physician coverage data would be better. First, we want to examine that the visit data in the two datasets are comparable.
## Results: At the zip3 level, the original ili data and physician coverage data do not report the same number of total visits, and discrepancies are evident in both directions. At the national level, the original ili data had a much greater number of visits than the physician coverage data (on the order of 40-100 million).
## Filenames: physician_coverage/DX_Coverage_by_Flu_Season_20150620.csv, dz_burden/SQL_export/ILIViz_allWeekly_totServ_totAge_allZip.csv
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

setwd('/home/elee/R/source_functions')
source("dfsumm.R")

require(readr)
require(dplyr)
require(tidyr)

##################################
# import flu data
setwd('/home/elee/Dropbox (Bansal Lab)/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/SQL_export')
fludata <- read_csv('ILIViz_allWeekly_totServ_totAge_allZip.csv', col_types = list(patient_zip3=col_character()))
names(fludata) <- c("week", "zip3", "ili", "visits.flu")
fludata2 <- fludata %>% select(-ili)

# import physician coverage data
setwd('/home/elee/Dropbox (Bansal Lab)/Elizabeth_Bansal_Lab/SDI_Data/physician_coverage')
covdata <- read_csv('DX_Coverage_by_Flu_Season_20150620.csv', col_types = list(ZIP3=col_character(), AVG_UNVRS_CNT=col_numeric())) 
covdata2 <- covdata %>% select(FLU_SEASON, ZIP3, SAMP_VIS_CNT) %>% mutate(period = substr(FLU_SEASON, 18, 21))
names(covdata2) <- c("seasontext", "zip3", "visits.cov", "period")

##################################
# plotting parameters
w = 580 
h = 700
ps = 14
un = "px"

##################################
# Let's perform checks in two different time periods for reference: 
# 1) Oct 2001 to April 2002 flu season; 2) Oct 2008 to April 2009 flu season

# process data for time period 1
fludata.p1 <- fludata2 %>% filter(week > as.Date("2001-09-30") & week < as.Date("2002-05-01")) %>% group_by(zip3) %>% summarise(visits.flu = sum(visits.flu, na.rm=T)) 
covdata.p1 <- covdata2 %>% select(-seasontext) %>% filter(period == "2002")
data.p1 <- right_join(fludata.p1, covdata.p1, by="zip3") %>% mutate(flu.min.cov = visits.flu-visits.cov)
hist(data.p1$flu.min.cov, breaks=50, main="Oct 01 to Apr 02 flu vs. physcov data", xlab="difference in visits")

# process data for time period 2
fludata.p2 <- fludata2 %>% filter(week > as.Date("2008-09-30") & week < as.Date("2009-05-01")) %>% group_by(zip3) %>% summarise(visits.flu = sum(visits.flu, na.rm=T)) 
covdata.p2 <- covdata2 %>% select(-seasontext) %>% filter(period == "2009")
data.p2 <- right_join(fludata.p2, covdata.p2, by="zip3") %>% mutate(flu.min.cov = visits.flu-visits.cov)
hist(data.p2$flu.min.cov, breaks=50, main="Oct 08 to Apr 09 flu vs. physcov data", xlab="difference in visits")

##################################
# It seems that the zip3 data are not comparable. For further information, let's perform checks for each season.
year1s <- 2001:2008
setwd('/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/graph_outputs/testdata_iliData_physcovData')

for (yr1 in year1s){
  yr2 <- as.character(yr1+1)
  yr1 <- as.character(yr1)  
  dummy.flu <- fludata2 %>% filter(week > as.Date(sprintf("%s-09-30", yr1)) & week < as.Date(sprintf("%s-05-01", yr2))) %>% group_by(zip3) %>% summarise(visits.flu = sum(visits.flu, na.rm=T)) 
  dummy.cov <- covdata2 %>% select(-seasontext) %>% filter(period == yr2)
  dummy.data <- right_join(dummy.flu, dummy.cov, by="zip3") %>% mutate(flu.min.cov = visits.flu-visits.cov)
  png(filename = sprintf("histComparison_flu_physcov_%s.png", yr2),  units=un, width=w, height=h, pointsize=ps, bg = 'white')
    hist(dummy.data$flu.min.cov, breaks=50, main=sprintf("Oct %s to Apr %s flu vs. physcov data", yr1, yr2), xlab="difference in visits", ylim=c(0,300))
  dev.off()
} # figures saved 8/5/15, 15:15

##################################
# There are large discrepancies across all of the seasons. Now let's compare national level data.

# import national coverage data
setwd('/home/elee/Dropbox (Bansal Lab)/Elizabeth_Bansal_Lab/SDI_Data/physician_coverage')
natcovdata <- read_csv('Nat_Coverage_by_Flu_Season_20150620.csv')
natcov <- natcovdata %>% select(FLU_SEASON, contains("VIS_CNT")) %>% mutate(period = substr(FLU_SEASON, 18, 21)) %>% select(-FLU_SEASON)
names(natcov) <- c("visits.cov", "period")

# process flu data to national level
incl.months1 <- c("10", "11", "12")
incl.months2 <- c("01", "02", "03", "04")
process1 <- fludata2 %>% ungroup %>% group_by(week) %>% summarise(visits.flu = sum(visits.flu, na.rm=T)) %>% mutate(period = ifelse(substr(week, 6, 7) %in% incl.months1, as.character(as.numeric(substr(week, 1, 4))+1), ifelse(substr(week, 6, 7) %in% incl.months2, substr(week, 1, 4), NA)))
process2 <- process1 %>% ungroup %>% filter(!is.na(period)) %>% group_by(period) %>% summarise(visits.flu = sum(visits.flu, na.rm=T))

combo <- left_join(process2, natcov, by="period") %>% mutate(flu.min.cov = visits.flu-visits.cov)

# recorded 8/5/15 afternoon
#       period visits.flu visits.cov flu.min.cov
# 1       2001   55376291         NA          NA
# 2       2002  111792748   69919556    41873192
# 3       2003  137506523   87849110    49657413
# 4       2004  174805755  111675641    63130114
# 5       2005  201584701  124830522    76754179
# 6       2006  209678830  125644037    84034793
# 7       2007  223744228  132347418    91396810
# 8       2008  268859470  162897733   105961737
# 9       2009  319230350  219960351    99269999
# 10      2010  437067998         NA          NA
