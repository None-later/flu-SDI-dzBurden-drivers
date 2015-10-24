
## Name: Elizabeth Lee
## Date: 10/24/15
## Function:
## Filenames: 
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(tidyr)
require(ggplot2)
require(readr)
require(dplyr)
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program

####plot formatting ################################
w = 6; h = 6

#### import data ################################
setwd('../R_export')
zipCov <- read_csv('physicianCoverage_IMSHealth_zip3.csv', col_types = list("zip3" = col_character()))

setwd('../Py_export')
viz_df <- read.csv('vizByallZip_allWeekly_totServ_totAge.csv', header=T, colClasses=c("week"="Date"))
pop_df <- read.csv('popByallZip_allYearly_totAge.csv', header=T, colClasses=c("year"="character"))

#### data cleaning ################################
pop_df <- pop_df %>% mutate(year = substr(pop_df$year, 1, 4))
pop_gather_df <- gather(pop_df, zip3, pop, X2:X999, convert=FALSE)
viz_gather_df <- gather(viz_df, zip3, viz, X2:X999, convert=FALSE) %>% mutate(week = as.Date(week, origin="1970-01-01")) %>% mutate(year = substr(as.character(week), 1, 4))
pop_viz_df <- left_join(viz_gather_df, pop_gather_df, by = c('year', 'zip3')) %>% mutate(zip3 = substr.Right(sub('X', '00', zip3), 3)) %>% mutate(year = as.numeric(year))

fullD <- left_join(pop_viz_df, zipCov %>% select(-minUnvrs, -maxUnvrs, -covProv), by = c("year", "zip3")) %>% rename(weekViz = viz) %>% filter(!is.na(for.plot))

vpRatio <- fullD %>% group_by(year, zip3) %>% 
  summarise(mn_wkViz = mean(weekViz, na.rm=T), pop = max(pop, na.rm=T), subViz = max(sampViz, na.rm=T), AMAphys = max(avgUnvrs, na.rm=T), IMSphys = max(sampProv, na.rm=T)) %>% 
  ungroup %>%
  mutate(vizPopRatio = mn_wkViz/pop) %>% 
  mutate(vizPhysRatio = mn_wkViz/IMSphys) %>% 
  mutate(vizPopRatio.sub = subViz/pop) %>%
  mutate(vizPhysRatio.sub = subViz/IMSphys)
  
vpRatio.mn <- vpRatio %>% group_by(year) %>% summarise(vizPopRatMn = mean(vizPopRatio, na.rm=T), vizPhysRatMn = mean(vizPhysRatio, na.rm=T), vizPopSubMn = mean(vizPopRatio.sub, na.rm=T), vizPhysSubMn = mean(vizPhysRatio.sub, na.rm=T))

#### explore data ################################
dir.create('../graph_outputs/explore_coverageRatioMetrics', showWarnings=FALSE) 
setwd('../graph_outputs/explore_coverageRatioMetrics')

# 1a) for each year, is the observed viz/population ratio constant across locations (viz from ILI data)?
vizpop.plot <- ggplot(vpRatio, aes(x = vizPopRatio, group = year)) +
  geom_histogram(aes(y = ..density..), stat = 'bin') +
  geom_vline(data = vpRatio.mn, aes(xintercept = vizPopRatMn), colour = 'red') +
  facet_wrap(~year) +
  ggtitle('Visits (ilidata) per population distribution')
print(vizpop.plot)
ggsave('vizPopRatio_iliDat.png', vizpop.plot, width = w, height = h)

# 1b) for each year, is the observed viz/physician ratio constant across locations (viz from ILI data)?
vizphys.plot <- ggplot(vpRatio, aes(x = vizPhysRatio, group = year)) +
  geom_histogram(aes(y = ..density..), stat = 'bin') +
  geom_vline(data = vpRatio.mn, aes(xintercept = vizPhysRatMn), colour = 'red') +
  facet_wrap(~year) +
  coord_cartesian(xlim = c(0, 750)) +
  ggtitle('Visits (ilidata) per physician distribution')
print(vizphys.plot)
ggsave('vizPhysRatio_iliDat.png', vizphys.plot, width = w, height = h)


# 2a) for each year, is the coverage viz/population ratio constant across locations (viz from coverage data)?
vizpop.plot2 <- ggplot(vpRatio, aes(x = vizPopRatio.sub, group = year)) +
  geom_histogram(aes(y = ..density..), stat = 'bin') +
  geom_vline(data = vpRatio.mn, aes(xintercept = vizPopSubMn), colour = 'red') +
  facet_wrap(~year) +
  ggtitle('Visits (covdata) per population distribution')
print(vizpop.plot2)
ggsave('vizPopRatio_covDat.png', vizpop.plot2, width = w, height = h)


# 2b) for each year, is the observed viz/physician ratio constant across locations (viz from coverage data)?
vizphys.plot2 <- ggplot(vpRatio, aes(x = vizPhysRatio.sub, group = year)) +
  geom_histogram(aes(y = ..density..), stat = 'bin') +
  geom_vline(data = vpRatio.mn, aes(xintercept = vizPhysSubMn), colour = 'red') +
  facet_wrap(~year) +
  ggtitle('Visits (covdata) per physician distribution')
print(vizphys.plot2)
ggsave('vizPhysRatio_covDat.png', vizphys.plot2, width = w, height = h)
