
## Name: Elizabeth Lee
## Date: 8/20/16
## Function: What are the contributions of backcalculated ILI < 1 to in.season weeks?
### disease burden metrics: 

## Filenames: fullIndicAll_periodicReg...
## Data Source: IMS Health 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header ####################################
require(dplyr)
require(ggplot2)
require(readr)
require(ISOweek)
require(tidyr)
setwd(dirname(sys.frame(1)$ofile))

#### local functions ####################################
source("source_export_inlaData_cty.R")

#### set these! ####################################
code <-"" # linear time trend term
code2 <- "_Octfit"
spatial <- list(scale = "county", stringcode = "County", stringabbr = "_cty", serv = "_totServ", servToggle = "") 
span.var <- 0.4 # 0.4, 0.6
degree.var <- 2
code.str <- sprintf('_span%s_degree%s', span.var, degree.var)

#### import data ####################################
setwd('../R_export')
importDat <- read_csv(sprintf('fullIndicAll_periodicReg_%silinDt%s%s_analyzeDB%s.csv', code, code2, code.str, spatial$stringabbr), col_names = T, col_types = list(fips = col_character(), ili = col_double(), pop = col_integer(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilin.dt = col_double(), ILIn = col_double(), in.season = col_logical(), has.epi = col_logical(), incl.analysis = col_logical()))

#### clean data ####################################
dat <- importDat %>% filter(in.season & season !=1)

cleanDat <- dat %>%
  mutate(ili1 = ifelse(ili < 1, TRUE, FALSE)) %>%
  group_by(season, fips) %>%
  summarise(totInSeason = sum(in.season), ili1InSeason = sum(ili1)) %>%
  mutate(allILI1 = ifelse(totInSeason == ili1InSeason, TRUE, FALSE)) %>%
  ungroup

#### plot formatting ####################################
w <- 4; h <- 4
setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/check_backcalculateILI_inSeason", showWarnings = FALSE)
setwd("../graph_outputs/check_backcalculateILI_inSeason")

#### exploration ####################################
# 1. By season, how many counties have at least one in.season week where ili < 1? (ili1Fips)
# 2. By season, how many counties have only in.season weeks where ili < 1? (allILI1Fips)
q1 <- cleanDat %>% 
  group_by(season) %>%
  summarise(totFips = length(fips), ili1Fips = sum(ifelse(ili1InSeason > 0, TRUE, FALSE)), allILI1Fips = sum(allILI1))

# 3. By season, distribution of in.season weeks where ili < 1?
totInSeas <- ggplot(cleanDat, aes(x = totInSeason, y = ..count.., group = season)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~season) +
  scale_x_continuous("total in.season weeks") +
  scale_y_continuous("counties")
ggsave(sprintf("totalInSeasonWeeks_%silinDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), totInSeas, width = w, height = h)
print(totInSeas)

q3 <- ggplot(cleanDat, aes(x = ili1InSeason, y = ..count.., group = season)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~season) +
  scale_x_continuous("in.season weeks where ILI < 1") +
  scale_y_continuous("counties")
ggsave(sprintf("ili1InSeasonWeeks_%silinDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), q3, width = w, height = h)
print(q3)

q3.no0 <- ggplot(cleanDat %>% filter(ili1InSeason>0), aes(x = ili1InSeason, y = ..count.., group = season)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~season) +
  scale_x_continuous("in.season weeks where ILI < 1 (excl.0)") +
  scale_y_continuous("counties")
ggsave(sprintf("ili1InSeasonWeeks_no0_%silinDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), q3.no0, width = w, height = h)
print(q3.no0)

# 4. By season, among counties with only in.season weeks < 1, what is the distribution of total in.season weeks?
q4Dat <- cleanDat %>% filter(allILI1)

q4 <- ggplot(q4Dat, aes(x = totInSeason, y = ..count.., group = season)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~season) +
  scale_x_continuous("total in.season weeks where allILI1") +
  scale_y_continuous("counties")
ggsave(sprintf("totalInSeasonWeeks_allILI1_%silinDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), q4, width = w, height = h)
print(q4)

# 5. By season, among counties with only in.season weeks where ILI < 1, what is the population distribution?
popDat <- dat %>%
  distinct(season, fips, pop)
q5Dat <- full_join(q4Dat, popDat, by = c("season", "fips"))

q5 <- ggplot(q5Dat, aes(x = pop, y = ..count.., group = season)) +
  geom_histogram() +
  facet_wrap(~season) +
  scale_x_continuous("pop where allILI1", limits = c(0,200000)) +
  scale_y_continuous("counties")
ggsave(sprintf("pop_allILI1_%silinDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), q5, width = w, height = h)
print(q5)

q5.zm <- ggplot(q5Dat, aes(x = pop, y = ..count.., group = season)) +
  geom_histogram() +
  facet_wrap(~season) +
  scale_x_continuous("pop where allILI1, zoomed", limits = c(0,50000)) +
  scale_y_continuous("counties")
ggsave(sprintf("pop_allILI1_zm_%silinDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), q5.zm, width = w, height = h)
print(q5.zm)

# 6. By season, what is the spatial distribution of counties with in.season weeks where ILI < 1?
q6Dat <- cleanDat %>%
  mutate(ILI1_proportion = ili1InSeason/totInSeason) 
seaslist <- q6Dat %>% distinct(season) %>% unlist
choro.type <- "tier"

for (s in seaslist){
  q6Dat_seas <- q6Dat %>% filter(season == s)
  exportPath <- sprintf("choro_ili1Proportion_inSeason_%s_%silinDt%s%s%s_S%s.png", choro.type, code, code2, code.str, spatial$stringabbr, s)
  plot_countyChoro(exportPath, q6Dat_seas, "ILI1_proportion", choro.type)
  
}







