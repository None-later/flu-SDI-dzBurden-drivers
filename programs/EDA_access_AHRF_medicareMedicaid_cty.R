## Name: Elizabeth Lee
## Date: 11/17/15
## Function: county-level EDA for Area Health Resource Files, Medicare and Medicaid eligibility data - choropleths for each access metric, time series organized by state
## Filenames: clean_AHRF_access.csv
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
require(dplyr)
require(choroplethr)

setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program

#### read data ################################
setwd("../../../../Sandra Goldlust's Work/Shared_Data/SG_covariate_data/Cleaned_Data")
medDat <- read_csv("clean_access_medicare_medicaid.csv", col_types = "ciiiiiidddd") %>%
  mutate(region = as.numeric(FIPS)) %>%
  mutate(st.fips = substring(FIPS, 1, 2)) %>%
  rename(mdcrElig = mdcr_elig, mdcrEnroll = mdcr_enroll, mcaidChild = mcaid_child, mcaidAdult = mcaid_adult, mdcrEligPerPop = mdcr_elig_normthous, mdcrEnrollPerPop = mdcr_enroll_normthous, mcaidChildPerPop = mcaid_child_normthous, mcaidAdultPerPop = mcaid_adult_normthous)

setwd(dirname(sys.frame(1)$ofile))
setwd("../reference_data")
abbrDat <- read_csv("state_abbreviations_FIPS.csv", col_types = "ccc") 

#### plot formatting ################################
h <- 5; w <- 8; dp <- 300
h2 <- 9; w2 <- 9
years <- sort(unique(medDat$year))
num <- 6
varnames <- names(medDat)[c(3:6, 8:11)]
leg.lab <- c("Medicare eligibles", "Medicare enrollees", "Medicaid eligible children", "Medicaid eligible adults", "Medicare elig. per pop per 1K", "Medicare enroll. per pop per 1K", "Medicaid elig. C per tot pop per 1K", "Medicaid elig. A per tot pop per 1K")

#### data cleaning ################################
fullDat <- left_join(medDat, abbrDat, by = c("st.fips" = "FIPS"))
uqst <- fullDat %>% select(State) %>% distinct %>% arrange(State) %>%
  mutate(for.plot = seq_along(1:nrow(.)))

fullDat2 <- left_join(fullDat, uqst, by = "State")
indexes <- seq(1, max(fullDat2 %>% select(for.plot)), by=num)

#### plotting ################################
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
dir.create(sprintf('../graph_outputs/EDA_access_AHRF_medicareMedicaid_cty'), showWarnings=FALSE)
dir.create(sprintf('../graph_outputs/EDA_access_AHRF_medicareMedicaid_cty/ts'), showWarnings=FALSE) # create directory if not exists
setwd('../graph_outputs/EDA_access_AHRF_medicareMedicaid_cty/ts')

#### time series ################################
for(i in indexes){
  for(v in varnames){
    dummyplots <- ggplot(fullDat2 %>% filter(for.plot>= i & for.plot < i+num), aes(x=year, y=eval(parse(text = v)), group=FIPS)) +
      theme_bw()+
      theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
      geom_line(aes(colour = FIPS)) +
      geom_point(aes(colour = FIPS)) +
      scale_y_continuous(name = leg.lab[which(v == varnames)]) +
      guides(colour = "none") +
      coord_cartesian(xlim = c(2000, 2015)) +
      facet_wrap(~State)
    labs <- fullDat2 %>% filter(for.plot>= i & for.plot < i+num) %>% select(Abbreviation) %>% distinct %>% slice(c(i, i+num-1))  %>% unlist
    ggsave(sprintf("%s_AHRF_cty_%s-%s.png", v, labs[1], labs[2]), dummyplots, width = w2, height = h2, dpi = dp)
  }
  
} # 11/17/15

#### choropleth ################################
dir.create(sprintf('../choro'), showWarnings=FALSE) # create directory if not exists
setwd('../choro')
for (y in years){
  dummy <- fullDat2 %>% filter(year == y)
  for (v in varnames){
    dummy2 <- dummy %>% rename_(value = as.name(v))
    if (all(is.na(dummy2$value))) {next}
    dps <- county_choropleth(dummy2, legend = leg.lab[which(v == varnames)])
    ggsave(sprintf("%s_AHRF_cty_%s.png", varnames[which(v == varnames)], y), dps, width = w, height = h, dpi = dp)
  }
  
} # 11/17/15

#### data cleaning: scatter bw access & population variables ################################
fullDat3 <- fullDat2 %>% 
  select(FIPS, Abbreviation, year, mdcrElig, mdcrEnroll, mcaidChild, mcaidAdult, population) %>%
  gather(accVar, n, 4:7)

#### scatter bw access & population variables ################################
dir.create(sprintf('../scatterPop'), showWarnings=FALSE)
setwd('../scatterPop')
for (y in years){
  dummy <- fullDat3 %>% filter(year == y)
  scatters <- ggplot(dummy, aes(x = n,  y = population)) +
    geom_point(color = 'black') +
    theme_bw(base_size = 12, base_family = "") +
    coord_cartesian(xlim = c(1, 1500000), ylim = c(1, 10000000)) +
    scale_y_log10() + scale_x_log10() +
    facet_wrap(~accVar, nrow = 3)
  ggsave(sprintf("scatter_access_pop_cty_%s.png", y), scatters, width = w, height = h, dpi = dp)
} # 11/18/15





