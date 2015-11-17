
## Name: Elizabeth Lee
## Date: 11/17/15
## Function: county-level EDA for Area Health Resource Files - choropleths for each access metric, time series organized by state
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
accDat <- read_csv("clean_AHRF_access.csv", col_types = "ciiiiiddd") %>%
  rename(outpatient = hosp_out, hospPerPop = hosp_norm_thous, outpatientPerPop = hosp_out_norm_thous, physPerPop = physicians_norm_thous) %>%
  mutate(region = as.numeric(FIPS)) %>%
  mutate(st.fips = substring(FIPS, 1, 2))

setwd(dirname(sys.frame(1)$ofile))
setwd("../reference_data")
abbrDat <- read_csv("state_abbreviations_FIPS.csv", col_types = "ccc") 

#### plot formatting ################################
h <- 5; w <- 8; dp <- 300
h2 <- 9; w2 <- 9
years <- sort(unique(accDat$year))
num <- 6
varnames <- c("hosp", "outpatient", "physicians", "population", "hospPerPop", "outpatientPerPop", "physPerPop")


#### data cleaning ################################
fullDat <- left_join(accDat, abbrDat, by = c("st.fips" = "FIPS"))
uqst <- fullDat %>% select(State) %>% distinct %>% arrange(State) %>%
  mutate(for.plot = seq_along(1:nrow(.)))

fullDat2 <- left_join(fullDat, uqst, by = "State")
indexes <- seq(1, max(fullDat2 %>% select(for.plot)), by=num)


#### plotting ################################
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program
dir.create(sprintf('../graph_outputs/EDA_access_AHRF_cty/ts'), showWarnings=FALSE) # create directory if not exists
setwd('../graph_outputs/EDA_access_AHRF_cty/ts')

#### time series ################################
for(i in indexes){
  for(v in varnames){
    dummyplots <- ggplot(fullDat2 %>% filter(for.plot>= i & for.plot < i+num), aes(x=year, y=eval(parse(text = v)), group=FIPS)) +
      theme_bw()+
      theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
      geom_line(aes(colour = FIPS)) +
      geom_point(aes(colour = FIPS)) +
      scale_y_continuous(name = eval(v)) +
      # scale_x_discrete() +
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
  # total hosp
  hospP <- county_choropleth(dummy %>% rename(value = hosp), legend = "Total hospitals")
  ggsave(sprintf("hosp_AHRF_cty_%s.png", y), hospP, width = w, height = h, dpi = dp)
  # total outpatients
  outpP <- county_choropleth(dummy %>% rename(value = outpatient), legend = "Total outpatients")
  ggsave(sprintf("outpatient_AHRF_cty_%s.png", y), outpP, width = w, height = h, dpi = dp)
  # total physicians
  physP <- county_choropleth(dummy %>% rename(value = physicians), legend = "Total physicians")
  ggsave(sprintf("physicians_AHRF_cty_%s.png", y), physP, width = w, height = h, dpi = dp)
  # hospitals per population per 1K
  hppP <- county_choropleth(dummy %>% rename(value = hospPerPop), legend = "Hosp per Pop per 1K")
  ggsave(sprintf("hospPerPop_AHRF_cty_%s.png", y), hppP, width = w, height = h, dpi = dp)
  # total physicians
  oppP <- county_choropleth(dummy %>% rename(value = outpatientPerPop), legend = "Outp per Pop per 1K")
  ggsave(sprintf("outpatientPerPop_AHRF_cty_%s.png", y), oppP, width = w, height = h, dpi = dp)
  # total physicians
  pppP <- county_choropleth(dummy %>% rename(value = physPerPop), legend = "Phys per pop per 1K")
  ggsave(sprintf("physPerPop_AHRF_cty_%s.png", y), pppP, width = w, height = h, dpi = dp)

  } # 11/17/15

