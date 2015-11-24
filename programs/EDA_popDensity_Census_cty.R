## Name: Elizabeth Lee
## Date: 11/23/15
## Function: EDA - Census 2000 & 2010 population and population density - choropleth, county-level; time series by county grouped by state
## Filename: SG_covariate_data/Cleaned_Data/clean_Census_popdensity_county.csv
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")
rm(list = ls())
#### header ####################################
require(ggplot2)
require(dplyr)
require(readr)
require(RColorBrewer)
require(choroplethr)
require(choroplethrMaps)
setwd(dirname(sys.frame(1)$ofile))

#### plot formatting ################################
tierVec <- paste("Tier", 1:5)
colVec <- brewer.pal(length(tierVec), 'RdYlGn')
h <- 5; w <- 8; dp <- 300
h2 <- 9; w2 <- 9
mar <- rep(0, 4)
par(mar = mar, oma = mar)
num <- 6

#### import data ################################
setwd(dirname(sys.frame(1)$ofile))
setwd("../../../../Sandra Goldlust's Work/Shared_Data/SG_covariate_data/Cleaned_Data")
# poverty data
popDat <- read_csv("clean_Census_popdensity_county.csv", col_types = "ccciidddddciccc") %>%
  filter(type == 'county') %>%
  select(year, fips_st, State, Abbreviation, fips, short_name, pop, housing_units, popDens_land, housDens_land) %>%
  rename(popDens = popDens_land, housDens = housDens_land, housUnits = housing_units) %>%
  mutate(region = as.numeric(fips))

#### plot variables ################################
varnames <- names(popDat)[c(7:10)]
leg.lab <- c("Total Population", "Housing Units", "Pop. Density per Sq. Mile", "Housing Density per Sq. Mile")
uqst <- popDat %>% select(State) %>% distinct %>% arrange(State) %>%
  mutate(for.plot = seq_along(1:nrow(.)))

fullDat2 <- left_join(popDat, uqst, by = "State")
indexes <- seq(1, max(fullDat2 %>% select(for.plot)), by=num)
years <- fullDat2 %>% select(year) %>% distinct %>% arrange(year) %>% unlist

#### plot setup ################################
setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/EDA_popDensity_Census_cty", showWarnings = FALSE)
setwd("../graph_outputs/EDA_popDensity_Census_cty")

#### choropleths ################################
dir.create("./choro", showWarnings = FALSE)
setwd("./choro")

for (yr in years){
  pltDat <- fullDat2 %>% filter(year == yr) 
  for (v in varnames) {
    pltDat2 <- pltDat %>% rename_(value = as.name(v))
    if (all(is.na(pltDat2$value))) {next}
    choro <- county_choropleth(pltDat2, legend = leg.lab[which(v == varnames)]) 
    ggsave(sprintf("%s_popDensity_Census_cty_%s.png", v, yr), choro, width = w, height = h, dpi = dp)
  }
} # 11/24/15

#### time series ################################
dir.create("../ts", showWarnings = FALSE)
setwd("../ts")

for(i in indexes){
  for(v in varnames){
    dummyplots <- ggplot(fullDat2 %>% filter(for.plot>= i & for.plot < i+num), aes(x=year, y=eval(parse(text = v)))) +
      theme_bw()+
      theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
      geom_line(aes(colour = fips)) +
      scale_y_continuous(name = leg.lab[which(v == varnames)]) +
      scale_x_continuous(breaks = seq(2000, 2010, 10), limits = c(1999, 2011)) +
      guides(colour = "none") +
      facet_wrap(~State)
    labs <- fullDat2 %>% filter(for.plot>= i & for.plot < i+num) %>% select(Abbreviation) %>% distinct %>% arrange(Abbreviation) %>% slice(c(i, i+num-1))  %>% unlist
    ggsave(sprintf("%s_popDensity_Census_cty_%s-%s.png", v, labs[1], labs[2]), dummyplots, width = w2, height = h2, dpi = dp)
  }
} # 11/24/15

