
## Name: Elizabeth Lee
## Date: 11/16/15
## Function: Plot county-level time series and choropleths for health insurance 
## Filenames: SG_covariate_data/Cleaned_Data/clean_HI_SAHIE_aggregate_CPS.csv
## Data Source: 
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(readr)
require(tidyr)
require(ggplot2)
require(dplyr)
require(choroplethr)
require(choroplethrMaps)
require(grid)
require(gridExtra)

setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program

#### import data ################################
setwd('../reference_data')
abbrDat <- read_csv("state_abbreviations_FIPS.csv", col_types = list(FIPS = col_character()))

setwd(dirname(sys.frame(1)$ofile))
setwd("../../../../Sandra Goldlust's Work/Shared_Data/SG_covariate_data/Cleaned_Data/")
sahieCPS <- read_csv("clean_HI_SAHIE_aggregate_CPS.csv", col_types = "ciiiiiiidididddddddddcc") %>%
  filter(type == 'county') %>%
  mutate(region = as.numeric(county_id)) %>%
  mutate(pctui.calc = nui/pop*100) %>%
  mutate(value = pctui.calc) %>%
  left_join(abbrDat, by = c("state_id" = "FIPS"))


#### plot formatting ################################
w <- 6; h <- 4; dp <- 300
w2 <- 9; h2 <- 9
num <- 6
years <- paste0('X', 2005:2007)
choro <- list()

#### clean data ################################
uqSt <- sahieCPS %>% select(state_id) %>% unique %>% 
  mutate(for.plot = seq_along(1:nrow(.)))

fullDat <- sahieCPS %>%
  select(year, region, value, county_id, Abbreviation, state_id) %>%
  left_join(uqSt, by = "state_id")
indexes <- seq(1, max(fullDat %>% select(for.plot)), by=num)

#### choropleth ################################
setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/EDA_uninsured_SAHIE_cty", showWarnings = F)
setwd("../graph_outputs/EDA_uninsured_SAHIE_cty")

for (y in years){
  pltDat <- fullDat %>%
    filter(year == substr.Right(y, 4))
  choro[[eval(y)]] <- county_choropleth(pltDat, legend = "Uninsured (%)") 
  ggsave(sprintf("pctui_SAHIE_CPS_cty_%s.png", substr.Right(y, 4)), choro[[eval(y)]], width = w, height = h, dpi = dp)
}

# # not easy to format everything in the same figure
# png(filename = "pctui_cty_0813.png", height = h, width = w*3, units = "in", res = dp)
# plts <- grid.arrange(grobs = choro, widths = unit(rep(w, 3), "in"), nrow = 2)
# dev.off()

#### time series ################################
for(i in indexes){
  dummyplots <- ggplot(fullDat %>% filter(for.plot>= i & for.plot < i+num), aes(x=year, y=value)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(colour = county_id)) +
    scale_y_continuous(name = "Uninsured (%)") +
    guides(colour = "none") +
    coord_cartesian(ylim = c(0, 45)) +
    facet_wrap(~Abbreviation)
  labs <- fullDat %>% filter(for.plot>= i & for.plot < i+num) %>% select(Abbreviation) %>% distinct %>% slice(c(i, i+num-1))  %>% unlist
  ggsave(sprintf("pctui_SAHIE_CPS_cty_%s-%s.png", labs[1], labs[2]), dummyplots, width = w2, height = h2, dpi = dp)

}