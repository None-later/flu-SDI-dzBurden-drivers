## Name: Elizabeth Lee
## Date: 11/2/15
## Function: EDA - SAIPE poverty - choropleth, county-level; time series by county grouped by state
### disease burden metrics: total ili summed across epidemic weeks, cumulative difference in ili and baseline, cumulative difference in ili and epidemic threshold, rate of ili at epidemic peak, epidemic duration, weeks from Nov1 to epidemic start, weeks from epidemic start to peak ili week 
## Notes: "*_universe" means the population size from which the "*_counts" and "*_percent" are derived

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
require(mapproj)
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
setwd('../reference_data')
abbrDat <- read_csv("state_abbreviations_FIPS.csv", col_types = list(FIPS = col_character()))

setwd(dirname(sys.frame(1)$ofile))
setwd("../../../../Sandra Goldlust's Work/Shared_Data/SG_covariate_data/Cleaned_Data")
# poverty data
povDat <- read_csv("clean_SAIPE_poverty.csv", col_types = list("state_id" = col_character(), "county_id" = col_character()), na = c("\\N")) %>%
  filter(type == 'county') %>%
  select(year, state_id, county_id, name, contains("_universe"), contains("_percent")) %>%
  select(-contains("_percentLB"), -contains("_percentUB90"))
# merge data together
fullDat <- left_join(povDat, abbrDat, by = c("state_id" = "FIPS")) %>%
  mutate(cty = tolower(substring(name, 1, nchar(name)-12))) %>%
  mutate(match = paste(tolower(State), cty, sep=',')) %>%
  rename(allPovPerc = all_poverty_percent, u18PovPerc = under18_poverty_percent, b5_17PovPerc = btwn517_poverty_percent, u5PovPerc = under5_poverty_percent)

#### plot variables ################################
varnames <- names(fullDat)[c(9:11)]
leg.lab <- c("All poverty (%)", "Under 18 in poverty (%)", "5-17 in poverty (%)")
uqst <- fullDat %>% select(State) %>% distinct %>% arrange(State) %>%
  mutate(for.plot = seq_along(1:nrow(.)))

fullDat2 <- left_join(fullDat, uqst, by = "State")
indexes <- seq(1, max(fullDat2 %>% select(for.plot)), by=num)
years <- fullDat2 %>% select(year) %>% distinct %>% arrange(year) %>% unlist

#### plot setup ################################
setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/EDA_poverty_SAIPE_cty", showWarnings = FALSE)
setwd("../graph_outputs/EDA_poverty_SAIPE_cty")

#### choropleths ################################
dir.create("./choro", showWarnings = FALSE)
setwd("./choro")

for (yr in years){
  pltDat <- fullDat2 %>% filter(year == yr) 
  for (v in varnames) {
    pltDat2 <- pltDat %>% rename_(value = as.name(v))
    if (all(is.na(pltDat2$value))) {next}
    else{if (dim(pltDat2 %>% filter(!is.na(value)))[1] < 5) {next} # skip if there are fewer than five counties with data
    pltDat3 <- pltDat2 %>% 
      mutate(val_bin = cut(value, breaks = quantile(value, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE)) %>%
      mutate(val_bin = factor(val_bin, levels = rev(levels(val_bin)), labels = tierVec)) %>% 
      mutate(val_color = factor(val_bin, levels = levels(val_bin), labels = colVec)) %>%
      mutate(val_col_string = as.character(val_color))
    
    pltOrder <- pltDat3$county_id[match(map("county", plot=FALSE)$names, pltDat3$match)]
    colOrder <- pltDat3$val_col_string[match(pltOrder, pltDat3$county_id)]
    
    png(filename = sprintf("%s_poverty_SAIPE_cty_%s.png", v, yr), height = h, width = w, units = "in", res = dp)
    map("county", fill = TRUE, col = colOrder, xlim = c(-124, -65), ylim = c(20, 50), lwd = 0.1)
    legend(list(x = -122.5, y = 24), legend = tierVec, horiz = TRUE,  fill = levels(pltDat3$val_color), cex = 0.75) # Tier 1= 120K to 140K income
    dev.off()
    }
  }
} # 11/22/15

#### time series ################################
dir.create("../ts", showWarnings = FALSE)
setwd("../ts")

for(i in indexes){
  for(v in varnames){
    dummyplots <- ggplot(fullDat2 %>% filter(for.plot>= i & for.plot < i+num), aes(x=year, y=eval(parse(text = v)))) +
      theme_bw()+
      theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
      geom_line(aes(colour = county_id)) +
      geom_point(aes(colour = county_id)) +
      scale_y_continuous(name = leg.lab[which(v == varnames)]) +
      guides(colour = "none") +
      coord_cartesian(xlim = c(1989, 2013)) +
      facet_wrap(~State)
    labs <- fullDat2 %>% filter(for.plot>= i & for.plot < i+num) %>% select(Abbreviation) %>% distinct %>% arrange(Abbreviation) %>% slice(c(i, i+num-1))  %>% unlist
    ggsave(sprintf("%s_poverty_SAIPE_cty_%s-%s.png", v, labs[1], labs[2]), dummyplots, width = w2, height = h2, dpi = dp)
  }
} # 11/22/15