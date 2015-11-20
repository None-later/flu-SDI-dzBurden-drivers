## Name: Elizabeth Lee
## Date: 11/2/15
## Function: EDA - SAIPE median household income - choropleth, county-level; time series by county grouped by state
### disease burden metrics: total ili summed across epidemic weeks, cumulative difference in ili and baseline, cumulative difference in ili and epidemic threshold, rate of ili at epidemic peak, epidemic duration, weeks from Nov1 to epidemic start, weeks from epidemic start to peak ili week 


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
incVec <- paste("Tier", 1:5)
colVec <- brewer.pal(length(incVec), 'RdYlGn')
h <- 5; w <- 8; dp <- 450
h2 <- 9; w2 <- 9
mar <- rep(0, 4)
par(mar = mar, oma = mar)
years <- c(1995, 1997:2013)
num <- 6

#### import data ################################
setwd('../reference_data')
abbrDat <- read_csv("state_abbreviations_FIPS.csv", col_types = list(FIPS = col_character()))

setwd(dirname(sys.frame(1)$ofile))
setwd("../../../../Sandra Goldlust's Work/Shared_Data/SG_covariate_data/Cleaned_Data")
# income data
incomeDat <- read_csv("clean_SAIPE_income.csv", col_types = list("state_id" = col_character(), "county_id" = col_character()), na = c("\\N")) %>%
  filter(type == 'county')
  
fullDat <- left_join(incomeDat, abbrDat, by = c("state_id" = "FIPS")) %>%
  mutate(cty = tolower(substring(name, 1, nchar(name)-12))) %>%
  mutate(match = paste(tolower(State), cty, sep=','))

uqst <- fullDat %>% select(State) %>% distinct %>% arrange(State) %>%
  mutate(for.plot = seq_along(1:nrow(.)))

fullDat2 <- left_join(fullDat, uqst, by = "State")
indexes <- seq(1, max(fullDat2 %>% select(for.plot)), by=num)

#### plot setup ################################
setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/EDA_income_SAIPE_cty", showWarnings = FALSE)
setwd("../graph_outputs/EDA_income_SAIPE_cty")

#### choropleths ################################
dir.create("./choro", showWarnings = FALSE)
setwd("./choro")
for (yr in years){
  pltDat <- fullDat %>%
    filter(year == yr) %>%
    mutate(mi_bin = cut(med_income, breaks = quantile(med_income, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE)) %>%
    mutate(mi_bin = factor(mi_bin, levels = rev(levels(mi_bin)), labels = incVec)) %>% 
    mutate(income_color = factor(mi_bin, levels = levels(mi_bin), labels = colVec)) %>%
    mutate(inc_col_string = as.character(income_color))
  
  pltOrder <- pltDat$county_id[match(map("county", plot=FALSE)$names, pltDat$match)]
  colOrder <- pltDat$inc_col_string[match(pltOrder, pltDat$county_id)]
  
  png(filename = sprintf("medHouseholdIncome_SAIPE_cty_%s.png", yr), height = h, width = w, units = "in", res = dp)
  map("county", fill = TRUE, col = colOrder, xlim = c(-124, -65), ylim = c(20, 50), lwd = 0.1)
  legend(list(x = -122.5, y = 24), legend = incVec, horiz = TRUE,  fill = levels(pltDat$income_color), cex = 0.75) # Tier 1= 120K to 140K income
  dev.off()
  print(pltDat %>% count(mi_bin))
} # renamed 11/20/15

# View(incomeDat %>% filter(name == 'Washington County (ME)'))

#### time series ################################
dir.create("../ts", showWarnings = FALSE)
setwd("../ts")

for(i in indexes){
    dummyplots <- ggplot(fullDat2 %>% filter(for.plot>= i & for.plot < i+num), aes(x=year, y=med_income)) +
      theme_bw()+
      theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
      geom_line(aes(colour = name)) +
      geom_point(aes(colour = name)) +
      scale_y_continuous(name = "Median Household Income") +
      guides(colour = "none") +
      coord_cartesian(xlim = c(1995, 2013)) +
      facet_wrap(~State)
    labs <- fullDat2 %>% filter(for.plot>= i & for.plot < i+num) %>% select(Abbreviation) %>% distinct %>% slice(c(i, i+num-1))  %>% unlist
    ggsave(sprintf("medHouseholdIncome_SAIPE_cty_%s-%s.png", labs[1], labs[2]), dummyplots, width = w2, height = h2, dpi = dp)
} # saved 11/20/15


