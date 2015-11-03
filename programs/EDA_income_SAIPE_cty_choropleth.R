## Name: Elizabeth Lee
## Date: 11/2/15
## Function: EDA - SAIPE median household income - choropleth, county-level
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
mar <- rep(0, 4)

#### import data ################################
setwd("../../../../Sandra Goldlust's Work/Shared_Data/SG_covariate_data/Cleaned_Data")
# income data
incomeDat <- read_csv("clean_SAIPE_income.csv", col_types = list("state_id" = col_character(), "county_id" = col_character()), na = c("\\N")) %>%
  filter(type == 'county') 

setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/EDA_income_SAIPE_cty", showWarnings = FALSE)
setwd("../graph_outputs/EDA_income_SAIPE_cty")

par(mar = mar, oma = mar)
years <- c(1995, 1997:2013)
for (yr in years){
  pltDat <- incomeDat %>%
    filter(year == yr) %>%
    mutate(mi_bin = cut(med_income, breaks = quantile(med_income, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE)) %>%
    mutate(mi_bin = factor(mi_bin, levels = rev(levels(mi_bin)), labels = incVec)) %>% 
    mutate(income_color = factor(mi_bin, levels = levels(mi_bin), labels = colVec)) %>%
    mutate(inc_col_string = as.character(income_color))
  png(filename = sprintf("medHouseholdIncome_cty_%s.png", yr), height = h, width = w, units = "in", res = dp)
  map("county", fill = TRUE, col = pltDat$inc_col_string, xlim = c(-124, -65), ylim = c(20, 50))
  legend(list(x = -122.5, y = 24), legend = incVec, horiz = TRUE,  fill = levels(pltDat$income_color), cex = 0.75) # Tier 1= 120K to 140K income
  dev.off()
  print(pltDat %>% count(mi_bin))
}

# View(incomeDat %>% filter(name == 'Washington County (ME)'))
