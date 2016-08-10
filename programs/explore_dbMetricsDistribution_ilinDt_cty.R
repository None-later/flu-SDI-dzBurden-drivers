## Name: Elizabeth Lee
## Date: 7/6/16
## Function: explore distributions of disease burden metrics for ilinDt at the county level
## Results: magnitude metrics could be truncated and shifted normals, but timing metrics don't appear to be normally distributed
### disease burden metrics: sum ILI across epidemic weeks, cumulative difference in ILI and baseline, cumulative difference in ILI and epidemic threshold, rate of ILI at epidemic peak, epidemic duration, time to epidemic from start of flu period, time to epidemic peak from start of epidemic
## Filenames: sprintf('dbMetrics_periodicReg_%silinDt%s_analyzeDB.csv', code, code2)
## Data Source: IMS Health 
## Notes: 

## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")


#### header ####################################
require(ggplot2)
require(readr)
require(dplyr)
require(tidyr)
setwd(dirname(sys.frame(1)$ofile))

source("source_clean_response_functions_cty.R") # functions to clean response and IMS coverage data (cty)

#### set these! ####################################
code <-"" # linear time trend term
code2 <- "_Octfit" # fit = Apr to Oct and fluseason = Oct to Apr
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"

# uncomment when running script separately
spatial <- list(scale = "county", stringcode = "County", stringabbr = "_cty")
span.var <- 0.4 # 0.4, 0.6
degree.var <- 2
code.str <- sprintf('_span%s_degree%s', span.var, degree.var)

#### FILEPATHS #################################
setwd('../reference_data')
path_abbr_st <- paste0(getwd(), "/state_abbreviations_FIPS.csv")
path_latlon_cty <- paste0(getwd(), "/cty_pop_latlon.csv")

setwd("../R_export")
path_response_cty <- paste0(getwd(), sprintf("/dbMetrics_periodicReg%s_analyzeDB_cty.csv", dbCodeStr))

# put all paths in a list to pass them around in functions
path_list <- list(path_abbr_st = path_abbr_st,
                  path_latlon_cty = path_latlon_cty,
                  path_response_cty = path_response_cty)


#### import data ####################################
iliSum <- cleanR_iliSum_cty(path_list)
iliPeak <- cleanR_iliPeak_cty(path_list)

#### plot formatting ####################################
w <- 9; h <- 6

#### plot distribution of dbMetrics ####################################
print(sprintf('plotting db metrics %s', code.str))
# 7/6/16 - saved figures
setwd(sprintf('../graph_outputs/EDA_IMS_burden_iliSum%s', spatial$stringabbr))

# total ILI plot
plt.distr.iliSum <- ggplot(iliSum, aes(x=y, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=10) + geom_density() + 
  # coord_cartesian(xlim=c(0, 250)) +
  facet_wrap(~season) + ggtitle("Sum ilinDt during flu season")
ggsave(sprintf("distr_ILITot_%silinDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), plt.distr.iliSum, width=w, height=h)


# ili peak case count plot
setwd(sprintf('../EDA_IMS_burden_iliPeak%s', spatial$stringabbr))
plt.distr.pkCount <- ggplot(iliPeak, aes(x=y, group=season)) +
  geom_histogram(aes(y=..density..), binwidth=5) + geom_density() + 
  # coord_cartesian(xlim=c(0, 50)) +
  facet_wrap(~season) + ggtitle("peak ilinDt count during flu season")
ggsave(sprintf("distr_pkCount_%silinDt%s%s%s.png", code, code2, code.str, spatial$stringabbr), plt.distr.pkCount, width=w, height=h)


print('finished plotting db metrics')

####################################
# compare the mean and variance for each metric by season
iliSum.summ <- iliSum %>% group_by(season) %>% summarise(MN = mean(y, na.rm=TRUE), VAR = var(y, na.rm=TRUE))
iliPk.summ <- iliPeak %>% group_by(season) %>% summarise(MN = mean(y, na.rm=TRUE), VAR = var(y, na.rm=TRUE))

print(sprintf('span %s degree %s', span.var, degree.var))
print(iliSum.summ)
print(iliPk.summ)



