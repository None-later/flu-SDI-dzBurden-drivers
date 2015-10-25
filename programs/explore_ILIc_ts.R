
## Name: Elizabeth Lee
## Date: 10/24/15
## Function: explore ilic time series data: ilic_{z, w} = ili_{z, w} / alpha_{z, y} / effPhysCov_{z, y}
## ilic --> number of ili cases in zip z in week w, correcting for constant care-seeking rates across zip3s and scaling up for physician coverage; scaling up assumes that the ili/physician ratio is the same for the reported and unreported cases
## alpha_{z, y} = (viz_{z, y}/numPhys_{z, y}) / (\bar{viz_y}/\bar{phys_y}) --> correction for general care-seeking behavior

## Filenames: R_export/ilicByallZip_allWeekly_totServ_totAge.csv
## Data Source: IMS Health ili dataset and physician coverage dataset
## Notes: 
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header ####################################
rm(list = ls())
require(dplyr)
require(ggplot2)
require(tidyr)
setwd(dirname(sys.frame(1)$ofile))

#### plot formatting ################################
w <- 9; h <- 6
num <- 6

#### import data ################################
setwd('../R_export')
ilic_df <- read_csv('ilicByallZip_allWeekly_totServ_totAge.csv', col_types = list("zip3" = col_character(), ili = col_integer(), pop = col_integer(), cov_z.y = col_double(), alpha_z.y = col_double(), ILIc = col_double(), cov_below5 = col_logical()))

#### prepare data for plotting ################################
zip3list <- ilic_df %>% filter(incl.lm) %>% select(zip3) %>% distinct %>% mutate(for.plot = seq_along(1:nrow(.))) 
data_plot <- right_join(ilic_df, zip3list, by="zip3") %>%
  filter(Thu.week < as.Date('2009-05-01')) %>%
  mutate(covIndic = ifelse(cov_below5, 0, NA))
indexes <- seq(1, max(data_plot %>% select(for.plot)), by=num)

#### plot data ################################
dir.create('../graph_outputs/explore_ILIc_ts', showWarnings = FALSE)
setwd('../graph_outputs/explore_ILIc_ts')

for(i in indexes){
  dummyplots <- ggplot(data_plot %>% filter(for.plot>= i & for.plot < i+num), aes(x=Thu.week, y=ILIc, group=zip3)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = flu.week)) + scale_color_discrete(name='flu.week') + 
    geom_line(aes(y = covIndic)) + 
    facet_wrap(~zip3, scales="free_y")
  # grab zip3s in plot for file name
  ziplabels <- data_plot %>% select(zip3) %>% distinct %>% slice(c(i, i+num-1)) 
  ggsave(sprintf("ilic_ts_%s-%s.png", ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
} # saved 10/25/15

