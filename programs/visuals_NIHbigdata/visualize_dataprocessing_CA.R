## Name: Elizabeth Lee
## Date: 11/1/15
## Function: time series and disease burden display for data processing section in NIH big data poster - constrained to a few select zip3s in the SF Bay Area
### disease burden metrics: total ili summed across epidemic weeks, cumulative difference in ili and baseline, cumulative difference in ili and epidemic threshold, rate of ili at epidemic peak, epidemic duration, weeks from Nov1 to epidemic start, weeks from epidemic start to peak ili week 

## Filenames: periodicReg_%sallZip3Mods_ilicnDt_Oct.csv
## Data Source: IMS Health 
## Notes: 9/15/15 - Refer to explore_fluSeasonDefinition_ILI.R for definition of "flu epidemic". Zip3s are considered to have experienced a flu epidemic if they had 4+ consecutive weeks above the epidemic threshold in the flu period. 
## 10/20/15 - Split program with write_fullIndic_periodicReg_ilicnDt.R (run that first)
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")
rm(list = ls())
#### header ####################################
require(dplyr)
require(ggplot2)
require(readr)
require(tidyr)
require(grid)
setwd(dirname(sys.frame(1)$ofile))

#### set these! ####################################
code <-"" # linear time trend term
code2 <- "_Octfit"

span.var <- 0.5 
degree.var <- 2
code.str <- sprintf('_span%s_degree%s', span.var, degree.var)
zipset <- c('941', '946', '947')
w <- 6; h <- 1.25
fontsz <- 10
mar <- c(0,0,0,0)

#### import data ##################
setwd('../../R_export/')
dbMetrics.g <- read.csv(sprintf('dbMetrics_periodicReg_%silicnDt%s%s_analyzeDB.csv', code, code2, code.str), header=T, colClasses=c(zip3="character", metric="character")) %>% 
  filter(season!=1 & zip3 %in% zipset) %>%
  mutate(zipname = ifelse(zip3 == '941', 'San Francisco (941)', ifelse(zip3 == '946', 'Oakland (946)', ifelse(zip3 == '947', 'Berkeley (947)', NA))))

#### plot time series data processing ##################
setwd('./dataSnapshot_NIHbigdata')
fullD <- read_csv(sprintf('fullIndicAll_CA_periodicReg_%silicnDt%s%s_analyzeDB.csv', code, code2, code.str), col_names = T, col_types = list(zip3 = col_character(), ili = col_integer(), pop = col_integer(), cov_z.y = col_double(), alpha_z.y = col_double(), ILIc = col_double(), cov_below5 = col_logical(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilicn.dt = col_double(), ILIcn = col_double())) %>%
  filter(season != 10)

caD <- fullD %>% filter(zip3 %in% zipset) %>%
  mutate(zipname = ifelse(zip3 == '941', 'San Francisco (941)', ifelse(zip3 == '946', 'Oakland (946)', ifelse(zip3 == '947', 'Berkeley (947)', NA)))) %>%
  mutate(ili.filt = ifelse(incl.analysis, ili, NA))

setwd('../../graph_outputs/visuals_NIHbigdata')
# plot raw ILI
iliP <- ggplot(caD, aes(x = Thu.week, y = ili, group = zipname)) +
  geom_line(aes(color = flu.week)) +
  scale_color_discrete(name = 'Potential Flu Season Period') +
  scale_y_continuous(name = 'ILI') +
  scale_x_date(name = 'Week') +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm"), legend.position = "bottom", axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
  facet_wrap(~zipname, nrow = 1) +
  guides(color = "none")
ggsave('rawILI_ts_CA.png', iliP, width = w, height = h, dpi = 450)
# x11()
# print(iliP)
  
# ili.filtP <- ggplot(caD, aes(x = Thu.week, y = ili.filt, group = zipname)) +
#   geom_line(aes(color = flu.week)) +
#   scale_color_discrete(name = 'Potential Flu Season Period') +
#   scale_y_continuous(name = 'filtered ILI') +
#   scale_x_date(name = 'Week') +
#   theme_bw(base_size = fontsz, base_family = "") +
#   theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm"), legend.position = "bottom") +
#   facet_wrap(~zipname, nrow = 1) +
#   guides(color = "none")
# ggsave('filtILI_ts_CA.png', ili.filtP, width = w, height = h, dpi = 450)
# # x11()
# # print(ili.filtP)

ilicn.P <- ggplot(caD, aes(x = Thu.week, y = ILIcn, group = zipname)) +
  geom_line(aes(color = flu.week)) +
  geom_line(aes(y = .fittedLoess), colour = 'black', size = 1) +
  scale_color_discrete(name = 'Potential Flu Season Period') +
  scale_y_continuous(name = 'ILIcn') +
  scale_x_date(name = 'Week') +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm"), legend.position = "bottom", axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
  facet_wrap(~zipname, nrow = 1) +
  guides(color = "none")
ggsave('ilicn_ts_CA.png', ilicn.P, width = w, height = h, dpi = 450)
print(ilicn.P)
  
ilicnDt.P <- ggplot(caD, aes(x = Thu.week, y = ilicn.dt, group = zipname)) +
  geom_line(aes(color = flu.week)) +
  geom_line(aes(y = .fitted), colour = 'black', size = 1) +
  scale_color_discrete(name = 'Potential Flu Season Period') +
  scale_y_continuous(name = 'ILIcn.dt') +
  scale_x_date(name = 'Week') +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm"), axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom", legend.margin = unit(-2, "mm")) +
  facet_wrap(~zipname, nrow = 1) 
ggsave('ilicnDt_ts_CA.png', ilicnDt.P, width = w, height = h+.75, dpi = 450)
print(ilicnDt.P)  

ilicnDt.inSeas.P <- ggplot(caD, aes(x = Thu.week, y = ilicn.dt, group = zipname)) +
  geom_line(aes(color = in.season)) +
  scale_color_brewer(name = 'Constrained Flu Period', palette = 'Set1') +
  scale_y_continuous(name = 'ILIcn') +
  scale_x_date(name = 'Week') +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm"), legend.position = "bottom", legend.margin = unit(-2, "mm")) +
  facet_wrap(~zipname, nrow = 1) 
ggsave('ilicnDt_inSeas_ts_CA.png', ilicnDt.inSeas.P, width = w, height = h+.75, dpi = 450)
print(ilicnDt.inSeas.P)  


#### plot disease burden metrics ##################
w2 <- 2.5; h2 <- 3.5

#### plot distribution of dbMetrics ####################################
print(sprintf('plotting db metrics %s', code.str))

# total ILI plot
plt.distr.iliSum <- ggplot(dbMetrics.g %>% filter(metric=='ilicnDt.sum'), aes(x=burden, group=zipname)) +
  geom_histogram(binwidth=1) +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm")) +
  scale_x_continuous(name = "Seasonal ILI rate intensity") +
  scale_y_continuous(name = "Number of seasons", breaks = c(0, 1, 2)) +
  facet_wrap(~zipname, nrow = 3) 
ggsave("ilicnDt_dbMag_CA.png", plt.distr.iliSum, width=w2, height=h2, dpi = 450)
print(plt.distr.iliSum)
  