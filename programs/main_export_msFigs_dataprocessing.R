## Name: Elizabeth Lee
## Date: 4/8/17
## Function: time series and disease burden display for data processing section in MS appendix - constrained to a few counties
### disease burden metrics: total ili summed across epidemic weeks, cumulative difference in ili and baseline, cumulative difference in ili and epidemic threshold, rate of ili at epidemic peak, epidemic duration, weeks from Nov1 to epidemic start, weeks from epidemic start to peak ili week 

## Filenames: 
## Data Source: IMS Health 
## Notes: 
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
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
seasons <- 3:9

fipset <- c('06075', '36061')
w <- 4.5; h <- 1.25; dp <- 300
fontsz <- 10
mar <- c(0,1,1,0)

#### import data ##################
setwd('../R_export/')
dbDat <- read_csv(sprintf('dbMetrics_periodicReg%s_analyzeDB_cty.csv', dbCodeStr)) %>% 
  filter(season %in% seasons) %>%
  filter(fips %in% fipset) %>%
  mutate(fipsname = ifelse(fips == '06075', 'San Francisco', ifelse(fips == '36061', 'Manhattan', NA)))

tsDat <- read_csv(sprintf('fullIndicAll_periodicReg%s_analyzeDB_cty.csv', dbCodeStr)) %>%
  filter(season %in% seasons) %>%
  filter(fips %in% fipset) %>%
  mutate(fipsname = ifelse(fips == '06075', 'San Francisco', ifelse(fips == '36061', 'Manhattan', NA)))

#### paths ##################
setwd(dirname(sys.frame(1)$ofile))
setwd('../graph_outputs/msFigs/dataprocessing')

#### plots ##################

ilin.P <- ggplot(tsDat, aes(x = Thu.week, y = ILIn, group = fipsname)) +
  geom_line(aes(color = flu.week)) +
  scale_color_discrete(name = 'Potential Flu Season Period') +
  scale_y_continuous(name = 'ILI rate per 10K') +
  scale_x_date(name = 'Week') +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = margin(mar), legend.position = "bottom", axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
  facet_wrap(~fipsname, nrow = 1) +
  guides(color = "none")
ggsave('1_ilin_ts_noline.png', ilin.P, width = w, height = h, dpi = dp)
print(ilin.P)

ilin.P2 <- ggplot(tsDat, aes(x = Thu.week, y = ILIn, group = fipsname)) +
  geom_line(aes(color = flu.week)) +
  geom_line(aes(y = .fittedLoess), colour = 'black', size = 1) +
  scale_color_discrete(name = 'Potential Flu Season Period') +
  scale_y_continuous(name = 'ILI rate per 10K') +
  scale_x_date(name = 'Week') +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = margin(mar), legend.position = "bottom", axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
  facet_wrap(~fipsname, nrow = 1) +
  guides(color = "none")
ggsave('2_ilin_ts_line.png', ilin.P2, width = w, height = h, dpi = dp)
print(ilin.P2)

ilinDt.P <- ggplot(tsDat, aes(x = Thu.week, y = ilin.dt, group = fipsname)) +
  geom_line(aes(color = flu.week)) +
  scale_color_discrete(name = 'Potential Flu Season Period') +
  scale_y_continuous(name = 'Detrended ILI') +
  scale_x_date(name = 'Week') +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = margin(mar), axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom", legend.margin = margin(mar)) +
  facet_wrap(~fipsname, nrow = 1) +
  guides(color = "none")
ggsave('3_ilinDt_ts_noline.png', ilinDt.P, width = w, height = h, dpi = dp)
print(ilinDt.P)

ilinDt.P2 <- ggplot(tsDat, aes(x = Thu.week, y = ilin.dt, group = fipsname)) +
  geom_line(aes(color = flu.week)) +
  geom_line(aes(y = .fitted), colour = 'black', size = 1) +
  scale_color_discrete(name = 'Potential Flu Season Period') +
  scale_y_continuous(name = 'Detrended ILI') +
  scale_x_date(name = 'Week') +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = margin(mar), axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom", legend.margin = margin(mar)) +
  facet_wrap(~fipsname, nrow = 1) #+
  # guides(color = "none")
ggsave('4_ilinDt_ts_line_legend.png', ilinDt.P2, width = w, height = h, dpi = dp)

# ilinDt.inSeas.P <- ggplot(tsDat, aes(x = Thu.week, y = ilin.dt, group = fipsname)) +
#   geom_line(aes(color = in.season)) +
#   scale_color_brewer(name = 'Influenza Epidemic Period', palette = 'Set1') +
#   scale_y_continuous(name = 'Detrended ILI per 10K') +
#   scale_x_date(name = 'Week') +
#   theme_classic(base_size = fontsz, base_family = "") +
#   theme(panel.grid.minor = element_blank(), plot.margin = margin(mar), axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom", legend.margin = margin(mar)) +
#   facet_wrap(~fipsname, nrow = 1) +
#   guides(color = "none")
# ggsave('5_ilinDt_inSeas_ts.png', ilinDt.inSeas.P, width = w, height = h, dpi = dp)

ilinDt.inSeas.P2 <- ggplot(tsDat, aes(x = Thu.week, y = ilin.dt, group = fipsname)) +
  geom_line(aes(color = in.season)) +
  scale_color_brewer(name = 'Influenza Epidemic Period', palette = 'Set1') +
  scale_y_continuous(name = 'Detrended ILI') +
  scale_x_date(name = 'Week') +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = margin(mar), axis.title.x = element_blank(), legend.position = "bottom", legend.spacing = unit(mar)) +
  facet_wrap(~fipsname, nrow = 1) #+
  # guides(color = "none")
ggsave('5_ilinDt_inSeas_ts_wTime.png', ilinDt.inSeas.P2, width = w, height = h+.55, dpi = dp)

# total ILI plot
plt.distr.iliSum <- ggplot(dbDat %>% filter(metric=='ilinDt.sum'), aes(x=burden, group=fipsname)) +
  geom_histogram(binwidth=1) +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = margin(mar)) +
  scale_x_continuous(name = "Seasonal Intensity") +
  scale_y_continuous(name = "Num. seasons", breaks = c(0, 1, 2)) +
  facet_wrap(~fipsname, nrow = 1)
ggsave("6_ilinDt_dbMag.png", plt.distr.iliSum, width=w, height=h+.25, dpi = dp)
print(plt.distr.iliSum)


# # WIPS seminar version (front matter) #
# ili.P <- ggplot(caD, aes(x = Thu.week, y = ili, group = zipname)) +
#   geom_line(aes(color = flu.week)) +
#   scale_color_discrete(name = 'Potential Flu Season Period') +
#   scale_y_continuous(name = 'Reports of ILI') +
#   scale_x_date(name = 'Week') +
#   theme_classic(base_size = fontsz, base_family = "") +
#   theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm"), axis.title.x = element_blank(), legend.position = "bottom", legend.margin = unit(-2, "mm")) +
#   facet_wrap(~zipname, nrow = 1) +
#   guides(color = 'none')
# ggsave('WIPS_ili_ts_CA.png', ili.P, width = w, height = h+.75, dpi = 450)
# 
# iliP <- ggplot(caD, aes(x = Thu.week, y = ili, group = zipname)) +
#   geom_line(aes(color = flu.week)) +
#   scale_color_discrete(name = 'Potential Flu Season Period') +
#   scale_y_continuous(name = 'Reports of ILI') +
#   scale_x_date(name = 'Week') +
#   theme_classic(base_size = fontsz, base_family = "") +
#   theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm"), legend.position = "bottom", axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
#   facet_wrap(~zipname, nrow = 1) +
#   guides(color = "none")
# ggsave('rawILI_ts_CA.png', iliP, width = w, height = h, dpi = 450)
 