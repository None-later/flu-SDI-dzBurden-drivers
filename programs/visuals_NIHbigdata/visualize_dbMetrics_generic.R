## Name: Elizabeth Lee
## Date: 11/2/15
## Function: generic graphical representations of disease burden
### disease burden metrics: total ili summed across epidemic weeks, cumulative difference in ili and baseline, cumulative difference in ili and epidemic threshold, rate of ili at epidemic peak, epidemic duration, weeks from Nov1 to epidemic start, weeks from epidemic start to peak ili week 


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
require(RColorBrewer)
require(gridExtra)
setwd(dirname(sys.frame(1)$ofile))

#### set these! ####################################
code <-"" # linear time trend term
code2 <- "_Octfit"

span.var <- 0.5 
degree.var <- 2
code.str <- sprintf('_span%s_degree%s', span.var, degree.var)
zipset <- c('941', '946', '947')
w <- 2; h <- 2; wleg <- 3.4
fontsz <- 10
mar <- c(0,0,0,0)
sz <- 1
dp <- 450
aOverlay <- 0.65

tsCol <- 'black'
naCol <- 'grey50'
falsCol <- '#a6cee3'
truCol <- '#cb181d'
truColLt <- '#fcbba1'
ylab <- 'Flu incidence'

#### import data ##################
setwd('../../R_export/dataSnapshot_NIHbigdata')

exampD <- read_csv(sprintf('fullIndicAll_CA_periodicReg_%silicnDt%s%s_analyzeDB.csv', code, code2, code.str), col_names = T, col_types = list(zip3 = col_character(), ili = col_integer(), pop = col_integer(), cov_z.y = col_double(), alpha_z.y = col_double(), ILIc = col_double(), cov_below5 = col_logical(), .fitted = col_double(), .se.fit = col_double(), .fittedLoess = col_double(), .se.fitLoess = col_double(), ilicn.dt = col_double(), ILIcn = col_double())) %>%
  filter(!is.na(zip3)) %>%
  filter(zip3 == '946' & season == 4) %>%
  mutate(zipname = ifelse(zip3 == '941', 'San Francisco (941)', ifelse(zip3 == '946', 'Oakland (946)', ifelse(zip3 == '947', 'Berkeley (947)', NA)))) %>%
  mutate(iliPeak = ifelse(ilicn.dt == max(ilicn.dt), T, ifelse(flu.week, F, NA))) %>%
  mutate(iliEarly = ifelse(Thu.week < "2003-12-01", T, F))

#### plot magnitude metrics ##################
setwd('../../graph_outputs/visuals_NIHbigdata')

iliSum.plt <- ggplot(exampD, aes(x = Thu.week, y = ilicn.dt)) +
  geom_bar(aes(fill = in.season), stat = 'identity') +
  scale_fill_manual(values = c('TRUE' = truCol, 'FALSE' = falsCol), na.value = naCol) +
  geom_line(colour = tsCol, size = sz) +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm"), axis.ticks = element_blank(), axis.text = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_x_date("Time") +
  scale_y_continuous(ylab) +
  guides(fill = "none") +
  ggtitle("Epidemic intensity")
ggsave("dbGeneric_iliSum.png", iliSum.plt, width = w, height = h, dpi = dp)
print(iliSum.plt)

iliPeak.plt <- ggplot(exampD, aes(x = Thu.week, y = ilicn.dt)) +
  geom_bar(aes(fill = iliPeak), stat = 'identity') +
  scale_fill_manual(name = "Legend", values = c('TRUE' = truCol, 'FALSE' = falsCol), breaks = c('TRUE', 'FALSE'), labels = c('Burden', 'Winter period\nNov. - Apr.'), na.value = naCol) +
  geom_line(colour = tsCol, size = sz) +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm"), axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "right", legend.spacing = unit(0, "mm"), plot.title = element_text(hjust = 0.5)) +
  scale_x_date("Time") +
  scale_y_continuous(ylab) +
  guides(fill = "none") +
  ggtitle("Peak intensity")
ggsave("dbGeneric_iliPeak.png", iliPeak.plt, width = w, height = h, dpi = dp)
print(iliPeak.plt)

iliEarly.plt <- ggplot(exampD, aes(x = Thu.week, y = ilicn.dt)) +
  geom_bar(aes(fill = iliEarly), stat = 'identity') +
  scale_fill_manual(name = "Legend", values = c('TRUE' = truCol, 'FALSE' = falsCol), breaks = c('TRUE', 'FALSE'), labels = c('Burden', 'Winter period\nNov. - Apr.'), na.value = naCol) +
  geom_line(colour = tsCol, size = sz) +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm"), axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "right", legend.spacing = unit(0, "mm"), plot.title = element_text(hjust = 0.5)) +
  scale_x_date("Time") +
  scale_y_continuous(ylab) +
  guides(fill = "none") +
  ggtitle("Onset intensity")
ggsave("dbGeneric_iliEarly.png", iliEarly.plt, width = w, height = h, dpi = dp)
print(iliEarly.plt)

aboveThresh.plt <- ggplot(exampD, aes(x = Thu.week, y = ilicn.dt)) +
  geom_bar(aes(fill = in.season), stat = 'identity') +
  geom_bar(aes(y = .fitted+(1.96*.se.fit)), stat = 'identity', fill = naCol, alpha = aOverlay) +
  geom_line(aes(y = .fitted+(1.96*.se.fit)), colour = naCol, size = sz - 0.5) +
  scale_fill_manual(values = c('TRUE' = truCol, 'FALSE' = falsCol), na.value = naCol) +
  geom_line(colour = tsCol, size = sz) +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm"), axis.ticks = element_blank(), axis.text = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_x_date("Time") +
  scale_y_continuous(ylab) +
  guides(fill = "none") +
  ggtitle("ILI above threshold")
ggsave("dbGeneric_aboveThresh.png", aboveThresh.plt, width = w, height = h, dpi = dp)
print(aboveThresh.plt)

#### gridded magnitude metrics plots ##################
png(filename = "dbGeneric_allMagnitude.png", height = h, width = w*2+wleg, units = "in", res = dp)
mag.plts <- grid.arrange(grobs = list(iliSum.plt, aboveThresh.plt, iliPeak.plt), widths = unit(c(w, w, wleg), "in"), nrow = 1)
dev.off()

png(filename = "dbGeneric_allMagnitude2.png", height = h, width = w+wleg, units = "in", res = dp)
mag.plts <- grid.arrange(grobs = list(iliSum.plt, iliPeak.plt), widths = unit(c(w, wleg), "in"), nrow = 1)
dev.off()

#### duration Markers ##################
# first potential flu season week
startFlu <- exampD %>% filter(flu.week) %>% select(Thu.week) %>% filter(Thu.week == min(Thu.week)) %>% unlist
startFlu <- as.Date(startFlu, origin = "1970-01-01")
# last potential flu season week
endFlu <- exampD %>% filter(flu.week) %>% select(Thu.week) %>% filter(Thu.week == max(Thu.week)) %>% unlist
endFlu <- as.Date(endFlu, origin = "1970-01-01")
# first in season epidemic week
startEpi <- exampD %>% filter(in.season) %>% select(Thu.week) %>% filter(Thu.week == min(Thu.week)) %>% unlist
startEpi <- as.Date(startEpi, origin = "1970-01-01")
# last in season epidemic week
endEpi <- exampD %>% filter(in.season) %>% select(Thu.week) %>% filter(Thu.week == max(Thu.week)) %>% unlist
endEpi <- as.Date(endEpi, origin = "1970-01-01")
# peak in season epidemic week
pkEpi <- exampD %>% filter(in.season) %>% filter(ilicn.dt == max(ilicn.dt)) %>% select(Thu.week) %>% unlist
pkEpi <- as.Date(pkEpi, origin = "1970-01-01")

# , arrow = arrow(type = "open", end = "both", length = unit(0.04, "inches"))

#### plot timing metrics ##################
epiDur.plt <- ggplot(exampD, aes(x = Thu.week, y = ilicn.dt)) +
  geom_bar(aes(fill = in.season), stat = 'identity') +
  scale_fill_manual(values = c('TRUE' = truColLt, 'FALSE' = falsCol), na.value = naCol) +
  geom_line(colour = tsCol, size = sz) +
  geom_segment(aes(x = startEpi, y = 6.75, xend = endEpi, yend = 6.75), colour = truCol, size = sz) +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm"), axis.ticks = element_blank(), axis.text = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_x_date("Time") +
  scale_y_continuous(ylab) +
  guides(fill = "none") +
  ggtitle("Epidemic duration")
ggsave("dbGeneric_epiDur.png", epiDur.plt, width = w, height = h, dpi = dp)
print(epiDur.plt)

timeToStart.plt <- ggplot(exampD, aes(x = Thu.week, y = ilicn.dt)) +
  geom_segment(aes(x = startFlu, y = 6.75, xend = startEpi, yend = 6.75, colour = truCol), size = sz) +
  scale_colour_manual(name = "Legend", values = c(truCol), breaks = c(truCol), labels = c("Burden")) +
  geom_bar(aes(fill = in.season), stat = 'identity') +
  scale_fill_manual(name = "", values = c('TRUE' = truColLt, 'FALSE' = falsCol), breaks = c('TRUE', 'FALSE'), labels = c('Epidemic period', 'Winter period\nNov. - Apr.'), na.value = naCol) +
  geom_line(colour = tsCol, size = sz) +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm"), axis.ticks = element_blank(), axis.text = element_blank(), plot.title = element_text(hjust = 0.5)) + # legend.position = "right", legend.spacing = unit(0, "in"), 
  scale_x_date("Time") +
  scale_y_continuous(ylab) +
  guides(fill = "none", colour = "none") +
  # guides(colour = guide_legend(order = 1),
         # fill = guide_legend(order = 2, title = NULL)) +
  ggtitle("Onset timing")
ggsave("dbGeneric_timeToStart.png", timeToStart.plt, width = w, height = h, dpi = dp)
print(timeToStart.plt)


timeToPeak.plt <- ggplot(exampD, aes(x = Thu.week, y = ilicn.dt)) +
  geom_bar(aes(fill = in.season), stat = 'identity') +
  scale_fill_manual(values = c('TRUE' = truColLt, 'FALSE' = falsCol), na.value = naCol) +
  geom_line(colour = tsCol, size = sz) +
  geom_segment(aes(x = startEpi, y = 6.75, xend = pkEpi, yend = 6.75), colour = truCol, size = sz) +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm"), axis.ticks = element_blank(), axis.text = element_blank(), plot.title = element_text(hjust = 0.5)) +
  scale_x_date("Time") +
  scale_y_continuous(ylab) +
  guides(fill = "none") +
  ggtitle("Peak timing")
ggsave("dbGeneric_timeToPeak.png", timeToPeak.plt, width = w, height = h, dpi = dp)
print(timeToPeak.plt)

timeToPeak.plt2 <- ggplot(exampD, aes(x = Thu.week, y = ilicn.dt)) +
  geom_bar(aes(fill = in.season), stat = 'identity') +
  scale_fill_manual(name = "", values = c('TRUE' = truColLt, 'FALSE' = falsCol), breaks = c('TRUE', 'FALSE'), labels = c('Epidemic period', 'Winter period\nNov. - Apr.'), na.value = naCol) +
  geom_line(colour = tsCol, size = sz) +
  geom_segment(aes(x = startEpi, y = 6.75, xend = pkEpi, yend = 6.75, colour = truCol), size = sz) +
  scale_colour_manual(name = "Legend", values = c(truCol), breaks = c(truCol), labels = c("Burden")) +
  theme_classic(base_size = fontsz, base_family = "") +
  theme(panel.grid.minor = element_blank(), plot.margin = unit(mar, "mm"), axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "right", legend.spacing = unit(0, "in"), plot.title = element_text(hjust = 0.5)) +
  scale_x_date("Time") +
  scale_y_continuous(ylab) +
  guides(colour = guide_legend(order = 1),
         fill = guide_legend(order = 2, title = NULL)) +
  ggtitle("Peak timing")

#### gridded magnitude metrics plots ##################
png(filename = "dbGeneric_allTiming.png", height = h, width = w*2+wleg, units = "in", res = dp)
tim.plts <- grid.arrange(grobs = list(epiDur.plt, timeToPeak.plt, timeToStart.plt), widths = unit(c(w, w, wleg), "in"), nrow = 1)
dev.off()

png(filename = "dbGeneric_allTiming2.png", height = h, width = w+wleg, units = "in", res = dp)
tim.plts <- grid.arrange(grobs = list(epiDur.plt, timeToPeak.plt2), widths = unit(c(w, wleg), "in"), nrow = 1)
dev.off()


