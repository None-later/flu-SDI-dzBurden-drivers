
## Name: Elizabeth Lee
## Date: 10/19/15
## Function: 1. explore periodic regression fits of raw ILI through plotting, colored by in season classifications; must run write_periodicReg_... and write_relativeDiseaseBurden_... programs before running this one (to determine in.season classifications)
## Filenames: 
## Data Source: 
## Notes: separated from explore_periodicReg_fits_ILI.R
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")
rm(list = ls())
#### header ####################################
require(dplyr)
require(ggplot2)
require(readr)
setwd(dirname(sys.frame(1)$ofile))
#### set these! ####################################
# code <- "t2sa_" # semi-annual periodicity
# code <- "t2_" # parabolic time trend term
code <- "t4_" # quartic time trend
# code <-"" # linear time trend term

# code2 <- "_Oct" # fluseason = Oct to Apr
code2 <- "_Octfit" # fit = Apr to Oct and fluseason = Oct to Apr
#### plot formatting ################################
w <- 9; h <- 6

#### import data ################################
setwd('../R_export')
data <- read_csv(file=sprintf('periodicReg_%sallZip3Mods_ILI%s.csv', code, code2), col_types=list(ili=col_double(), .fitted=col_double(), .se.fit=col_double()))
data.inSeas <- read_csv(file=sprintf('fullIndicAll_periodicReg_%sILI%s_analyzeDB.csv', code, code2), col_types=list(zipname=col_character()))
data.combos <- read_csv(file=sprintf('zip3SeasonCombos_%sILI%s.csv', code, code2), col_types=list(zipname=col_character()))

#### initial time series plots ################################
dir.create(sprintf('../graph_outputs/explore_periodicReg_%sfits_ILI%s', code, code2), showWarnings=FALSE)
setwd(sprintf('../graph_outputs/explore_periodicReg_%sfits_ILI%s', code, code2))

#### 9/15/15 in.season fits ################################
dir.create(sprintf('../explore_periodicReg_%sfits_ILI%s/inSeason', code, code2), showWarnings=FALSE)
setwd(sprintf('../explore_periodicReg_%sfits_ILI%s/inSeason', code, code2))

zip3list2 <- data.inSeas %>% select(zipname) %>% distinct %>% arrange(zipname) %>% mutate(for.plot = seq_along(1:nrow(.)))
data_plot2 <- right_join(data.inSeas, zip3list2, by="zipname")
indexes2 <- seq(1, max(data_plot2 %>% select(for.plot)), by=6)

for(i in indexes2){
  dummyplots <- ggplot(data_plot2 %>% filter(for.plot>= i & for.plot < i+6), aes(x=week, y=ili, group=zipname)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = in.season)) + scale_color_discrete(name='in.season') + 
    geom_line(aes(y=ifelse(flu.week, 1, NA)), color = 'black') + 
    geom_line(aes(y = .fitted), color = 'black') + 
    facet_wrap(~zipname, scales="free_y")
  # grab zip3s in plot for file name
  ziplabels <- data_plot2 %>% select(zipname) %>% distinct %>% slice(c(i, i+5)) 
  ggsave(sprintf("periodicReg_inSeas_%sfits_ILI%s_%s-%s.png", code, code2, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
} # saved 10/20/15

#### 10/20/15 in.season filtered fits (write_zip3seasonFiltered_ILI.R) ################################
dir.create(sprintf('../inSeasonFiltered', code, code2), showWarnings=FALSE)
setwd(sprintf('../inSeasonFiltered', code, code2))

zips.filtered <- data.combos %>% filter(season != 1) %>% count(zipname) %>% filter(n == 8) %>% select(zipname)
zip3list3 <- data.inSeas %>% select(zipname) %>% distinct %>% filter(zipname %in% zips.filtered$zipname) %>% arrange(zipname) %>% mutate(for.plot = seq_along(1:nrow(.)))
data_plot3 <- right_join(data.inSeas, zip3list3, by="zipname")
indexes3 <- seq(1, max(data_plot3 %>% select(for.plot)), by=6)

for(i in indexes3){
  dummyplots <- ggplot(data_plot3 %>% filter(for.plot>= i & for.plot < i+6), aes(x=week, y=ili, group=zipname)) +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
    geom_line(aes(color = in.season)) + scale_color_discrete(name='in.season') + 
    geom_line(aes(y=ifelse(flu.week, 1, NA)), color = 'black') + 
    geom_line(aes(y = .fitted), color = 'black') + 
    facet_wrap(~zipname, scales="free_y")
  # grab zip3s in plot for file name
  ziplabels <- data_plot3 %>% select(zipname) %>% distinct %>% slice(c(i, i+5)) 
  ggsave(sprintf("periodicReg_inSeasFilt_%sfits_ILI%s_%s-%s.png", code, code2, ziplabels[1,], ziplabels[2,]), dummyplots, width=w, height=h)
} # saved 10/20/15
