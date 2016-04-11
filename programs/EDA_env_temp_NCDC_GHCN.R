
## Name: Elizabeth Lee
## Date: 11/24/15
## Function: 
## Filenames: clean_ghcnm_temp.csv
## Data Source: NCDC GHCN monthly average temperature data
## Notes: Temperature values (VALUE#) are in hundredths of a degree Celsius, but are expressed as whole integers (e.g. divide by 100.0 to get whole degrees Celsius)
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
rm(list = ls())
require(readr); require(ggplot2); require(tidyr); require(dplyr)
require(grid); require(gridExtra); require(RColorBrewer)
setwd(dirname(sys.frame(1)$ofile)) # only works if you source the program

#### import data ################################
setwd("../../../../Sandra Goldlust's Work/El_SG_Shared_Work/mySQL_Tables/ghcnm_temp")
imported <- read_csv("clean_ghcnm_temp.csv", col_types = paste0("cic", paste0(rep("iccc", 12), collapse=""), "dd_c___"), na = "\\N")

#### data cleaning ################################
top <- 50 # north lat
left <- -125 # west long
right <- -67 # east long
bottom <-  25 # south lat

# gather temp value, DM flag, QC flag by month
dat1 <- gather(imported, bigkey, value, 4:51) %>%
  mutate(month = regmatches(bigkey, regexpr("[0-9]+", bigkey))) %>%
  mutate(key = regmatches(bigkey, regexpr("[[:alpha:]]+", bigkey)))
dat2 <- dat1 %>%
  select(-bigkey) %>%
  spread(key, value)
dat3 <- dat2 %>% 
  mutate(date = as.Date(sprintf("%s-%s-1", YEAR, month))) %>%
  mutate(pltVal = ifelse(is.na(DMFLAG) & is.na(QCFLAG), as.numeric(VALUE)/100, NA)) %>%
  filter(LONGITUDE > left & LONGITUDE < right & LATITUDE > bottom & LATITUDE < top)

uqid <- dat3 %>% select(ID) %>% distinct %>% mutate(for.ts = seq_along(1:nrow(.)))
dat4 <- left_join(dat3, uqid, by = "ID")

# #### plot formatting ################################
# w <- 6; h <- 4; dp <- 300
# w2 <- 9; h2 <- 9
# mar <- rep(2, 4)
# tierVec <- paste("Tier", 1:5)
# colVec <- brewer.pal(length(tierVec), 'RdYlGn')
# 
# varnames <- dat3 %>% select(ELEMENT) %>% distinct %>% arrange(ELEMENT) %>% unlist
# varlabels <- c("Avg Temp (C)", "Max Temp (C)", "Min Temp (C)")
# times <- dat3 %>% select(date) %>% distinct %>% arrange(date)
# times.ix <- seq(1, length(times$date), by=12)
# 
# num <- 40
# indexes <- seq(1, max(dat4 %>% select(for.ts)), by=num)
# #### map temperature ################################
# setwd(dirname(sys.frame(1)$ofile))
# dir.create('../graph_outputs/EDA_env_temp_NCDC_GHCN', showWarnings = FALSE)
# dir.create('../graph_outputs/EDA_env_temp_NCDC_GHCN/maps', showWarnings = FALSE)
# setwd('../graph_outputs/EDA_env_temp_NCDC_GHCN/maps')
# 
# for(v in varnames){
#   pltDat <- dat3 %>% 
#     filter(ELEMENT == v & date %in% times$date[times.ix]) %>%
#     mutate(bins = cut(pltVal, breaks = quantile(pltVal, probs = seq(0, 1, by = 1/5), na.rm=T), ordered_result = TRUE)) %>%
#     mutate(bins = factor(bins, levels = rev(levels(bins)), labels = tierVec)) %>% 
#     mutate(bin_color = factor(bins, levels = levels(bins), labels = colVec))
#   
#   llmap <- ggplot(pltDat, aes(x = LONGITUDE, y = LATITUDE)) +
#     geom_point(aes(fill = pltVal), pch = 21, alpha = 0.6) +
#     scale_fill_gradient(name = varlabels[which(varnames==v)], low = "blue", high = "red", guide = "colourbar", na.value = "grey50") +
#     theme_minimal(base_size = 16, base_family = "") +
#     theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), plot.margin = unit(mar, "mm"), panel.margin = unit(mar, "mm"), legend.position = "bottom") +
#     coord_cartesian(xlim = c(left, right), ylim = c(bottom, top)) +
#     facet_wrap(~YEAR, nrow = 3)
#   ggsave(sprintf("%s_gradient_NCDC_wStation_Jan.png", v), llmap, width = w, height = h, dpi = dp)
#   
#   llmapD <- ggplot(pltDat, aes(x = LONGITUDE, y = LATITUDE)) +
#     geom_point(aes(fill = bin_color), pch = 21, alpha = 0.6) +
#     scale_fill_discrete(name = varlabels[which(varnames==v)], labels = tierVec, guide = "legend", na.value = "grey50") +
#     theme_minimal(base_size = 16, base_family = "") +
#     theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), plot.margin = unit(mar, "mm"), panel.margin = unit(mar, "mm"), legend.position = "bottom") +
#     coord_cartesian(xlim = c(left, right), ylim = c(bottom, top)) +
#     facet_wrap(~YEAR, nrow = 3)
#   ggsave(sprintf("%s_tier_NCDC_wStation_Jan.png", v), llmapD, width = w, height = h, dpi = dp)
# } # saved 11/24/15
# 
# 
# #### time series data ################################
# dir.create('../ts', showWarnings = FALSE)
# setwd('../ts')
# 
# for(i in indexes){
#   pltDat2 <- dat4 %>% 
#     filter(for.ts>= i & for.ts < i+num) 
#   
#   tsP <- ggplot(pltDat2, aes(x = date, y = pltVal)) +
#     geom_point(aes(colour = ELEMENT)) +
#     geom_line(aes(colour = ELEMENT)) +
#     scale_y_continuous(name = "Temperature (C)") +
#     theme_classic(base_size = 14, base_family = "") +
#     theme(legend.position = "bottom") +
#     facet_wrap(~ID, ncol = 8)
#   plabs <- pltDat2 %>% filter(for.ts == min(for.ts) | for.ts == max(for.ts)) %>% select(for.ts) %>% distinct %>% unlist
#   ggsave(sprintf("temp_NCDC_GHCN_%s-%s.png", plabs[1], plabs[2]), tsP, width = w2, height = h2, dpi = dp)
#   
# } # saved 11/29/15
  