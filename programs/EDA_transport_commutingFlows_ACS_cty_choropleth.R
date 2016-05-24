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
nq <- 5
incVec <- paste("Tier", 1:nq)
colVec <- brewer.pal(length(incVec), 'Reds')
h <- 5; w <- 8; dp <- 450
mar <- rep(0, 4)
par(mar = mar, oma = mar)

#### import transport data ################################
setwd("../../../../Sandra Goldlust's Work/Shared_Data/SG_covariate_data/Cleaned_Data")
# income data
# trouble converting characters: http://stackoverflow.com/questions/4993837/r-invalid-multibyte-string
transDat <- read_csv("clean_transport_ACS0610_iconv.csv", col_types = 'cccciicccc') %>%
  rename(res_st = state_id_residence_2digit) %>%
  rename(res_cty = county_id_residence_3digit) %>%
  rename(wk_st = state_id_workplace_3digit) %>%
  rename(wk_cty = county_id_workplace_3digit) %>%
  mutate(res_fips = paste0(res_st, res_cty)) %>%
  mutate(wk_fips = paste0(substr.Right(wk_st, 2), wk_cty)) %>%
  filter(wk_cty != '000')
transDat2 <- transDat %>%
  mutate(match = tolower(paste(state_workplace, substring(county_workplace, 1, nchar(county_workplace)-7), sep = ',')))

#### import population data ################################
setwd(dirname(sys.frame(1)$ofile))
setwd('../../../Census/Source_Data')
popDat <- read_csv("CO-EST00INT-TOT.csv") %>%
  mutate(cty = substr.Right(paste0("00", COUNTY), 3)) %>% 
  mutate(st = substr.Right(paste0("0", STATE), 2)) %>%
  mutate(fips = paste0(st, cty))

# merge trans and popdat
fullDat <- left_join(transDat2, popDat, by = c("wk_fips" = "fips"))

#### generate plots ################################

setwd(dirname(sys.frame(1)$ofile))
dir.create("../graph_outputs/EDA_transport_commutingFlows_ACS_cty", showWarnings = FALSE)
setwd("../graph_outputs/EDA_transport_commutingFlows_ACS_cty")


processData <- function(dataOb){
  dataOb2 <- dataOb %>%
    group_by(wk_fips) %>%
    summarise(influx = sum(Number), pop = max(CENSUS2010POP, na.rm=T)) %>%
    mutate(inf.norm = influx/pop*1000) %>%
    mutate(in_bin = cut(influx, breaks = quantile(influx, probs = seq(0, 1, by = 1/nq), na.rm=T), ordered_result = TRUE, include.lowest = TRUE)) %>%
    mutate(in_bin = factor(in_bin, levels = rev(levels(in_bin)), labels = incVec)) %>% 
    mutate(influx_color = factor(in_bin, levels = levels(in_bin), labels = colVec)) %>%
    mutate(influx_colstring = as.character(influx_color)) %>%
    mutate(norm_bin = cut(inf.norm, breaks = quantile(inf.norm, probs = seq(0, 1, by = 1/nq), na.rm=T), ordered_result = TRUE, include.lowest = TRUE)) %>%
    mutate(norm_bin = factor(norm_bin, levels = rev(levels(norm_bin)), labels = incVec)) %>% 
    mutate(norm_color = factor(norm_bin, levels = levels(norm_bin), labels = colVec)) %>%
    mutate(norm_colstring = as.character(norm_color))
  return(dataOb2)
}

#### include all domestic commutes ####
pltDat <- processData(fullDat)
pltOrder <- pltDat$wk_fips[match(map("county", plot=FALSE)$names, pltDat$match)]
norm_colstring <- pltDat$norm_colstring[match(pltOrder, pltDat$wk_fips)]

png(filename = sprintf("commutingInflows_cty_inclWithinCty.png"), height = h, width = w, units = "in", res = dp)
map("county", fill = TRUE, lwd = 0.1, col = norm_colstring, xlim = c(-124, -65), ylim = c(20, 50))
legend(list(x = -122.5, y = 24), legend = incVec, horiz = TRUE,  cex = 0.75, fill = levels(pltDat$influx_color))
dev.off()

png(filename = sprintf("commutingInflowsNorm_cty_inclWithinCty.png"), height = h, width = w, units = "in", res = dp)
map("county", fill = TRUE, lwd = 0.1, col = norm_colstring, xlim = c(-124, -65), ylim = c(20, 50))
legend(list(x = -122.5, y = 24), legend = incVec, horiz = TRUE,  fill = levels(pltDat$norm_color), cex = 0.75)
dev.off()

print(pltDat %>% count(in_bin))

#### filter out within-county commutes ####
pltDat2 <- processData(fullDat %>% filter(res_cty != wk_cty))
pltOrder2 <- pltDat2$wk_fips[match(map("county", plot=FALSE)$names, pltDat2$match)]
influx_colstring <- pltDat2$influx_colstring[match(pltOrder2, pltDat2$wk_fips)]


png(filename = sprintf("commutingInflows_cty_exclWithinCty.png"), height = h, width = w, units = "in", res = dp)
map("county", fill = TRUE, lwd = 0.1, col = pltDat2$influx_colstring, xlim = c(-124, -65), ylim = c(20, 50))
legend(list(x = -122.5, y = 24), legend = incVec, horiz = TRUE,  fill = levels(pltDat2$influx_color), cex = 0.75)
dev.off()

png(filename = sprintf("commutingInflowsNorm_cty_exclWithinCty.png"), height = h, width = w, units = "in", res = dp)
map("county", fill = TRUE, lwd = 0.1, col = pltDat2$norm_colstring, xlim = c(-124, -65), ylim = c(20, 50))
legend(list(x = -122.5, y = 24), legend = incVec, horiz = TRUE,  fill = levels(pltDat2$norm_color), cex = 0.75)
dev.off()

print(pltDat2 %>% count(in_bin))

#### include ONLY within-county commutes ####
pltDat3 <- processData(fullDat %>%  filter(res_cty == wk_cty))

png(filename = sprintf("commutingInflows_cty_withinCtyOnly.png"), height = h, width = w, units = "in", res = dp)
map("county", fill = TRUE, lwd = 0.1, col = pltDat3$influx_colstring, xlim = c(-124, -65), ylim = c(20, 50))
legend(list(x = -122.5, y = 24), legend = incVec, horiz = TRUE,  fill = levels(pltDat3$influx_color), cex = 0.75)
dev.off()

png(filename = sprintf("commutingInflowsNorm_cty_withinCtyOnly.png"), height = h, width = w, units = "in", res = dp)
map("county", fill = TRUE, lwd = 0.1, col = pltDat3$norm_colstring, xlim = c(-124, -65), ylim = c(20, 50))
legend(list(x = -122.5, y = 24), legend = incVec, horiz = TRUE,  fill = levels(pltDat3$norm_color), cex = 0.75)
dev.off()

print(pltDat3 %>% count(in_bin))

