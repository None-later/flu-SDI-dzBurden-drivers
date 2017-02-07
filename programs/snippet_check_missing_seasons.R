## Name: Elizabeth Lee
## Date: 1/31/17
## Function: check which season in sequence 8a V2-6 s... have missing data
## Filenames: 
## Data Source: IMS Health
## Notes: need to SSH into snow server
## 
## useful commands:
## install.packages("pkg", dependencies=TRUE, lib="/usr/local/lib/R/site-library") # in sudo R
## update.packages(lib.loc = "/usr/local/lib/R/site-library")

#### header #################################
require(readr); require(tidyr); require(dplyr)

#### SOURCE: clean and import model data #################################
setwd(dirname(sys.frame(1)$ofile))

#### set these! ################################
dbCodeStr <- "_ilinDt_Octfit_span0.4_degree2"
modCodeStrLs <- c("8a_iliSum_v2-6_s6", "8a_iliSum_v2-6_s4", "8a_iliSum_v2-6_s2", paste0(rep(c("8a_iliSum_v2-6_s6-", "8a_iliSum_v2-6_s4-", "8a_iliSum_v2-6_s2-"), 3), c(rep(5,3), rep(6,3), rep(7,3), rep(8,3), rep(9,3)))) 

#### IMPORT FILEPATHS #################################
setwd("../R_export/inlaModelData_export")
path_upper <- getwd()

for (modCodeStr in modCodeStrLs){
  
  #### EXPORT FILEPATHS #################################
  # diagnostic plot export directories
  setwd(path_upper)
  setwd(sprintf("./%s", modCodeStr))
  fname <- grep("summaryStatsFitted", list.files(), value = TRUE)
  
  dummyDat <- read_csv(fname) %>% 
    filter(is.na(y1)) %>%
    group_by(season) %>%
    count
  
  print(paste(modCodeStr, "***********************"))
  print(dummyDat)
}