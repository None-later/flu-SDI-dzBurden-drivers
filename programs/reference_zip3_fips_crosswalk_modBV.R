# modified from Barrett's cleaning code ("ZCTA_and_FIPS_script")

# The first 89 lines of this file create a crosswalk between zip3 and FIPS codes 
setwd(dirname(sys.frame(1)$ofile))
setwd("../reference_data/zip3_cty_crosswalk_toclean")

# The following dataframe was taken from www.udsmapper.com and provides which ZCTA each
# zip5 population lives in
zip_to_zcta <- read.csv("Zip_Centroid_to_ZCTA_JSI2012.csv", header = T, stringsAsFactors = F, colClasses = "character")
zip_to_zcta[1:5,]

# Removing extraneous rows
popSumCheck <- zip_to_zcta[,-(2:4)]
popSumCheck[1:5,]

# Reading in the zcta dataframe
zcta <- read.csv("ZCTA5_MCDC.csv", stringsAsFactors = FALSE, colClasses = c("character"))
new.zcta <- zcta[-1,] # to get rid of explanatory text in first row
new.zcta$pctcnty <- as.numeric(new.zcta$pctcnty)
new.zcta$Pop10ZCTAstate <- as.numeric(new.zcta$Pop10ZCTAstate)
new.zcta$ZipTotPop <- as.numeric(new.zcta$ZipTotPop)

#checks
new.zcta[1:5,]
sum(new.zcta$ZipTotPop,na.rm = T)

# m1 will allow us to see which ZCTAs contribute to each zip3 
m1 <- merge(popSumCheck,new.zcta,by.x = "ZCTA_USE",by.y = "zcta5", all = F)
colnames(m1)[3] <- "ZIP3"
m1[,3] <- substr(m1[,2],1,3)

#checks
m1[1:5,]
sum(m1$ZipTotPop,na.rm=T) #note this has duplicated ZCTAs, leading to too large of a population

#get rid of extraneous columns
m2 <- m1[,-(4:9)]
#check
m2[1:5,]

#get rid of duplicated ZCTAs
unik <- !duplicated(m2$ZCTA_USE)  
#check
sum(m2$ZipTotPop[unik], na.rm = T)

# m2_unik is a dataframe of only the unique ZCTAs in each zip3
m2_unik <- subset(m2,unik)
#checks
m2_unik[1:5,]
sum(m2_unik$ZipTotPop, na.rm = T)

#find total population in each zip3 by summing the ZCTA populations in each zip3
zip3pops <- aggregate(ZipTotPop~ZIP3, FUN = sum, data = m2_unik)
colnames(zip3pops)[2] <- "Zip3Pop"
#checks
zip3pops[1:5,]
sum(zip3pops$Zip3Pop)

#combo dataframe will hold the percent of each zcta that lives in each county
combo <- new.zcta[,c(2,5,6,7)]
combo[1:5,]
colnames(combo)[1] <- "ZCTA" 

# lastdf will give ZCTA population and the population of the zip3 that ZCTA falls within
lastdf <- merge(m2_unik, zip3pops, by = "ZIP3")
#check
lastdf[1:5,]

colnames(lastdf)[4] <- "ZCTApop"
colnames(lastdf)[2] <- "ZCTA"
# zcta_pct_zip3 gives the percent of the zip3 that each ZCTA represents
lastdf$zcta_pct_zip3 <- (lastdf$ZCTApop / lastdf$Zip3Pop)
#check
lastdf[1:5,]

#final dataframe combines all information about zip3, zcta, and fips code
final <- merge(lastdf, combo, by = "ZCTA")
#check
final[1:5,]

# Check zcta composition of zip3 correct
aggregate(zcta_pct_zip3~ZIP3, FUN = sum, data = final) # all proportions sum to 1 over a zip3

#get rid of unimportant information
applied <- final[,-c(3:5)]
applied[1:5,]

write.csv(applied, file = "Zip3ToFIPS.csv", row.names = FALSE)

####################################
# FIPS code population summing
FIPSdf <- new.zcta[,-c(1,3,4,8)]
FIPSdf$PopFIPS1 <- FIPSdf$pctcnty/100 * FIPSdf$ZipTotPop
FIPSdf$PopFIPS2 <- FIPSdf$ZipTotPop - FIPSdf$PopFIPS1
#check
FIPSdf[1:5,]
sum(FIPSdf$ZipTotPop, na.rm =T)

#create dataframes for each of the fips code columns (for combining later)
FIPS1 <- FIPSdf[,c(1,3,6)]
FIPS2 <- FIPSdf[,c(1,4,7)]
#check
FIPS1[1:5,]
FIPS2[1:5,]

#get rid of empty spaces in the secondary fips code column
FIPS2 <- FIPS2[-which(FIPS2$FipCo2==""),]

#rename columns so we can use rbind() to combine the dataframes
colnames(FIPS1) <- c("zcta5","FIPScode","Population")
colnames(FIPS2) <- c("zcta5","FIPScode","Population")

zctaToFIPS <- rbind(FIPS1,FIPS2)
#checks
zctaToFIPS[1:5,]
sum(zctaToFIPS$Population, na.rm = T)

#aggregate the populations by FIPS code
TotalFIPSPopulations <- aggregate(Population~FIPScode, FUN = sum, data = zctaToFIPS) 
#checks
TotalFIPSPopulations[1:5,] # Gives population by FIPS code
sum(TotalFIPSPopulations$Population,na.rm=T) # Same as before!

write.csv(TotalFIPSPopulations, file = "FIPSPopulations.csv", row.names = FALSE)

## ECL 12/24/13 These populations should be checked across populations by fips code in zip3_RUCC2013.csv
##########################################################
# 5/27/16
require(dplyr); require(tidyr)

zctaToFIPS2 <- tbl_df(zctaToFIPS) %>% 
  mutate(zip3 = substring(zcta5, 1, 3)) %>%
  mutate(uqid = paste0(zip3, "_", FIPScode)) %>%
  group_by(uqid) %>%
  summarise(zinf_pop = sum(Population, na.rm=TRUE)) # zip3 contribution to fips population

zctaToFIPS3 <- zctaToFIPS2 %>%
  mutate(zip3 = substring(uqid, 1, 3), fips = substr.Right(uqid, 5)) %>%
  left_join(TotalFIPSPopulations, by = c("fips" = "FIPScode")) %>%
  rename(fipstot_pop = Population) %>%
  mutate(zinf_prop = zinf_pop/fipstot_pop) %>%
  select(uqid, fips, zip3, zinf_pop, fipstot_pop, zinf_prop) %>%
  arrange(fips, zip3)

## write to file ##
require(readr)
setwd(dirname(sys.frame(1)$ofile))
setwd("../reference_data")
write_csv(zctaToFIPS3, "zip3_cty_crosswalk.csv")

## perform checks ##########################################################
# # check: do the population sizes for both fips & zip3 make sense? YES
# checkDat_fips <- zctaToFIPS3 %>%
#   distinct(fips, fipstot_pop) %>%
#   select(fipstot_pop) %>%
#   sum
# checkDat_zip3 <- zctaToFIPS3 %>%
#   distinct(zip3, zinf_pop) %>%
#   select(zinf_pop) %>%
#   sum

# # check: Does zinf_prop sum to 1 for each fips? YES
# chk_zinf <- zctaToFIPS3 %>%
#   group_by(fips) %>%
#   summarise(zinf = sum(zinf_prop, na.rm=TRUE)) 

# # check: Is fipstot_pop close to official 2010 census pop? YES - largest differences are on the order of ~50% of pop size
# setwd(dirname(sys.frame(1)$ofile))
# setwd("../reference_data")
# ctypopDat <- read_csv("cty_pop_latlon.csv", col_types = "_c_i__", col_names = c("fips", "pop10"), skip = 1)
# checkDat <- left_join(zctaToFIPS3 %>% select(fips, fipstot_pop), ctypopDat, by = "fips") %>%
#   mutate(diff = fipstot_pop-pop10, percdiff = (fipstot_pop-pop10)/pop10*100)
# # plot histograms of difference and percent difference
# hist(checkDat$diff, breaks = 50)
# hist(checkDat$percdiff, breaks = 100)
