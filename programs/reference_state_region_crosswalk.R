# Elizabeth Lee
# 4/18/16
# state-HHS region crosswalk

setwd("/home/elee/Dropbox/Elizabeth_Bansal_Lab/SDI_Data/dz_burden/reference_data")
regCW <- read_csv("stateabbr_regnum_crosswalk.csv", col_types = "ci")
abbrDat <- read_csv("state_abbreviations_FIPS.csv", col_types = "ccc")

# merge states and region numbers
stDat <- full_join(regCW, abbrDat, by = c("state" = "Abbreviation")) %>%
  rename(Abbreviation = state, Region = HHSregion, fips = FIPS) %>%
  select(fips, State, Abbreviation, Region)

write_csv(stDat, "state_abbreviations_FIPS_region.csv")
# 4/18/16