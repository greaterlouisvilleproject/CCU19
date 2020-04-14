library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

# Read in institution and transaction CDFI data

# Institution Data
inst_cols <- c("ncnn",       # org ID, inst type, year, zip
               rep("_", 21), # ownership
               rep("_", 5),  # staff
               rep("_", 19), # capital
               rep("_", 11), # financials
               rep("_", 18), # revenue
               rep("_", 21), # other financials
               rep("n", 28)) # origination number and amounts

# Transaction Data
trans_cols <- c("nnnccccc",   # org, transaction, amount, FIPS, investee, date, purpose)
                rep("_", 42)) # other

inst_cols  %<>% paste(collapse = "") %>% str_pad(203, "right", "_")
trans_cols %<>% paste(collapse = "")

institution <- read_csv("CDFI/releaseILR_fy03_17(1of1).csv",
                        col_types = inst_cols)

for(f in list.files("CDFI/TLR")) {
  temp <- read_csv("CDFI/TLR/" %p% f, col_types = trans_cols)
  transaction <- assign_row_join(transaction, temp)
}

purpose_recode <- c(BUSFIXED = "business", BUSINESS = "business", BUSWORKCAP = "business", MICRO = "business",
                    CONSUMER = "consumer", HOMEIMP = "consumer", HOMEPURCH = "consumer", 
                    RECOCOM = "commercial_re", RERHCOM = "commercial_re",
                    RECOMULTI = "residential_re", RECOSINGLE = "residential_re",  
                    RERHMULTI = "residential_re", RERHSINGLE = "residential_re",
                    OTHER = "other")

transaction %<>%
  
  # recode two-digit year contained in date to four-digit numeric year
  separate(dateclosed, into = c("day", "month", "year"), sep = "-", convert = T, fill = "right") %>%
  mutate(year = if_else(year >= 84, year + 1900, year + 2000)) %>%
  filter(year >= 2000) %>%
  
  # Recode 2000 and 2010 census tracts to county FIPS codes and 2000/2010 census tracts
  mutate(
    projectfipscode_2000 = 
      replace(projectfipscode_2000, projectfipscode_2000 == "NONE", NA) %>%
      str_pad(11, "left", "0"),
    projectfipscode_2010 = 
      replace(projectfipscode_2010, projectfipscode_2010 == "NONE", NA) %>%
      str_pad(11, "left", "0"),
    FIPS_00 = str_sub(projectfipscode_2000, 1, 5),
    FIPS_10 = str_sub(projectfipscode_2010, 1, 5),
    FIPS = if_else(is.na(FIPS_10), FIPS_00, FIPS_10),
    tract_10 = projectfipscode_2010,
    tract_00 = projectfipscode_2000) %>%
  
  # Add labels from the STL FED
  mutate(purpose = recode(purpose, !!!purpose_recode)) %>%
  
  pull_peers(geog = "MSA", add_info = F)

cdfi_county <- transaction %>%
  pull_peers(add_info = F) %>%
  group_by(FIPS, year) %>%
  summarise(cdfi = sum(originalamount)) %>%
  ungroup() %>%
  complete(FIPS, year, fill = list(cdfi = 0)) %>%
  stl_merge(cdfi, method = "sum") %>%
  COLA(cdfi, rpp = F) %>%
  per_capita_adj(cdfi)
  
cdfi_msa_1yr <- transaction %>%
  left_join(MSA_FIPS, by = "FIPS") %>%
  group_by(MSA, year) %>%
  summarise(cdfi = sum(originalamount)) %>%
  ungroup() %>%
  complete(MSA, year, fill = list(cdfi = 0)) %>%
  COLA(cdfi, rpp = F) %>%
  per_capita_adj(cdfi)
  
cdfi_map <- transaction %>%
  filter(
    year >= 2014,
    str_sub(tract_10, 1, 5) == "21111") %>%
  mutate(tract = tract_10) %>%
  group_by(tract, year) %>%
  summarise(cdfi = sum(originalamount)) %>%
  ungroup() %>%
  COLA(cdfi, rpp = F)

process_map(cdfi_map, cdfi, pop_adjust = T, method = "sum", return_name = "cdfi") %>%
  list2env(.GlobalEnv)

write_csv(cdfi_county,  "output_data/CDFI_county.csv")
write_csv(cdfi_msa_1yr, "output_data/CDFI_msa.csv")
write_csv(cdfi_tract,   "output_data/CDFI_tract.csv")
write_csv(cdfi_nh,      "output_data/CDFI_nh.csv")
write_csv(cdfi_muw,     "output_data/CDFI_muw.csv")


  
  