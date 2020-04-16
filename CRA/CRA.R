library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

files <- list.files("CRA/data")

for(f in files[1:4]) {
  temp <- read_fwf("CRA/data/" %p% f,
                   col_types = "cnccccnnnnnnnnn",
                   fwf_cols(table = c(1, 5),
                            year = c(6, 9), 
                            FIPS = c(12, 16),
                            MSA = c(17, 20), 
                            tract = c(21, 27), 
                            income_group = c(30, 32), 
                            geog_level = c(33, 35),
                            num_under_100k     = c(36, 41),
                            amount_under_100k  = c(42, 49),
                            num_100k_250k      = c(50, 55),
                            amount_100k_250k   = c(56, 63),
                            num_250k_1m      = c(64, 69),
                            amount_250k_1m   = c(70, 77),
                            num_business_1m    = c(78, 83),
                            amount_business_1m = c(84, 91)))
  
  cra <- assign_row_join(cra, temp)
}

for(f in files[5:length(files)]) {
  temp <- read_fwf("CRA/data/" %p% f,
                   col_types = "cnccccnnnnnnnnn",
                   fwf_cols(table = c(1, 5),
                            year = c(6, 9), 
                            FIPS = c(12, 16),
                            MSA = c(17, 21), 
                            tract = c(22, 28), 
                            income_group = c(31, 33), 
                            geog_level = c(34, 36),
                            num_under_100k = c(37, 46),
                            amount_under_100k = c(47, 56),
                            num_100k_250k = c(57, 66),
                            amount_100k_250k = c(67, 76),
                            num_250k_1m = c(77, 86),
                            amount_250k_1m = c(87, 96),
                            num_business_1m = c(97, 106),
                            amount_business_1m = c(107, 116)))
  
  cra <- assign_row_join(cra, temp)
}

cra %<>% 
  filter(table == "A1-1") %>%
  select(FIPS, tract, year, income_group, geog_level, contains("num"), contains("amount")) %>%
  mutate_at(vars(contains("amount")), ~ . * 1000) %>%
  mutate(all_sb_loans = amount_under_100k + amount_100k_250k + amount_250k_1m)

cra_county <- cra %>%
  filter(geog_level == 200) %>%
  pull_peers(geog = "FIPS", add_info = F) %>%
  stl_merge(num_under_100k:all_sb_loans, method = "sum") %>%
  COLA(amount_under_100k:all_sb_loans, rpp = F) %>%
  per_capita_adj(num_under_100k:all_sb_loans)
  
cra_msa_1yr <- cra  %>%
  filter(geog_level == 200) %>%
  pull_peers(geog = "MSA", add_info = F) %>%
  sum_FIPS_to_MSA(num_under_100k:all_sb_loans) %>%
  COLA(amount_under_100k:all_sb_loans, rpp = F) %>%
  per_capita_adj(num_under_100k:all_sb_loans)

cra_tract <- cra  %>%
  filter(is.na(geog_level), FIPS == "21111") %>%
  mutate(tract = "21111" %p% str_remove(tract, "\\.")) %>%
  select(-FIPS, -income_group, -geog_level) %>%
  COLA(amount_under_100k:all_sb_loans, rpp = F) %>%
  filter(year >= 2012) %>%
  per_capita_adj(num_under_100k:all_sb_loans)

write_csv(cra_county, "output_data/cra_tract.csv")
write_csv(cra_msa_1yr, "output_data/cra_tract.csv")
write_csv(cra_tract, "output_data/cra_tract.csv")
write_csv(cra_tract, "output_data/cra_tract.csv")
write_csv(cra_tract, "output_data/cra_tract.csv")
