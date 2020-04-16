library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

sba_7a  <- readxl::read_excel("SBA/FOIA - 7(a)(FY2010-Present).xlsx")
sba_504 <- readxl::read_excel("SBA/FOIA - 504 (FY1991-Present).xlsx")

FIPS_join <- FIPS_info %>%
  mutate(
    state = state_abbr,
    county = str_to_upper(county) %>%
      str_replace("ST.", "SAINT"))

sba_7a %<>%
  transmute(
    zip = BorrZip,
    year = str_sub(ApprovalDate, 1, 4) %>% as.numeric(),
    sba_loan_7a = SBAGuaranteedApproval,
    total_loan_7a = GrossApproval,
    county = ProjectCounty,
    state = ProjectState) %>%
  left_join(FIPS_join, by = c("state", "county")) %>%
  pull_peers(add_info = F) %>%
  select(FIPS, zip, year, sba_loan_7a, total_loan_7a)

sba_county_7a <- sba_7a %>%
  group_by(FIPS, year) %>%
  summarise(
    sba_loan_7a = sum(sba_loan_7a),
    total_loan_7a = sum(total_loan_7a)) %>%
  ungroup() %>%
  COLA(sba_loan_7a:total_loan_7a, base_year = 2019, rpp = F) %>%
  per_capita_adj(sba_loan_7a:total_loan_7a, geog = "FIPS")

sba_zip_7a <- sba_7a %>%
  filter(
    FIPS == "21111", 
    zip %in% FIPS_zip$zip[FIPS_zip$FIPS == "21111"]) %>%
  group_by(FIPS, zip, year) %>%
  summarise(
    sba_loan_7a = sum(sba_loan_7a),
    total_loan_7a = sum(total_loan_7a)) %>%
  ungroup() %>%
  COLA(sba_loan_7a:total_loan_7a, base_year = 2019, rpp = F) %>%
  select(-FIPS) %>%
  left_join(FIPS_zip, by = "zip") %>%
  mutate(
    sba_loan_7a_pp = sba_loan_7a / population_total,
    total_loan_7a_pp = total_loan_7a / population_total) %>%
  select(-population_total, -population_in_FIPS, -FIPS)


sba_504 %<>%
  transmute(
    zip = BorrZip,
    year = str_sub(ApprovalDate, 1, 4) %>% as.numeric(),
    total_loan_504 = GrossApproval,
    county = ProjectCounty,
    state = ProjectState) %>%
  left_join(FIPS_join, by = c("state", "county")) %>%
  pull_peers(add_info = F) %>%
  filter(year >= 2000) %>%
  select(FIPS, zip, year, total_loan_504)

sba_county_504 <- sba_504 %>%
  group_by(FIPS, year) %>%
  summarise(
    total_loan_504 = sum(total_loan_504)) %>%
  ungroup() %>%
  COLA(total_loan_504, base_year = 2019, rpp = F) %>%
  per_capita_adj(total_loan_504, geog = "FIPS")

sba_zip_504 <- sba_504 %>%
  filter(
    FIPS == "21111", 
    zip %in% FIPS_zip$zip[FIPS_zip$FIPS == "21111"]) %>%
  group_by(FIPS, zip, year) %>%
  summarise(
    total_loan_504 = sum(total_loan_504)) %>%
  ungroup() %>%
  COLA(total_loan_504, base_year = 2019, rpp = F) %>%
  select(-FIPS) %>%
  left_join(FIPS_zip, by = "zip") %>%
  mutate(
    total_loan_504_pp = total_loan_504 / population_total) %>%
  select(-population_total, -population_in_FIPS, -FIPS)

sba_county <- left_join(sba_county_7a, sba_county_504, by = c("FIPS", "year"))
sba_zip    <- left_join(sba_zip_7a, sba_zip_504, by = c("zip", "year"))

# replace per capita data for zip codes with 0 population
sba_zip %<>% mutate_at(vars(contains("_pp")), ~ if_else(. == Inf, NA_real_, .))

sba_county %<>% select(FIPS, year, sba_loan_7a, total_loan_7a, total_loan_504, sba_loan_7a_pp, total_loan_7a_pp, total_loan_504_pp)
sba_zip %<>% select(zip, year, sba_loan_7a, total_loan_7a, total_loan_504, sba_loan_7a_pp, total_loan_7a_pp, total_loan_504_pp)

write_csv(sba_county, "output_data/sba_county.csv")
write_csv(sba_zip, "output_data/sba_zip.csv")

