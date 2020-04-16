library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

col_types <- c(rep("skip",    2),
               rep("text",    5),  # Borrower info
               rep("skip",    5), 
               rep("numeric", 2),  # SBA loan amount and total loan amount
               "date",             # Approval date
               rep("skip",    10),
               rep("text",    2),  # County and state
               rep("skip",    9))

sba_7a_00  <- readxl::read_excel("SBA/FOIA - 7(a)(FY2000-FY2009).xlsx",  col_types = col_types)
sba_7a_10  <- readxl::read_excel("SBA/FOIA - 7(a)(FY2010-Present).xlsx", col_types = col_types)
sba_504    <- readxl::read_excel("SBA/FOIA - 504 (FY1991-Present).xlsx")

sba_7a <- bind_rows(sba_7a_00, sba_7a_10)

# Adapt the FIPS data frame to correctly label the SBA files
FIPS_join <- FIPS_info %>%
  mutate(
    state = state_abbr,
    county = str_to_upper(county) %>%
      str_replace("ST.", "SAINT"))

# Rename and keep relevant columns for SBA 7a data; subset to peers
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
  select(FIPS, zip, year, sba_loan_7a, total_loan_7a) %>%
  filter(year >= 2000)

# Create county data frame; adjust for population and inflation
sba_county_7a <- sba_7a %>%
  group_by(FIPS, year) %>%
  summarise(
    sba_loan_7a = sum(sba_loan_7a),
    total_loan_7a = sum(total_loan_7a)) %>%
  ungroup() %>%
  stl_merge(sba_loan_7a:total_loan_7a, method = "sum") %>%
  COLA(sba_loan_7a:total_loan_7a, base_year = 2019, rpp = F) %>%
  per_capita_adj(sba_loan_7a:total_loan_7a, geog = "FIPS")

# Create zip code data frame; adjust for population and inflation
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

# Do the same for the SBA 504 program, though it is not included in the report
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
  stl_merge(total_loan_504, method = "sum") %>%
  complete(FIPS, year, fill = list(total_loan_504 = 0)) %>%
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

sba_county <- bind_df(sba_county_7a, sba_county_504, by = c("FIPS", "year"))
sba_zip    <- bind_df(sba_zip_7a, sba_zip_504)

# replace per capita data for zip codes with 0 population
sba_zip %<>% mutate_at(vars(contains("_pp")), ~ if_else(. == Inf, NA_real_, .))

sba_county %<>% select(FIPS, year, sba_loan_7a, total_loan_7a, total_loan_504, sba_loan_7a_pp, total_loan_7a_pp, total_loan_504_pp)
sba_zip %<>% select(zip, year, sba_loan_7a, total_loan_7a, total_loan_504, sba_loan_7a_pp, total_loan_7a_pp, total_loan_504_pp)

write_csv(sba_county, "output_data/sba_county.csv")
write_csv(sba_zip, "output_data/sba_zip.csv")

