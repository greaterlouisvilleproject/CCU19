library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

cra <- readxl::read_excel("CRA/A1-13114021111.XLSX", col_names = F)

cra %<>%
  transmute(
    tract = ...2,
    num_under_100k = ...7,
    amount_under_100k = ...10,
    num_100k_250k = ...13,
    amount_100k_250k = ...16,
    num_250k_plus = ...19,
    amount_250k_plus = ...23,
    num_business_1m = ...27,
    amount_business_1m = ...33) %>%
  filter(str_detect(tract, "\\d{4}\\.\\d{2}")) %>%
  mutate_at(vars(-tract), as.numeric) %>%
  mutate_at(vars(contains("amount")), ~ . * 1000) %>%
  mutate(tract = "1400000US21111" %p% str_remove(tract, "\\."))

cra_tract <- cra %>%
  transmute(
    tract,
    total_loan = amount_under_100k + amount_100k_250k + amount_250k_plus,
    business_loan = amount_business_1m) %>%
  per_capita_adj(total_loan:business_loan, geog = "tract")

write_csv(cra_tract, "data/cra_tract.csv")
