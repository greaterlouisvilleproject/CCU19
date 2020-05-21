library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

fortune <- readxl::read_excel("pitchbook/PitchBook_1000.xlsx", skip = 7)

fortune %<>%
  transmute(
    name = `Company Name`,
    location = `HQ Location`,
    zip = `HQ Post Code`) %>%
  left_join(MSA_zip, by = "zip") %>%
  group_by(MSA) %>%
  summarise(fortune_1000 = n()) %>%
  per_capita_adj(fortune_1000, keep_pop = T) %>%
  pull_peers(add_info = F) %>%
  mutate(fortune_1000_per_lou = fortune_1000_pp * population[MSA == "31140"])

  
VC <- readxl::read_excel(path %p% "PitchBook_VC.xlsx", skip = 7,
                         col_types = c("text", "text", "text", "text", "date",
                                       "numeric", "text", "text"))

VC %<>%
  transmute(
    name = `Company Name`,
    zip = `Company Post Code`,
    year = `Deal Date` %>% as.character() %>% str_sub(1, 4) %>% as.numeric(),
    amount = `Deal Size`) %>%
  left_join(MSA_zip, by = "zip") %>%
  group_by(MSA, year) %>%
  summarise(
    deals = n(),
    deals_amount = n() - sum(is.na(amount)),
    total_dollars = sum(amount, na.rm = TRUE) * 1000000,
    avg_deal = total_dollars / deals_amount,
    median_deal = median(amount, na.rm = TRUE) * 1000000) %>%
  ungroup() %>%
  filter(!is.na(MSA)) %>%
  COLA(total_dollars:median_deal, rpp = F) %>%
  left_join(pop_df, by = "MSA") %>%
  mutate(
    deals_per_100000 = deals / population * 100000,
    total_dollars_pc = total_dollars / population) %>%
  transmute(
    MSA, year,
    sex = "total", race = "total",
    deals, deals_per_100000,
    total_dollars, total_dollars_pc,
    avg_deal, median_deal)

pitchbook_msa_1yr <- VC

update_sysdata(pitchbook_msa_1yr)
