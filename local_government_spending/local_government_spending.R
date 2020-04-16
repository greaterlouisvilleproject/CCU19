library(readr)

local_government <- read_csv("local_government_spending/Local Government Spending.csv", skip = 1,
                             col_names = c("FIPS", "local_spend"), col_types = "_cn__")

write_csv(local_government, "output_data/local_government_spending.csv")