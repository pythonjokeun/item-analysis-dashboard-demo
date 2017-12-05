library(tidyverse)
library(lubridate)

# # download data
# download.file(
#   "http://archive.ics.uci.edu/ml/machine-learning-databases/00352/Online%20Retail.xlsx",
#   "data.xlsx"
# )

dataset = readxl::read_excel("data.xlsx") %>%
  janitor::clean_names()

# cleansing
clean = dataset %>%
  filter(
    !is.na(customerid),
    quantity > 0,
    unitprice > 0,
    str_length(stockcode) >= 5,
    str_length(stockcode) < 12
  ) %>%
  mutate(total_spend = quantity * unitprice)

# identify valuable items
pareto = clean %>%
  select(stockcode, total_spend) %>%
  group_by(stockcode) %>%
  summarise(total_spend = sum(total_spend)) %>%
  ungroup() %>%
  arrange(desc(total_spend)) %>%
  mutate(
    running_trx = cumsum(total_spend),
    running_trx_prop = running_trx / sum(total_spend),
    class = case_when(
      running_trx_prop <= 0.8 ~ "A",
      running_trx_prop <= 1 ~ "B"
    )
  ) %>%
  select(stockcode, class)

# valuable items
pareto %>%
  filter(class == "A") %>%
  left_join(clean, by = "stockcode") %>%
  select(stockcode, description, invoicedate, quantity, country) %>%
  write_csv("data.csv")
