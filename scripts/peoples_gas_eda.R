library(tidyverse)
library(lubridate)
library(pdftools)

extract_usage <- function(x){
  
  x <- pdf_text(x)
  
  x <- str_split(x, "\n")[1]
  
  x |> 
    map(str_squish) |> 
    unlist() |> 
    enframe() |> 
    filter(str_detect(value, "MCF @")) |> 
    separate(value, into = c("mcf", "price_cost"), sep = "@", remove = FALSE) |> 
    mutate(mcf = str_remove_all(mcf, "\\s"),
           mcf = str_remove(mcf, "MCF")) |> 
    mutate(price_cost = str_remove(price_cost, "\\s")) |> 
    #mutate(price_cost = str_replace(price_cost, "\\s", "_")) |> 
    separate(price_cost, into = c("price", "cost"), sep = "\\s", remove = TRUE) |> 
    mutate(across(c(mcf, price, cost), parse_number)) |> 
    select(-c(name, value))
  
}

extract_bill_date <- function(x){
  
  x <- pdf_text(x)
  
  x <- str_split(x, "\n")[1]
  
  date_location <- x |> 
    map(str_squish) |> 
    unlist() |> 
    enframe() |> 
    filter(str_detect(value, "^Account Number")) |> 
    slice_head(n = 1) |> 
    pull(name)
  
  x |> 
    map(str_squish) |> 
    unlist() |> 
    enframe() |> 
    filter(name == date_location + 1) |> 
    mutate(value = str_remove(value, "^\\d+"),
           value = str_remove(value, "^\\s"),
           value = str_sub(value, 1, 12)) |> 
    pull(value)
  
}

bill_pdfs <- list.files("inputs/peoples_gas", pattern = ".pdf", full.names = TRUE) |> 
  set_names() |> 
  enframe() |> 
  select(name) |> 
  rename(bill_id = name)

bill_data <- bill_pdfs |> 
  mutate(bill_date = map_chr(bill_id, extract_bill_date)) |> 
  mutate(usage_stats = map(bill_id, extract_usage)) |> 
  unnest(usage_stats) |> 
  mutate(bill_date = mdy(bill_date)) |> 
  arrange(bill_date)

bill_data |> 
  ggplot(aes(bill_date, mcf)) +
  geom_line()

bill_pdfs |> 
  filter(row_number() == 4) |> 
  pull() |> 
  extract_bill_date()

bill_pdfs |> 
  filter(row_number() == 1) |> 
  pull() |> 
  extract_usage()

date_location <- bill_pdfs |> 
  filter(row_number() == 4) |> 
  pull(bill_id) |> 
  pdf_text() |>
  str_split("\n") |> 
  map(str_squish) |> 
  unlist() |> 
  enframe() |> 
  filter(str_detect(value, "^Account Number")) |> 
  slice_head(n = 1) |> 
  pull(name)

bill_pdfs |> 
  filter(row_number() == 4) |> 
  pull(bill_id) |> 
  pdf_text() |>
  str_split("\n") |> 
  map(str_squish) |> 
  unlist() |> 
  enframe() |> 
  filter(row_number() == date_location + 1)

#get usage stats
bill_pdfs |> 
  map_dfr(extract_mcf, .id = "bill_id") |> 
  separate(value, into = c("mcf", "price_cost"), sep = "@", remove = FALSE) |> 
  mutate(mcf = str_remove_all(mcf, "\\s"),
         mcf = str_remove(mcf, "MCF")) |> 
  mutate(price_cost = str_remove(price_cost, "\\s")) |> 
  #mutate(price_cost = str_replace(price_cost, "\\s", "_")) |> 
  separate(price_cost, into = c("price", "cost"), sep = "\\s", remove = TRUE) |> 
  mutate(across(c(mcf, price, cost), parse_number))
  glimpse()
  
bill_pdfs |> 
  map_dfr(extract_bill_date)

test <- pdftools::pdf_text("inputs/peoples_gas/BillPDF_2022_10_13.pdf")


cat(test)

print(test)

test_split <- str_split(test, "\n")[1]

print(test_split)

str_split(test_split, "\n") |> 
  str()

test_split |> 
  map(str_squish) |> 
  unlist() |> 
  enframe() |> 
  filter(name == 6) |> 
  mutate(value = str_remove(value, "^\\d+"),
         value = str_remove(value, "^\\s"),
         value = str_sub(value, 1, 12)) |> 
  mutate(value = mdy(value)) |> 
  pull(value)
  
