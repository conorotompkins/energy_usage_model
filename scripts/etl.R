#etl
library(tidyverse)
library(janitor)
library(lubridate)

source("scripts/functions.R")

#read in peoples gas data
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
  write_csv("inputs/clean_data/gas.csv")

#read in duquesne light data
energy_df <- list.files("inputs/duquesne_light", pattern = ".csv", full.names = TRUE) |> 
  map_dfr(~read_csv(.x, skip = 4)) |> 
  clean_names()

energy_df |> 
  write_csv("inputs/clean_data/electricity.csv")