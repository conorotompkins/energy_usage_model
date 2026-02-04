#etl
library(tidyverse)
library(janitor)
library(lubridate)

source("scripts/functions.R")

#read in peoples gas data
bill_pdfs <- list.files(
  "inputs/peoples_gas",
  pattern = ".pdf",
  full.names = TRUE
) |>
  set_names() |>
  enframe() |>
  select(name) |>
  rename(bill_id = name)

bill_data <- bill_pdfs |>
  mutate(bill_date = map_chr(bill_id, extract_bill_date)) |>
  mutate(usage_stats = map(bill_id, extract_usage)) |>
  unnest(usage_stats) |>
  mutate(bill_date = mdy(bill_date)) |>
  arrange(bill_date) |>
  mutate(type = "Gas usage") |>
  select(type, everything())

bill_data |>
  write_csv("inputs/clean_data/gas.csv")

#read in duquesne light data

read_dl_csv <- function(x) {
  x <- read_csv(x, skip = 4)

  if (!("UNITS" %in% colnames(x))) {
    colnames(x) <- c(
      "type",
      "date",
      "start_time",
      "end_time",
      "usage",
      "cost",
      "notes"
    )
    x <- x |> mutate(cost = NA, units = "kWh")
  } else {
    colnames(x) <- c(
      "type",
      "date",
      "start_time",
      "end_time",
      "usage",
      "units",
      "cost",
      "notes"
    )
  }
  x
}

energy_usage_files <- list.files(
  "inputs/duquesne_light",
  pattern = ".csv",
  full.names = TRUE
) |>
  set_names()

energy_usage_files |>
  map(read_dl_csv)

energy_df <- energy_usage_files |>
  map(read_dl_csv) |>
  list_rbind()

glimpse(energy_df)

energy_df |>
  write_csv("inputs/clean_data/electricity.csv")
