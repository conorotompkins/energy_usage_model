#functions

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