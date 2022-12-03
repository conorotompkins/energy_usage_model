library(tidyverse)
library(pdftools)

extract_mcf <- function(x){
  
  x <- pdf_text(x)
  
  x <- str_split(x, "\n")[1]
  
  x |> 
    map(str_squish) |> 
    unlist() |> 
    enframe() |> 
    filter(str_detect(value, "MCF @"))
  
}

list.files("inputs/peoples_gas", pattern = ".pdf", full.names = TRUE) |> 
  set_names() |> 
  map_dfr(extract_mcf, .id = "bill_id") |> 
  separate(value, into = c("mcf", "price_cost"), sep = "@", remove = FALSE) |> 
  mutate(mcf = str_remove_all(mcf, "\\s"),
         mcf = str_remove(mcf, "MCF")) |> 
  mutate(price_cost = str_remove(price_cost, "\\s")) |> 
  #mutate(price_cost = str_replace(price_cost, "\\s", "_")) |> 
  separate(price_cost, into = c("price", "cost"), sep = "\\s", remove = TRUE) |> 
  mutate(across(c(mcf, price, cost), parse_number))
  glimpse()

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
  filter(str_detect(value, "MCF @"))
