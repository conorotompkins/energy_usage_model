library(tidyverse)
library(fable)
library(tsibble)
library(janitor)
library(lubridate)
library(ragg)

options(scipen = 999, digits = 4, use.ragg = TRUE)

theme_set(theme_bw())

energy_df <- list.files("inputs/duquesne_light", pattern = ".csv", full.names = TRUE) |> 
  map_dfr(~read_csv(.x, skip = 4)) |> 
  clean_names()

glimpse(energy_df)

energy_df <- energy_df |> 
  mutate(date_time = str_c(date, start_time, sep = " "),
         date_time = ymd_hms(date_time),
         year = year(date),
         month = month(date, label = T, abbr = FALSE),
         dow = wday(date, label = T),
         start_hour = hour(start_time)) |> 
  mutate(year = as.factor(year))

glimpse(energy_df)

#total timeline
energy_df |>
  ggplot(aes(date_time, usage)) +
  geom_point(alpha = .1, size = .3) +
  geom_smooth(span = .1)

#compare hourly usage by month
compare_hour_by_month <- energy_df |> 
  ggplot(aes(start_hour, usage)) +
  geom_line(aes(group = date), alpha = .04) +
  geom_smooth(aes(color = year), se = FALSE, span = .3) +
  facet_wrap(vars(month), scales = "free_y") +
  labs(x = "Hour",
       y = "kWh")

compare_hour_by_month

ggsave(filename = "outputs/compare_hour_by_month.png",
       width = 20, height = 12)

#compare day of week by month
dow_df <- energy_df |> 
  group_by(date, year, month, dow) |> 
  summarize(usage = sum(usage)) |> 
  ungroup()

dow_df |> 
  ggplot(aes(dow, usage)) +
  geom_boxplot(aes(group = dow)) +
  geom_smooth(aes(color = year, group = year), se = FALSE) +
  facet_wrap(vars(month), scales = "free_y") +
  theme_bw()