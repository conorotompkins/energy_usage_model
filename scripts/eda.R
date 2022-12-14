library(tidyverse)
library(fable)
library(tsibble)
library(janitor)
library(lubridate)
library(ragg)

options(scipen = 999, digits = 4, use.ragg = TRUE)

theme_set(theme_bw())

energy_df <- read_csv("inputs/clean_data/electricity.csv") |> 
  mutate(date_time = str_c(date, start_time, sep = " "),
         date_time = ymd_hms(date_time),
         year = year(date),
         month = month(date, label = T, abbr = FALSE),
         dow = wday(date, label = T),
         start_hour = hour(start_time)) |> 
  filter(!(year == 2018 & month == "September"))

glimpse(energy_df)

#total timeline
energy_df |>
  ggplot(aes(date_time, usage)) +
  geom_point(alpha = .1, size = .3) +
  geom_smooth(span = .1) +
  geom_smooth(method = "lm")

energy_df |>
  ggplot(aes(date_time, usage)) +
  geom_density2d_filled()

energy_df |>
  ggplot(aes(x = date_time, y = usage)) +
  geom_bin_2d(aes(alpha = after_stat(count)))

#compare hourly usage by month
compare_hour_by_month <- energy_df |> 
  mutate(year = as.factor(year)) |> 
  ggplot(aes(start_hour, usage)) +
  geom_line(aes(group = date), alpha = .02) +
  geom_smooth(aes(color = year), se = FALSE, span = .3) +
  facet_wrap(vars(month), scales = "free_y") +
  labs(x = "Hour",
       y = "kWh")

compare_hour_by_month

ggsave(filename = "outputs/compare_hour_by_month.png",
       width = 20, height = 12)

#compare day of week by month
dow_df <- energy_df |> 
  mutate(year = as.factor(year)) |> 
  group_by(date, year, month, dow) |> 
  summarize(usage = sum(usage)) |> 
  ungroup()

dow_df |> 
  ggplot(aes(dow, usage)) +
  geom_boxplot(aes(group = dow)) +
  geom_smooth(aes(color = year, group = year), se = FALSE) +
  facet_wrap(vars(month), scales = "free_y") +
  theme_bw()

#peoples gas
gas_df <- read_csv("inputs/clean_data/gas.csv") |> 
  mutate(year = year(bill_date),
         month = month(bill_date, label = TRUE, abbr = FALSE))

gas_df |> 
  ggplot(aes(bill_date, mcf)) +
  geom_line()

gas_df |> 
  mutate(year = as.factor(year)) |> 
  ggplot(aes(month, mcf, color = year, group = year)) +
  geom_line()

#combined
combined_usage <- energy_df |> 
  group_by(type, year, month) |> 
  summarize(usage = sum(usage)) |> 
  bind_rows(gas_df |> 
              select(type, year, month, mcf) |> 
              rename(usage = mcf)) |> 
  mutate(date = str_c(year, month, "01", sep = "-"),
         date = ymd(date))

combined_usage |> 
  ggplot(aes(date, usage, color = type)) +
  geom_line() +
  facet_wrap(vars(type), scales = "free_y", ncol = 1)

#model
combined_usage_ts <- combined_usage |> 
  mutate(ym = yearmonth(date)) |> 
  as_tsibble(key = type, index = ym)

combined_usage_ts |> 
  scan_gaps()

test_model <- combined_usage_ts |> 
  model(arima = ARIMA(log(usage + 1)),
        lm = TSLM(log(usage + 1) ~ trend() + season()))

forecasts <- test_model |> 
  forecast(h = "6 months") |> 
  bind_rows(combined_usage_ts |> 
              rename(yvar = usage)) |> 
  mutate(yvar = coalesce(yvar, .mean)) |> 
  mutate(upper = hilo(usage)$upper,
         lower = hilo(usage)$lower) |>
  arrange(type, ym) |> 
  replace_na(list(.model = "actual"))

forecasts |> 
  ggplot(aes(ym, yvar, color = .model, fill = .model)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  geom_line(aes(lty = .model != "actual")) +
  facet_wrap(vars(type), scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("black", "#F8766D", "#00BFC4")) +
  scale_color_manual(values = c("black", "#F8766D", "#00BFC4"))
