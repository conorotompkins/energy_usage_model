library(tidyverse)
library(fable)
library(tsibble)
library(fabletools)
library(janitor)
library(lubridate)
library(ragg)
library(scales)
library(plotly)
library(tictoc)

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
  geom_point(alpha = .01, size = .3) +
  geom_smooth(span = .1) +
  geom_smooth(method = "lm", lty = 2)

energy_df |>
  ggplot(aes(date_time, usage)) +
  geom_density2d_filled()

energy_df |>
  ggplot(aes(date_time, usage)) +
  geom_hex() +
  scale_fill_viridis_c()

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
  ungroup() |> 
  bind_rows(gas_df |> 
              select(type, year, month, mcf) |> 
              rename(usage = mcf)) |> 
  mutate(date = str_c(year, month, "01", sep = "-"),
         date = ymd(date))

combined_usage <- combined_usage |> 
  mutate(ym = yearmonth(date)) |> 
  mutate(wfh_flag = case_when(ym >= yearmonth("2020-03-01") ~ TRUE,
                              ym < yearmonth("2020-03-01") ~ FALSE),
         new_ac_flag = case_when(ym >= yearmonth("2019-10-01") ~ TRUE,
                                 ym < yearmonth("2019-10-01") ~ FALSE))

combined_usage |> 
  group_by(wfh_flag) |> 
  summarize(first_date = min(ym),
            last_date = max(ym))

combined_usage |> 
  group_by(new_ac_flag) |> 
  summarize(first_date = min(ym),
            last_date = max(ym))

combined_usage |> 
  ggplot(aes(date, usage)) +
  geom_line() +
  geom_vline(xintercept = ymd("2020-03-01"), lty = 2) +
  geom_vline(xintercept = ymd("2019-10-01"), lty = 2) +
  facet_wrap(vars(type), scales = "free_y", ncol = 1)

#model
combined_usage_ts <- combined_usage |> 
  as_tsibble(key = type, index = ym)

combined_usage_ts |> 
  scan_gaps()

test_model <- combined_usage_ts |> 
  model(lm = TSLM(log(usage + 1) ~ trend() + season()),
        arima = ARIMA(log(usage + 1)),
        arima_wfh = ARIMA(log(usage + 1) ~ wfh_flag),
        arima_new_ac = ARIMA(log(usage + 1) ~ new_ac_flag),  
        arima_context = ARIMA(log(usage + 1) ~ wfh_flag + new_ac_flag)
        )

test_model |> 
  glance() |> 
  view()

test_model |> 
  tidy() |> 
  view()

future_hypothetical <- new_data(combined_usage_ts, n = 12) |> 
  mutate(wfh_flag = TRUE,
         new_ac_flag = TRUE)

#compare basic models
basic_forecasts <- test_model |> 
  forecast(new_data = future_hypothetical) |> 
  bind_rows(combined_usage_ts |> 
              rename(yvar = usage)) |> 
  mutate(yvar = coalesce(yvar, .mean)) |> 
  mutate(upper = hilo(usage)$upper,
         lower = hilo(usage)$lower) |>
  arrange(type, ym) |> 
  replace_na(list(.model = "actual")) |> 
  filter(.model %in% c("actual", "lm", "arima"))

colors_needed <- basic_forecasts |> 
  distinct(.model) |> 
  nrow() - 1

palette <- c("black", hue_pal()(colors_needed))

show_col(palette)

basic_forecasts |> 
  ggplot(aes(ym, yvar, color = .model, fill = .model)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  geom_line(aes(lty = .model != "actual")) +
  facet_wrap(vars(type), scales = "free_y", ncol = 1) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette)

#compare models with context
context_forecasts <- test_model |> 
  forecast(new_data = future_hypothetical) |> 
  bind_rows(combined_usage_ts |> 
              rename(yvar = usage)) |> 
  mutate(yvar = coalesce(yvar, .mean)) |> 
  mutate(upper = hilo(usage)$upper,
         lower = hilo(usage)$lower) |>
  arrange(type, ym) |> 
  replace_na(list(.model = "actual")) |> 
  filter(str_detect(.model, "actual|arima"))

colors_needed <- context_forecasts |> 
  distinct(.model) |> 
  nrow() - 1

palette <- c("black", hue_pal()(colors_needed))

show_col(palette)

fc_plot <- context_forecasts |> 
  ggplot(aes(ym, yvar, color = .model, fill = .model)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  geom_line(aes(lty = .model != "actual")) +
  facet_wrap(vars(type), scales = "free_y", ncol = 1) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette)

ggplotly(fc_plot)

#special model for hourly energy
#for some reason there are duplicates for 1 AM on some dates. did the rate change mid-hour?
energy_df |> 
  duplicates(key = type, index = date_time)

energy_ts <- energy_df |>
  group_by(type, date_time) |> 
  summarize(usage = mean(usage)) |> 
  ungroup() |> 
  as_tsibble(key = type, index = date_time) |> 
  fill_gaps(usage = 0)

energy_ts |> 
  scan_gaps()

energy_ts |> 
  as_tibble() |> 
  filter(ymd(str_sub(date_time, 1, 10)) == ymd("2019-03-10"))

tic()
energy_model <- energy_ts |> 
  model(arima = ARIMA(log(usage + 1)),
        arima_k2 = ARIMA(log(usage + 1) ~ fourier(K = 2)),
        #arima_k3 = ARIMA(log(usage + 1) ~ fourier(K = 3)),
        arima_k4 = ARIMA(log(usage + 1) ~ fourier(K = 4))
        )
toc()

energy_model |> 
  glance()

energy_model |> 
  tidy()

energy_model |> 
  accuracy()

energy_fc <- energy_model |> 
  forecast(h = "1 month") |> 
  bind_rows(energy_ts |> 
              rename(yvar = usage)) |> 
  mutate(yvar = coalesce(yvar, .mean)) |> 
  mutate(upper = hilo(usage)$upper,
         lower = hilo(usage)$lower) |>
  arrange(type, date_time) |> 
  replace_na(list(.model = "actual")) |> 
  filter(.model %in% c("actual", "arima_k2"))

colors_needed <- energy_fc |> 
  distinct(.model) |> 
  nrow() - 1

palette <- c("black", hue_pal()(colors_needed))

show_col(palette)

energy_fc_plot <- energy_fc |> 
  filter(date_time >= max(energy_ts$date_time) - months(1)) |> 
  ggplot(aes(date_time, yvar, color = .model, fill = .model)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1) +
  #geom_point(alpha = .1, size = .3) +
  geom_line() +
  facet_wrap(vars(type), scales = "free_y", ncol = 1) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette)

energy_fc_plot

forecast(energy_model, new_data(energy_ts, n = 24*7)) %>%
  autoplot(energy_ts |> 
             filter(date_time >= max(energy_ts$date_time) - months(1))) +
  facet_wrap(vars(.model))
