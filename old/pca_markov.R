require("arrow")
require("ggplot2")
require("lubridate")
require("tidyverse")

df_pc_raw <- read_parquet(
  "yc_pc.parquet")

df_regime_raw <- read_parquet(
  "regime_periods.parquet") 

min_date <- min(df_pc_raw$date)
max_date <- max(df_pc_raw$date)

df_regime_prep <- df_regime_raw %>% 
  filter(regime_start > min_date) %>% 
  mutate(
    regime_end = replace_na(as.Date(regime_end), max_date),
    period_duration = regime_end - regime_start,
    weekday = wday(regime_start),
    date = if_else(weekday == 1 | weekday == 7, regime_start + 2, regime_start + 1))

df_combined <- df_pc_raw %>% 
  filter(PC == "PC3") %>% 
  full_join(df_regime_prep, by = "date") %>% 
  fill(regime_start, regime_end, regime_num, period_duration, weekday) %>% 
  drop_na() %>% 
  group_by(regime_num) %>% 
  mutate(index = 1:n()) %>% 
  ungroup()

df_combined %>% 
  ggplot(aes(x = index, y = value, group = regime_num)) +
  geom_line(aes(col = regime_num)) +
  labs(title = "Curve Convexity using Markov Regime Switching periods (Didn't work and bad coloring)")
