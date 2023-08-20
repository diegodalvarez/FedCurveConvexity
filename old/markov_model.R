require("MSwM")
require("roll")
require("arrow")
require("ggplot2")
require("tidyverse")

df_raw <- read_parquet("fedfunds.parquet")

# exploratory Fed Funds Work
start <- min(df_raw$date)
end <- max(df_raw$date)

df_raw %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  ylab("Rate %") +
  labs(title = paste("Fed Funds Effective Rate from", start, "to", end))

# Markov Regime Switching
df_fed_lagging <- df_raw %>% 
  drop_na() %>% 
  select(date, "current" = value) %>% 
  mutate(lag = lag(current)) %>% 
  drop_na()

# making ols for fed funds and markov regime switching model
df_fed_ols <- lm(current ~ lag, data = df_fed_lagging)
fed_markov_regime <- msmFit(
  df_fed_ols,
  k = 2, p = 0,
  sw = rep(TRUE, 3),
  control = list(parallel = F))

# getting probabilities from markov regime switching
df_fed_probs <- fed_markov_regime@Fit@filtProb %>% 
  as.data.frame() %>% 
  tibble() %>% 
  mutate(date = df_fed_lagging$date)

# plotting those probabilities
df_fed_probs %>% 
  pivot_longer(!date) %>% 
  ggplot(aes(x = date, y = value, group = name)) +
  facet_wrap(~name) +
  geom_line()

# smoothing out the probabilities over a 2 year window
df_rolling_mean_prob <- df_fed_probs %>% 
  rename(
    "Low Rate Regime" = V1,
    "High Rate Regime" = V2) %>% 
  pivot_longer(!date) %>% 
  group_by(name) %>% 
  mutate(roll_mean = roll_mean(value, width = 24)) %>%
  ungroup() %>% 
  select(-value) %>% 
  drop_na()

# plotting the smoothed out probabilities 
df_rolling_mean_prob %>% 
  ggplot(aes(x = date, y = roll_mean, group = name)) +
  facet_wrap(~name) +
  geom_line()

# making an indicator function
regime_mode_change <- df_rolling_mean_prob %>% 
  pivot_wider(names_from = "name", values_from = "roll_mean") %>% 
  rename(
    "regime1" = "Low Rate Regime",
    "regime2" = "High Rate Regime") %>% 
  pivot_longer(!date) %>% 
  mutate(regime_mode = if_else(value > 0.5, 1, 0))

# this gets the number of regime changes
regime_num <- regime_mode_change %>% 
  filter(name == "regime1") %>% 
  mutate(regime_mode_change = abs(regime_mode / lag(regime_mode) - 1)) %>% 
  drop_na() %>% 
  filter(regime_mode_change != "Inf" & regime_mode_change > 0) %>% 
  select(date) %>% 
  mutate(
    regime_num = 1:n(),
    regime_start = lag(date)) %>% 
  rename("regime_end" = date) %>% 
  select(regime_start, regime_end, regime_num)

# we need a regime for the current regime we are in
max_date <- max(regime_num$regime_end)
add_on = tibble(
  regime_start = max_date, 
  regime_num = length(regime_num$regime_num) + 1,
  regime_end = NA)

full_regime_num <- regime_num %>% 
  bind_rows(add_on)

full_regime_num %>%
  write_parquet("regime_periods.parquet")
