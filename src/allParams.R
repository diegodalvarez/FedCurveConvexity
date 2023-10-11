require("arrow")
require("moments")
require("tidyverse")

parent_path <- dirname(getwd())
params_path <- file.path(parent_path, "data", "all_params.parquet")

df_params <- read_parquet(
  params_path) %>% 
  mutate(date = as.Date(date))

df_rsquared <- df_params %>% 
  filter(stat == "rsquared") %>% 
  select(-stat)

df_pos <- df_rsquared %>% 
  select(ticker, tenor) %>% 
  unique() %>% 
  pivot_longer(!ticker) %>% 
  select(-name) %>% 
  group_by(ticker) %>% 
  mutate(pos = 1:n()) %>% 
  ungroup() %>% 
  rename("tenor" = value)

df_plot <- df_rsquared %>% 
  full_join(y = df_pos, by = c("ticker", "tenor")) %>% 
  select(-tenor)

sample_tickers <- c("0.25_0.5_1", "0.25_0.5_10", "0.25_0.5_2")

df_sample_plot <- df_plot %>% 
  filter(ticker %in% sample_tickers) 

df_avg <- df_sample_plot %>% 
  select(-ticker) %>% 
  group_by(date, pos) %>% 
  summarise(avg = mean(value)) %>% 
  ungroup()

ggplot(df_plot, aes(x = date, y = value, color = ticker)) +
  geom_line()

df_sample_plot %>% 
  ggplot(aes(x = date, y = value, color = ticker)) +
  ggplot(df_avg, aes(x = date, y = avg)) +
  facet_wrap(~pos, scale = "free", nrow = 3) +
  geom_line(alpha = 0.7, color = "grey") +
  geom_line()
