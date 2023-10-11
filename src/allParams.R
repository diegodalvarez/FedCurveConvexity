require("arrow")
require("moments")
require("latex2exp")
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

# the plot is very hard for ggplot to do
df_avg <- df_plot %>% 
  select(-ticker) %>% 
  group_by(date, pos) %>% 
  summarise(avg = mean(value)) %>% 
  ungroup()

df_rename <- tibble(
  name = c("lower wing", "body", "upper wing"),
  pos = c(1,2,3))

start_date <- min(df_avg$date)
end_date <- max(df_avg$date)

df_avg %>% 
  full_join(y = df_rename, by = "pos") %>% 
  ggplot(aes(x = date, y = avg)) +
  facet_wrap(~name, scale = "free_y", nrow = 3) +
  geom_line() +
  labs(title = TeX(paste(
    "Cross-Sectional average of all $R^2$ of every regression from", start_date, "to", end_date)))
