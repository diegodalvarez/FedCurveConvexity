require("arrow")
require("moments")
require("tidyverse")

df_params <- read_parquet(
  "params.parquet") %>% 
  mutate(date = as.Date(date))

df_moments <- df_params %>% 
  filter(frequency == "yearly") %>% 
  select(-c(frequency, month_year,  year, date)) %>% 
  group_by(tsy_new_ticker, param) %>% 
  summarise(
    mean = mean(value),
    std = sd(value),
    skew = skewness(value),
    kurtosis = kurtosis(value)) %>% 
  ungroup()

start_date <- min(df_params$date, na.rm = TRUE)
end_date <- max(df_params$date, na.rm = TRUE)

df_params %>% 
  filter(frequency == "yearly") %>% 
  select(-c(frequency, month_year, year, date)) %>% 
  ggplot(aes(x = value)) + 
  facet_wrap(~tsy_new_ticker + param, scale = "free") +
  geom_histogram(color = "black", bins = 30) +
  labs(title = paste("OLS Parameter Distribution (yearly) from", start_date, "to", end_date))

df_params %>% 
  filter(frequency == "monthly") %>% 
  select(-c(frequency, month_year, year, date)) %>% 
  ggplot(aes(x = value)) +
  facet_wrap(~tsy_new_ticker + param, scale = "free") +
  geom_histogram(color = "black", bins = 30) +
  labs(title = paste("OLS Parameter Distribution (Monthly) from", start_date, "to", end_date))

df_params %>% 
  filter(frequency == "roll") %>% 
  select(-c(frequency, month_year, year, date)) %>% 
  ggplot(aes(x = value)) +
  facet_wrap(~tsy_new_ticker + param, scale = "free") +
  geom_histogram(color = "black", bins = 50) +
  labs(title = paste("OLS Parameter Distribution (Rolling 1y) from", start_date, "to", end_date))
