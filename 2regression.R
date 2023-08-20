require("arrow")
require("tidyverse")
require("latex2exp")

# read in data
df_fly <- read_parquet("fly.parquet")
df_yield <- read_parquet("yield_curve.parquet")

# let's just use 2-3-5 yield
df_fly_prep <- df_fly %>% 
  filter(ticker == "2_3_5") %>% 
  rename("fly_ticker" = ticker)

df_yield_prep <- df_yield %>% 
  filter(series_id %in% c("DGS2", "DGS3", "DGS5")) %>% 
  select(date, "tsy_ticker" = series_id, "yield" = value)

col_rename <- tibble(
  "tsy_ticker" = c("DGS2", "DGS3", "DGS5"),
  "tsy_new_ticker" = c("2y", "3y", "5y"))

df_combined <- df_fly_prep %>% 
  inner_join(df_yield_prep, by = "date") %>% 
  inner_join(col_rename, by = "tsy_ticker") %>% 
  mutate(value = value * 100) %>% 
  rename(
    "fly_yield" = "value",
    "tsy_yield" = "yield")

start_date <- min(df_combined$date)
end_date <- max(df_combined$date)

df_combined %>% 
  ggplot(aes(x = tsy_yield, y = fly_yield, group = tsy_new_ticker)) +
  facet_wrap(~tsy_ticker, scale = "free") +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fly Yield (%)") +
  xlab("TSY Yield (%)") +
  labs(title = paste("Regression of Butterfly Yield vs. Respective Wings from", start_date, "to", end_date))

df_combined_year <- df_combined %>% 
  mutate(year = format(date, "%Y"))

df_combined_year %>% 
  ggplot(aes(x = tsy_yield, y = fly_yield, group = tsy_new_ticker, color = year)) +
  facet_wrap(~tsy_ticker, scale = "free") +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fly Yield (%)") +
  xlab("TSY Yield (%)") +
  labs(title = paste("Regression of Butterfly Yield vs. Respective wings from", start_date, "to", end_date, "color by year")) +
  theme(legend.position = "none")

regression <- function(df){
  
  lm_model <- lm(fly_yield ~ tsy_yield, df)
  coefs <- coef(lm_model)
  alpha <- coefs[[1]]
  beta <- coefs[[2]]
  rsquared <- summary(lm_model)$r.squared
  
  output_tibble <- tibble(
    param = c("alpha", "beta", "rsquared"),
    value = c(alpha, beta, rsquared))
  
  return(output_tibble)
}

df_params <- df_combined_year %>% 
  group_by(year, tsy_new_ticker) %>% 
  do(regression(.)) %>% 
  ungroup()

df_params %>% 
  filter(param == "rsquared") %>% 
  ggplot(aes(x = year, y = value, group = tsy_new_ticker)) +
  facet_wrap(~tsy_new_ticker, scale = "free", nrow = 3) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = TeX(paste("$R^2$ of Butterfly ~ Wings from", start_date, "to", end_date)))

df_params %>% 
  mutate(date = as.Date(year, format = "%Y")) %>% 
  ggplot(aes(x = year, y = value)) +
  facet_wrap(~param + tsy_new_ticker, scale = "free") +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = paste("Regression Parameters of butterfly vs wings by year from", start_date, "to", end_date))

df_monthly_params <- df_combined_year %>% 
  select(-year) %>% 
  mutate(month_year = format(date, "%Y-%m")) %>% 
  group_by(month_year, tsy_new_ticker) %>% 
  do(regression(.)) %>% 
  ungroup()

df_monthly_params %>% 
  filter(param == "rsquared") %>% 
  ggplot(aes(x = month_year, y = value)) +
  facet_wrap(~tsy_new_ticker, scale = "free_y", nrow = 3,) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = TeX(paste("$R^2$ of Butterfly ~ Wings from", start_date, "to", end_date)))
