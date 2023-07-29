require("tidy")
require("dplyr")
require("arrow")
require("reshape2")
require("tidyverse")

df_fly <- read_parquet("fly.parquet")
df_yc <- read_parquet("yield_curve.parquet")

# the regression that they use is that 2s3s5s and then compare against those yield
# they also use 2013 and 2015

# separate out the 2s3s5s

two_three_five <- df_fly %>% 
  filter(ticker == "2_3_5")
  
tsy_tickers <- c("DGS2", "DGS3", "DGS5")
df_tsy <- df_yc %>% 
  filter(series_id %in% tsy_tickers) %>% 
  select(date, "tsy_ticker" = series_id, "tsy_yield" = value)

# two_three_five %>%
#   inner_join(df_tsy, by = "date") %>%
#   mutate(year = format(date, "%Y")) %>%
#   filter(year %in% c(2013, 2023)) %>%
#   ggplot(aes(x = value, y = tsy_yield)) +
#   facet_wrap(~year + tsy_ticker, scale = "free") +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   ylab("Treasury Yield") +
#   xlab("2s3s5s Yield") +
#   labs(title = "2s3s5s Regression against respective tenors")


df_combined <- two_three_five %>% 
  inner_join(df_tsy, by = "date") %>% 
  mutate(year = format(date, "%Y"))

df_combined_list <- split(df_combined, df_combined$year)
df_regression <- lapply(df_combined_list, function(df){
  
  model <- lm(value~tsy_yield, data = df)
  c(alpha = coef(model)[1],
    beta = coef(model)[2],
    rsquared = summary(model)$r.squared)
})

regression_output <- data.frame(do.call(rbind, df_regression)) %>% 
  rownames_to_column()

regression_output %>% 
  select(
    "year" = "rowname",
    "alpha" = "alpha..Intercept.",
    "beta" = "beta.tsy_yield",
    rsquared) %>% 
  pivot_longer(!year, names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = value)) +
  facet_wrap(~variable, scale = "free") +
  geom_histogram(bins = 20) +
  labs(title = "OLS Regression Results by year of 2s3s5s Butterfly vs. Respective Tenors")











































