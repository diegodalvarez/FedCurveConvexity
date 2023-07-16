require("arrow")
require("reshape2")
require("tidyverse")

# reading in treasury data
df_raw <- read_parquet(
  "yield_curve.parquet")

max_date <- max(df_raw$date)
min_date <- min(df_raw$date)

# we are going to have to cut some data out but let's make sure we are doing the best way
df_tsy_count <- df_raw %>%
  select(date, series_id) %>%
  group_by(series_id) %>%
  summarise(n = n()) %>%
  arrange(n)

# we can see that most if we exclude 1m Treasury we are left with around 10k for the rest
df_slice <- df_raw %>%
  filter(series_id != "DGS1MO") %>%
  select(date, series_id, value) %>%
  pivot_wider(names_from = series_id, values_from = value) %>%
  drop_na()

# make all of the fly tickers

df_tenors <- df_slice %>% 
  filter(date == min(date)) %>% 
  pivot_longer(!date) %>% 
  mutate(
    year = substr(name, 4, nchar(name)),
    is_month = substr(name, nchar(name) - 1, nchar(name)),
    year = if_else(is_month == "MO", as.numeric(substr(year, 1,1)) / 12, as.numeric(year))) %>% 
  select(-c(is_month, date, value)) %>% 
  arrange(year)

check_ascending <- function(x){
  
  if (all(x = sort(x))){return(x)}
  else{return(NULL)}
}

combinations <- combn(df_tenors$year, 3, FUN = check_ascending) 
df_butterfly <- tibble()

# this makes all of the butterflies
for (i in 1:length(combinations[1,])){
  
  tsy_tickers <- combinations[, i]
  col_name <- paste0(tsy_tickers, collapse = "_")
  
  df_tmp <- df_tenors %>% 
    filter(year %in% tsy_tickers) %>% 
    mutate(
      index = 1:n(),
      multiplier = if_else(index == 1 | index == 3, -1, 2)) %>% 
    rename("tsy_ticker" = name) %>% 
    select(-index)
  
  df_yield <- df_slice %>% 
    pivot_longer(!date, names_to = "tsy_ticker", values_to = "yield")
  
  df_combined <- df_yield %>% 
    inner_join(df_tmp, by = "tsy_ticker") %>% 
    mutate(new_yield = multiplier * yield) %>% 
    select(date, new_yield) %>% 
    group_by(date) %>% 
    summarise(value = sum(new_yield)) %>% 
    mutate(ticker = col_name)
  
  df_butterfly <- bind_rows(df_butterfly, df_combined)
}

start_date <- min(df_butterfly$date)
end_date <- max(df_butterfly$date)

df_butterfly_wider <- df_butterfly %>% 
  write_parquet("yc_flys.parquet") %>% 
  pivot_wider(names_from = ticker, values_from = value) %>% 
  select(-date)

fly_corr <- cor(df_butterfly_wider)
melted_fly_corr <- melt(fly_corr)

ggplot(data = melted_fly_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  xlab("Butterfly") +
  ylab("Butterfly") +
  labs(title = paste("Butterfly yield correlation from", start_date, "to", end_date))

df_butterfly %>% 
  write_parquet("fly.parquet")
