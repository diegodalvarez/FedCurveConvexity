require("arrow")
require("reshape2")
require("tidyverse")

# path management
parent_path <- dirname(getwd())
data_path <- file.path(parent_path, "data", "data.parquet")

# reading in treasury data
df_raw <- read_parquet(
  data_path) %>% 
  filter(symbol != "FEDFUNDS") %>% 
  drop_na()

max_date <- max(df_raw$date)
min_date <- min(df_raw$date)

df_count <- df_raw %>% 
  select(symbol, date) %>% 
  group_by(symbol) %>% 
  summarise(count = n()) %>% 
  arrange(count)

df_count %>% 
  ggplot(aes(x = reorder(symbol, -count), y = count)) +
  geom_bar(stat = "identity") +
  xlab("Treasury Ticker") +
  ylab("Count") +
  labs(title = "Treasury Data Count")

df_raw %>% 
  select(symbol, date) %>% 
  group_by(symbol) %>% 
  summarise(
    start = min(date),
    end = max(date))

df_slice <- df_raw %>% 
  filter(symbol != "DGS1MO") %>% 
  pivot_wider(names_from = "symbol", values_from = "price") %>% 
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

df_butterfly_change <- df_butterfly %>% 
  group_by(ticker) %>% 
  mutate(change = value / lag(value) - 1) %>% 
  select(-value) %>% 
  ungroup() %>% 
  drop_na()

df_butterfly_wider_change <- df_butterfly_change %>% 
  pivot_wider(names_from = "ticker", values_from = "change") %>% 
  select(-date)

fly_change_corr <- cor(df_butterfly_wider_change)
melted_fly_change_corr <- melt(fly_change_corr)

ggplot(data = melted_fly_change_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank()) +
  xlab("Butterfly") +
  ylab("Butterfly") +
  labs(title = paste("Butterfly Yield Change correlation from", start_date, "to", end_date))

fly_out_path <- file.path(parent_path, "data", "fly.parquet")

df_butterfly %>%
  write_parquet(fly_out_path)
