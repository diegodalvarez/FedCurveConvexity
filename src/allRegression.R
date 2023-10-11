require("roll")
require("arrow")
require("tidyverse")
require("latex2exp")

parent_path <- dirname(getwd())
tsy_data_path <- file.path(parent_path, "data", "data.parquet")
fly_data_path <- file.path(parent_path, "data", "fly.parquet")
out_path <- file.path(parent_path, "data", "all_params.parquet")

# read in data
df_fly <- read_parquet(fly_data_path)
df_yield <- read_parquet(tsy_data_path)

# name each fly
df_sep <- df_fly %>% 
  select(ticker) %>% 
  unique() %>% 
  group_by(ticker) %>% 
  mutate(
    fly = 1:n(),
    wing1 = str_split(ticker, "_")[[1]][1],
    body = str_split(ticker, "_")[[1]][2],
    wing2 = str_split(ticker, "_")[[1]][3]) %>% 
  ungroup() %>% 
  pivot_longer(!c(ticker,fly), values_to = "tenor") %>% 
  select(-name)

df_name <- df_fly %>% 
  pivot_longer(!c(date, ticker)) %>% 
  select(-name) %>% 
  full_join(y = df_sep, by = "ticker", relationship = "many-to-many")

df_tsy_name <- df_name %>% 
  select(tenor) %>% 
  unique() %>% 
  mutate(tenor = as.numeric(tenor)) %>% 
  arrange(tenor) %>% 
  mutate(
    symbol = c(
      "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS3", "DGS5", "DGS7", "DGS10",
      "DSG30"))

df_tsy <- df_tsy_name %>%
  mutate(tenor = as.character(tenor)) %>% 
  full_join(y = df_name, by = "tenor", relationship = "many-to-many")

df_combine <- df_tsy %>% 
  inner_join(y = df_yield, by = c("symbol", "date"), relationship = "many-to-many")

regress <- function(df){
  
  df_order <- df %>% arrange(date)
  
  regression <- roll_lm(
    x = df_order$price, y = df_order$value, width = 90)
  
  df_params <- regression$coefficients %>% 
    as_tibble()
  
  colnames(df_params) <- c("alpha", "beta")
  
  df_date <- df_params %>% 
    mutate(
      date = df_order$date,
      rsquared = regression$r.squared[,]) %>% 
    drop_na()
  
  df_out <- df_date %>% 
    pivot_longer(!date, names_to = "stat", values_to = "value")
  
  return(df_out)
}

df_regress <- df_combine %>% 
  group_by(ticker, tenor) %>% 
  group_modify(~regress(.)) %>% 
  ungroup()

df_regress %>% 
  write_parquet(out_path)
