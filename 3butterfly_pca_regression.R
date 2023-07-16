require("arrow")
require("reshape2")
require("tidyverse")

df_pca <- read_parquet("yc_pcs.parquet")
df_butterfly <- read_parquet("fly.parquet")

df_butterfly_rtn <- df_butterfly %>% 
  group_by(ticker) %>% 
  mutate(rtn = value / lag(value) - 1) %>% 
  drop_na() %>% 
  select(-value) %>% 
  ungroup()

pca3 <- df_pca %>% 
  filter(pc == "PC3") %>% 
  select(-c(var_scale, pc))
