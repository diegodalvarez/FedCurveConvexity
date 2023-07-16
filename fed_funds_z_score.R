require("roll")
require("arrow")
require("ggplot2")
require("tidyverse")

fed_funds_raw <- read_parquet("fedfunds.parquet")

df_pc_raw <- read_parquet(
  "yc_pc.parquet")

fed_funds_prep <- fed_funds_raw %>% 
  select(date, value) %>% 
  mutate(
    roll_mean = roll_mean(value, width = 30),
    roll_std = roll_sd(value, width = 30),
    roll_z_score = (value - roll_mean) / roll_std) %>% 
  drop_na()

start_date <- min(fed_funds_raw$date)
end_date <- max(fed_funds_raw$date)

fed_funds_prep %>% 
  ggplot(aes(x = date, y = roll_z_score)) +
  geom_line() +
  labs(title = paste("Fed Funds 30d Z-Score from", start_date, "to", end_date)) +
  ylab("Rolling Z-Score")

fed_funds_filter <- fed_funds_prep %>% 
  filter(roll_z_score > 2) %>% 
  mutate(year = format(as.Date(date, "%Y"), "%Y"))

years <- fed_funds_filter %>% 
  select(year) %>% 
  unique() %>% 
  pull()

df_pc_filter <- df_pc_raw %>% 
  filter(PC == "PC3") %>% 
  mutate(year = format(as.Date(date), "%Y")) %>% 
  filter(year %in% years) %>% 
  group_by(year) %>% 
  mutate(index = 1:n()) %>% 
  ungroup() 

df_pc_filter %>% 
  ggplot(aes(x = index, y = value, group = year)) +
  geom_line(aes(col = year)) +
  xlab("") +
  ylab("PC3") +
  labs(
    title = "Fed Funds Futures Curve Convexity During Higher Fed Rate Hikes using Rolling Z-Score")

df_pc3 <- df_pc_raw %>% 
  filter(PC == "PC3") %>% 
  mutate(year = format(as.Date(date), "%Y"))

df_seperated <- df_pc3 %>% 
  mutate(group_name = if_else(year %in% years, "High FF Z-Score", "Low FF Z-Score")) %>% 
  group_by(year) %>% 
  arrange(date) %>% 
  mutate(n = 1:n())

df_seperated %>% 
  ggplot(aes(x = n, y = value, group = year)) +
  geom_line(aes(col = year)) +
  facet_wrap(~group_name, scale = "free") 

df_seperated %>% 
  select(value, group_name, n) %>% 
  group_by(group_name) %>% 
  arrange(n) %>% 
  summarise(mean = mean())
