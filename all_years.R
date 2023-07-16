require("arrow")
require("ggplot2")
require("tidyverse")

df_raw <- read_parquet(
  "yc_pc.parquet")

df_raw %>% 
  filter(PC == "PC3") %>% 
  mutate(year = format(as.Date(date), "%Y")) %>% 
  group_by(year) %>% 
  mutate(index = 1:n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = index, y = value, color = year)) +
  geom_line(aes(col = year)) +
  labs(title = "Yield Curve Convexity by Year")
         