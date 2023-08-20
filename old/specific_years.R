require("arrow")
require("ggplot2")
require("tidyverse")

df_raw <- read_parquet(
  "yc_pc.parquet")

historical_pc <- df_raw %>% 
  pivot_wider(names_from = "PC", values_from = "value")

# let's just look at the dates that they have given us
manual_dates <- tibble(
  period = c("1998-1999", "1994-1995", "1999-2000", "2005-2006", "2017-2018", "2022-2023"),
  start = c(1998, 1994, 1999, 2005, 2017, 2022),
  end = c(1999, 1995, 2000, 2006, 2018, 2023)) %>%
  pivot_longer(!period, values_to = "year")

manual_pcs <- historical_pc %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  inner_join(manual_dates, by = "year", relationship = "many-to-many") %>%
  group_by(period) %>%
  arrange(date) %>%
  mutate(index = 1:n()) %>%
  ungroup()

manual_pcs %>%
  ggplot(aes(x = index, y = PC3, group = period)) +
  geom_line(aes(col = period)) +
  labs(title = paste("3rd Principal Component During Over Specific Years"))

years <- c(1998, 1999, 1994, 1995, 1999, 2000, 2005, 2006, 2017, 2018, 2022, 2023)

historical_pc %>% 
  select(-c(PC2, PC1)) %>% 
  pivot_longer(!date, names_to = "PC", values_to = "value") %>% 
  mutate(
    year = format(as.Date(date), "%Y"),
    group_name = if_else(year %in% years, "group_1", "group_2")) %>% 
  group_by(year) %>% 
  mutate(index = 1:n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = index, y = value, group = year)) + 
  facet_wrap(~group_name, scale = "free") +
  geom_line(aes(col = year))

p1 <- historical_pc %>% 
  select(-c(PC2, PC1)) %>% 
  pivot_longer(!date, names_to = "PC", values_to = "value") %>% 
  mutate(
    year = format(as.Date(date), "%Y"),
    group = if_else(year %in% years, "Rate Hike Cycle", "Non-Rate Hike Cycle")) %>% 
  group_by(year) %>% 
  mutate(index = 1:n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = index, y = value, group = year)) + 
  facet_wrap(~group, scale = "free") +
  geom_line(alpha = 0.3)

p2 <- historical_pc %>% 
  select(-c(PC2, PC1)) %>% 
  pivot_longer(!date, names_to = "PC", values_to = "value") %>% 
  mutate(
    year = format(as.Date(date), "%Y"),
    group = if_else(year %in% years, "Rate Hike Cycle", "Non-Rate Hike Cycle")) %>% 
  group_by(year) %>% 
  arrange(date) %>% 
  mutate(index = 1:n()) %>% 
  ungroup() %>% 
  select(value, group, index) %>% 
  group_by(group, index) %>% 
  summarize(value = mean(value)) %>%
  ungroup() %>% 
  ggplot(aes(x = index, y = value, group = group)) +
  facet_wrap(~group, scale = "free") +
  geom_line()

p1 + p2
