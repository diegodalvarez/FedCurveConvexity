require("arrow")
require("tidyverse")

# reading in treasury data
df_raw <- read_parquet(
  "yield_curve.parquet")

max_date <- max(df_raw$date)
min_date <- min(df_raw$date)

# plotting the treasury
df_raw %>%
  rename(
    "yield" = value,
    "Tenor" = series_id) %>%
  ggplot(aes(x = date, y = yield, color = Tenor)) +
  geom_line() +
  labs(
    title = paste("Treasury Yield from", min_date, "to", max_date))

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

# we keep it in wider and pass it through the PCA
pca_model <- prcomp(
  df_slice %>% select(-date),
  scale = TRUE,
  center = TRUE,
  rank = 3)

# getting historical PCs
historical_pc <- pca_model$x %>%
  as_tibble() %>%
  mutate(date = df_slice$date)

min_date <- min(historical_pc$date)
max_date <- max(historical_pc$date)

historical_pc_plot <- historical_pc %>%
  pivot_longer(!date, names_to = "PC", values_to = "value")

# plot historical values of PCs
historical_pc_plot %>%
  ggplot(aes(x = date, y = value, color = PC)) +
  geom_line() +
  labs(title = paste("Historical PCs from", min_date, "to", max_date))

# plotting them scale free as subplots
historical_pc_plot %>%
  ggplot(aes(x = date, y = value, group = PC)) +
  facet_wrap(~PC, scale = "free") +
  geom_line() +
  labs(title = paste("Historical PCs from", min_date, "to", max_date))

# get explained variance
explained_variance <- summary(pca_model)$importance %>%
  as.data.frame() %>%
  select(PC1, PC2, PC3)

explained_variance %>% 
  mutate(n = 1:n()) %>% 
  pivot_longer(!n, names_to = "name", values_to = "value") %>% 
  filter(n %in% c(2, 3)) %>% 
  mutate(variance = if_else(n == 2, "Explained Variance", "Cumulative Variance")) %>% 
  select(-n) %>% 
  ggplot(aes(x = name, y = value, group = variance)) +
  geom_bar(stat = "identity") +
  facet_wrap(~variance, scale = "free") +
  labs(title = "Principal Componenet Analysis Explained Variances") +
  ylab("Explained Variance") +
  xlab("")

explained_var_scale <- explained_variance %>% 
  mutate(n = 1:n()) %>% 
  pivot_longer(!n, names_to = "pc", values_to = "var_scale") %>% 
  filter(n == 2) %>% 
  select(-n)

scaled_pcs <- historical_pc %>% 
  pivot_longer(!date, names_to =  "pc", values_to = "unscaled") %>% 
  inner_join(explained_var_scale, by = "pc") %>% 
  mutate(scaled = unscaled * var_scale)

start_date <- min(scaled_pcs$date)
end_date <- max(scaled_pcs$date)

scaled_pcs %>% 
  select("Date" = date, "PC" = pc, "Unscaled" = unscaled, "Scaled" = "scaled") %>%
  pivot_longer(!c(Date, PC)) %>% 
  group_by(PC, name) %>% 
  arrange(Date) %>%
  ungroup() %>%  
  ggplot(aes(x = Date, y = value, col = PC)) +
  facet_wrap(~name, scale = "free") +
  geom_line() +
  ylab("Historical PC") +
  labs(title = paste("Historical PCs from", start_date, "to", end_date, "Scaled by Explained Variance"))

scaled_pcs %>% 
  select("Date" = date, "PC" = pc, "Unscaled" = unscaled, "Scaled" = "scaled") %>%
  pivot_longer(!c(Date, PC)) %>% 
  group_by(PC, name) %>% 
  arrange(Date) %>%
  ungroup() %>% 
  ggplot(aes(x = Date, y = value, col = name)) +
  facet_wrap(~PC, scale = "free") +
  geom_line() +
  ylab("Historical PC") +
  labs(title = paste("Historical PCs from", start_date, "to", end_date, "Scaled by Explained Variance"))

scaled_pcs %>% 
  write_parquet("yc_pcs.parquet")
