require("roll")
require("arrow")
require("ggplot2")
require("tidyverse")

df_butterfly_raw <- read_parquet(
  "fly.parquet")

# calculate the rolling z-score 
df_fly_z_score <- df_butterfly_raw %>% 
  group_by(ticker) %>% 
  mutate(z_score = lag((value - roll_mean(value, width = 90)) / roll_mean(value, width = 90))) %>% 
  drop_na() %>% 
  ungroup()

max_date <- max(df_fly_z_score$date)

# examining the last z-score
df_fly_last <- df_fly_z_score %>% 
  filter(date == max_date) %>% 
  mutate(color = if_else(z_score < 0, "red", "darkgreen"))

x_axis_order <- df_fly_last %>% 
  arrange(z_score) %>% 
  select(ticker) %>% 
  pull()

average_z_score <- mean(df_fly_last$z_score)

df_fly_last %>% 
  ggplot(aes(x = factor(ticker, level = x_axis_order), y = z_score, fill = color)) +
  geom_col() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  xlab("Fly Tenors") +
  ylab("Z-Score") +
  labs(title = paste("Flys Z-Score as of", max_date)) +
  theme(legend.position = "none") +
  geom_hline(yintercept = average_z_score)

df_fly_last %>% 
  ggplot(aes(x = factor(ticker, level = x_axis_order), y = z_score, fill = color)) +
  geom_col() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  xlab("Fly Tenors") +
  ylab("Z-Score") +
  labs(title = paste("Flys Z-Score as of", max_date)) +
  geom_hline(yintercept = average_z_score) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank())

df_fly_last_z <- df_fly_last %>% 
  mutate(zz_score = abs(z_score - mean(z_score)) / sd(z_score)) %>% 
  filter(z_score < 2)

x_axis_order <- df_fly_last_z %>% 
  arrange(z_score) %>% 
  select(ticker) %>% 
  pull()

average_z_score <- mean(df_fly_last_z$z_score)

df_fly_last_z %>% 
  ggplot(aes(x = factor(ticker, level = x_axis_order), y = z_score, fill = color)) +
  geom_col() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  xlab("Fly Tenors") +
  ylab("Z-Score") +
  labs(title = paste("Flys Z-Score as of", max_date, "(outliers dropped)")) +
  geom_hline(yintercept = average_z_score) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank())

cr <- colorRamp(c("green", "black"))
 
df_fly_mean_mat <- df_fly_last_z %>% 
  group_by(ticker) %>% 
  mutate(mean_mat = mean(as.numeric(unlist(str_split(ticker, "_"))))) %>% 
  ungroup()

df_fly_mean_mat %>% 
  ggplot(aes(
    x = factor(ticker, level = x_axis_order), 
    y = z_score, 
    fill = rgb(cr(mean_mat / max(mean_mat))))) +
  geom_col() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  xlab("Fly Tenors") +
  ylab("Z-Score") +
  labs(title = paste("Flys Z-Score as of", max_date, "(outliers dropped)")) +
  geom_hline(yintercept = average_z_score) +
  theme(
    legend.position = "none",
    axis.text.x = element_blank())
