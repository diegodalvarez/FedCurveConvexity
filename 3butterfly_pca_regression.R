require("arrow")
require("reshape2")
require("tidyverse")

df_pca <- read_parquet("yc_pcs.parquet")

pca3 <- df_pca %>% 
  filter(pc == "PC3") %>% 
  select(-c(var_scale, pc))

# general plotting
start_date <- min(pca3$date)
end_date <- max(pca3$date)

pca3 %>% 
  ggplot(aes(x = date, y = unscaled)) +
  geom_line() +
  labs(title = paste("Third Principal Component of the Yield Curve from", start_date, "to", end_date)) +
  ylab("PC")

# now break up 3rd PC by year and then re-index them then join each and run regression
# not we have to slice our data so that our first date is the first date of next year to get the index right

index_seperate_join <- function(df){
  
  start <- min(df$date)
  next_year <- as.numeric(format(start_date, "%Y")) + 1 
  
  pca_indexed <- df %>% 
    mutate(year = format(date, "%Y")) %>% 
    filter(year > next_year) %>% 
    group_by(year) %>% 
    arrange(date) %>% 
    mutate(index = 1:n()) %>% 
    ungroup() %>% 
    select(-scaled)
  
  # now separate out the current year and then rejoin it back to the rest
  current_indexed <- pca_indexed %>% 
    filter(year == max(year)) %>% 
    select("unscaled_current" = unscaled, "year_current" = year, index) 
  
  previous_indexed <- pca_indexed %>% 
    filter(year != max(year)) %>% 
    select("unscaled_previous" = unscaled, "year_previous" = year, index)
  
  combined_index <- current_indexed %>% 
    inner_join(previous_indexed, by = "index")
  
  return(combined_index)
}

# run all the regressions of year comparison
index_seperate_join(pca3) %>% 
  ggplot(aes(x = unscaled_previous, y = unscaled_current)) +
  facet_wrap(~year_previous, scale = "free") +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Unscaled Current year 3rd PC") +
  xlab("Unscaled Previous year 3rd PC") +
  labs(title = "PCA Regression comparison of this year vs. previous years")

pca_change <- pca3 %>% 
  mutate(unscaled = unscaled / lag(unscaled) - 1)

index_seperate_join(pca_change) %>% 
  ggplot(aes(x = unscaled_previous, y = unscaled_current)) +
  facet_wrap(~year_previous, scale = "free") +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Unscaled Current year 3rd PC") +
  xlab("Unscaled Previous year 3rd PC") +
  labs(title = "PCA change Regression comparison of this year vs. previous years")

