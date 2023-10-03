require("arrow")
require("tidyquant")
require("tidyverse")

tickers <- c(
  "DGS1", "DGS10", "DGS2", "DGS5", "DGS30", "DGS1MO", "DGS3MO", "DGS3", 
  "DGS6MO", "DGS7", "FEDFUNDS")

start_date = as.Date("1900-01-01")
end_date = Sys.Date()

df_tmp <- tq_get(
  x = tickers, get = "economic.data", from = start_date, to = end_date)

parent_path <- dirname(getwd())
data_out_path <- file.path(parent_path, "data", "data.parquet")

df_tmp %>% 
  write_parquet(data_out_path)
