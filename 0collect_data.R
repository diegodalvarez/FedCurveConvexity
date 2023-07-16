require("fredr")
require("arrow")
require("tidyverse")

api_key <- "744a26556c8a3c15d4e076ac6515af12"
fredr_set_key(api_key)

tickers <- c("DGS1", "DGS10", "DGS2", "DGS5", "DGS30", "DGS1MO", "DGS3MO", "DGS3", 
  "DGS6MO", "DGS7")
start_date = as.Date("1900-01-01")
end_date = Sys.Date()

df_out <- tibble()

for (ticker in tickers){

  fred_tmp  <- fredr(
    series_id = ticker,
    observation_start = start_date,
    observation_end = end_date)

  df_out <- bind_rows(df_out, fred_tmp)
}

fed_funds_df <- fredr(
  series_id = c("FEDFUNDS"),
  observation_start = start_date,
  observation_end = end_date)

write_parquet(df_out, "yield_curve.parquet")
write_parquet(fed_funds_df, "fedfunds.parquet")


