
# Data Preparation ----

grocery <- read_csv("./data/Ambato_Tungurahua_Grocery.csv")


clean_grocery <- grocery %>%
  mutate(date = as.Date(date)) # Mutate to be actual date-datatype

clean_imp_grocery <- clean_grocery %>%
  mutate(sales = ifelse(sales == 0, NA, sales)) %>% # Replace 0 with NA
  mutate(sales = ts_impute_vec(sales, period = 12)) # Imputate the Sales Value


# Show Data
plot_time_series(clean_imp_grocery, date,
  sales,
  .interactive = TRUE,
  .smooth = FALSE,
  .title = "Daily Grocery Sales"
)


# Group Data by Week/Month ----

weekly_grocery <- clean_imp_grocery %>%
  summarize_by_time(
    .by = "week", # Other values: hour, day, month, bimonth, quarter, season
    date,
    weekly_sales = sum(sales), # Other values: mean(), median(), min(), max(), var(),
    weekly_promo = sum(onpromotion)
  )