source("TimeSeries/Libraries.r")

# Read, Clean, Aggregate Data
source("TimeSeries/Prepare.r")

# Add Weather Data ----

# Lookup Station name at https://mesonet.agron.iastate.edu/request/download.phtml
# ! Funktioniert nicht gescheit :
# ! riem::riem_measures("SELT", date_start = "2012-01-01", date_end = "2018-01-01")

latacunga_weather <- read_csv("./data/SELT2.csv") %>%
  mutate(valid = as.Date(substr(valid, 1, 10))) %>% # Substring -> Take only the date, cutoff time
  rename(date = valid) %>%
  mutate(tempc = (feel - 32) * (5 / 9)) %>% # Convert fahrenheit to celcius
  group_by(date) %>% # Group by day and get avg. day temperature.
  summarise(
    avg_temp = mean(tempc, na.rm = TRUE)
  ) %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  mutate(avg_temp = na.approx(avg_temp))

latacunga_weather_wkly <- latacunga_weather %>%
  summarize_by_time(
    .by = "week",
    date,
    avg_temp_wkly = mean(avg_temp, na.rm = TRUE)
  )


# Add Holiday Data ----
# Found this myself: https://date.nager.at/ allows csv download

ecuador_holidays <- read_csv("./data/publicholiday.csv") %>%
  select(c(Date)) %>%
  rename(date = Date) %>%
  mutate(holiday_flag = ifelse(is.na(type), 0, 1))

ecuador_holidays_wkly <- ecuador_holidays %>%
  summarize_by_time(
    .by = "week",
    date,
    sum_holidays = sum(holiday_flag, na.rm = TRUE)
  )



# Enrich/Combine Dataset ----

weekly_grocery_rich <- weekly_grocery %>%
  left_join(ecuador_holidays_wkly, by = "date") %>%
  left_join(latacunga_weather_wkly, by = "date") %>%
  mutate(sum_holidays = ifelse(is.na(sum_holidays), 0, sum_holidays))


# Create Training- / Test Split ----

splits <- time_series_split(weekly_grocery_rich, assess = "1 year", cumulative = TRUE)

# Visualize Data Split
splits %>%
  tk_time_series_cv_plan() %>% # fct top interface TS
  group_by(date) %>%
  plot_time_series_cv_plan(date, weekly_sales,
    .interactive = TRUE,
    .title = "Daily Grocery Sales"
  )

# Models ----

#* Linear Model : Lineare Regression
linear_model <- linear_reg() %>%
  set_engine("lm") %>% # Other Engines: lm, glm, glmnet, stan, spark, keras, brulee
  fit(
    weekly_sales ~ date + as.numeric(date) + month(date, label = TRUE) + weekly_promo + sum_holidays + avg_temp_wkly, # as numeric for trends
    data = training(splits)
  )

#* Arima :
arima_model <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(
    # ! Important for model: always provide date as predictor !
    weekly_sales ~ date + as.numeric(date) + month(date, label = TRUE) + weekly_promo + sum_holidays + avg_temp_wkly,
    data = training(splits)
  )

#* Prophet model
prophet_model <- prophet_reg() %>%
  set_engine("prophet") %>%
  fit(
    # ! Important for model: always provide date as predictor !
    weekly_sales ~ date + as.numeric(date) + month(date, label = TRUE) + weekly_promo + sum_holidays + avg_temp_wkly,
    data = training(splits)
  )

#* Prophet XG-Boost Model
prophet_boost_model <- prophet_boost() %>%
  set_engine("prophet_xgboost") %>%
  fit(
    # ! Important for model: always provide date as predictor !
    weekly_sales ~ date + as.numeric(date) + month(date, label = TRUE) + weekly_promo + sum_holidays + avg_temp_wkly,
    data = training(splits)
  )

# Put models into one "dataframe"
model_tbl <- modeltime_table(
  linear_model,
  arima_model,
  prophet_model,
  prophet_boost_model
)

# Calibrate Models ----
calibration_tbl <- model_tbl %>%
  modeltime_calibrate(testing(splits))

# Show Accuracy of Models in Table
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(resizable = TRUE, bordered = TRUE)


# Forecast Values and plot in Graph
calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = weekly_grocery_rich,
    conf_interval = 0.5
  ) %>%
  plot_modeltime_forecast(
    .legend_show = TRUE,
    .legend_max_width = 25
  )


# Refit ----
# takes a Model time table and run the alg on a new data and fits parms

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = weekly_grocery_rich)

forecast_tbl <- refit_tbl %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = weekly_grocery_rich,
    conf_interval = 0.5
  )

forecast_tbl %>%
  plot_modeltime_forecast(.interactive = TRUE)

# --> Best model is Prophet XGboost


# Averaging ----
# ! Not always a good solution
mean_forcast_tbl <- forecast_tbl %>% # Averages the Forecast
  filter(.key != "actual") %>%
  group_by(.key, .index) %>% # group all predictions for one date
  summarise(across(.value:.conf_hi, mean)) %>% # summarize the value and conf and compute mean across all models
  mutate(
    .model_id = 3, # numb of models
    .model_desc = "Average of Models"
  )

forecast_tbl %>%
  filter(.key == "actual") %>%
  bind_rows(mean_forcast_tbl) %>%
  plot_modeltime_forecast()

# holidays <- holidayNYSE(year = getRmetricsOptions("currentYear"),
#             type = c("", "standard", "special")) %>% ymd()



# https://mesonet.agron.iastate.edu/request/download.phtml?network=DE__ASOS
