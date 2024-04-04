

#####*************************Time Series Models***************************#####
#
# This lab contains 2 major parts:
#   *A- Univariate: 
#       We learn how to model TS in a univariate manner.
#       learn how to make decision of best model Vs. aggregation --> depending on
#       the nature of the data.
#   
#   *B- Multivariate:
#       Not all models can deal with Mutlivariate Datasets
#       Add Promotion as new Variable and re-evaluate the model
#####**********************************************************************#####


# A- Univariate----

# 1 Bibliotheken----
# Date and time manipulation
library(lubridate)

# Tools for working with time series data
library(timetk)

# Data manipulation and visualization
library(tidyverse)

# Framework for time series forecasting
library(modeltime)

# Unified interface to many models
library(parsnip)

# High-quality vector graphics device
library(Cairo)

# Simplifying complex code
library(magrittr)

# Create animations with ggplot2
library(gganimate)

# Graphics devices toolkit
library(gdtools)

# Benchmark datasets for machine learning
library(mlbench)

# Tools for modeling and machine learning
library(tidymodels)

# Data visualization and manipulation (again)
library(tidyverse)

# Kernel-based machine learning algorithms
library(kernlab)

# Unified interface for modeling and machine learning
library(workflows)

# Hyperparameter tuning
library(tune)

# Visualization of correlation matrices
library(corrplot)

# 2 Datensatz----
Party_df <- read_csv("TS_BSP_PARTY_store9/Party.csv")

# * TS-Vizualisation----
Party_df %>%
  mutate(date=as.Date(date)) %>% 
  plot_time_series(date, sales,
                   .interactive=TRUE,
                   .title="Store_transactions",
                   .smooth = TRUE)

# 2 data cleaning, imputation and conversion to ts format----
Party_tbl<- Party_df %>%
  #  remove 2013- Jan_2014
  mutate(sales=ifelse(sales==0,NA,sales))%>%
  mutate(sales=ts_impute_vec(sales,period = 12))

plot_time_series(Party_tbl, date,sales,
                 .interactive = TRUE,
                 .title = " Celebration articles with imputation")



# Löschen des Zeitraums Jan 13 bis Jan 14
clean_party <- Party_tbl[Party_tbl$date >= "2014-01-01", ]
Party_tbl_clean<- clean_party %>%
  #  remove 2013- Jan_2014
  mutate(sales=ifelse(sales==0,NA,sales))%>%
  mutate(sales=ts_impute_vec(sales,period = 12))

plot_time_series(Party_tbl_clean, date,sales,
                 .interactive = TRUE,
                 .title = " Celebration articles with imputation Starting Januar 2014")

#  3 Modeling----
#  * split data for ML

splits<- time_series_split( Party_tbl, assess = "1 year", cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%   # fct top interface TS
  group_by(date) %>%
  plot_time_series_cv_plan(date, sales,
                           .interactive = TRUE,
                           .title = "Celebration articles")

# * ARIMA

arima_model<- arima_reg()%>%
  set_engine("auto_arima")%>%
  fit(
    sales~date,
    data= training(splits)
  )

# * Linear Regression

LM_model<- linear_reg()%>%
  set_engine("lm")%>%
  fit(
    sales ~ as.numeric(date) + month(date, label = TRUE), #as numeric for trends
    data=training(splits)
  )

# * Linear Regression without trend
Lm_model_no_trends<- linear_reg()%>%
  set_engine("lm")%>%
  fit(
    sales~ month(date, label = TRUE),
    data=training(splits)
  )


# * Prophet
Prophet_model<- prophet_reg() %>%
  set_engine("prophet")%>%
  fit(
    sales~ date,
    data=training(splits)
  )

# * Random Forrest

RF_model<- rand_forest(mode = "regression") %>%
  set_engine("randomForest") %>%
  fit(
    sales~ as.numeric(date)+ month(date, label = TRUE),
    data=training(splits)
  )

# * XGBoost

XgBoost_model<-boost_tree(mode = "regression") %>%
  set_engine("xgboost") %>%
  fit(
    sales~ as.numeric(date)+ month(date, label = TRUE),
    data=training(splits)
  )


# * Support vector machine SVM_ polynomial

SVM_poly_model<- svm_poly(mode = "regression")%>%
  set_engine("kernlab")%>%
  fit(
    sales~ as.numeric(date)+ month(date, label = TRUE),
    data= training(splits)
  )


# SVM RBF - fitting model via spark

SVM_rbf_model<- svm_rbf(mode = "regression")%>%
  set_engine("kernlab")%>%
  fit(
    sales~ as.numeric(date)+ month(date, label = TRUE),
    data= training(splits)
  )


# * Prophet Boost

Prophet_boost_model<- prophet_boost() %>%
  set_engine("prophet_xgboost") %>%
  fit(
    sales ~ date + as.numeric(date) + month(date, label = TRUE), 
    data = training(splits) 
  )

# * Arima Boost

Arima_xgboost_model<- arima_boost()%>%
  set_engine("auto_arima_xgboost")%>%
  fit(
    sales~date + as.numeric(date)+ month(date, label = TRUE),
    data=training(splits)
  )



# 4 Modeltime Forecast Workflow----
model_tbl<- modeltime_table(
  arima_model,
  LM_model,
  Lm_model_no_trends,
  Prophet_model,
  RF_model,
  XgBoost_model,
  SVM_poly_model,
  SVM_rbf_model,
  Prophet_boost_model,
  Arima_xgboost_model
)


# * Calibration----
# run all models on the testing data 

calibration_tbl<- model_tbl %>%
  modeltime_calibrate(testing(splits))

#plot in an interactive format
calibration_tbl %>%
  modeltime_accuracy()%>%
  table_modeltime_accuracy(resizable=TRUE, bordered=TRUE)


# * Forecast and plot the results----
calibration_tbl%>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = Party_tbl,
    conf_interval = 0.95
  ) %>%
  plot_modeltime_forecast(.legend_show = TRUE,
                          .legend_max_width = 25)

# * Refit ----
# takes a Model time table and run the alg on a new data and fits parms

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = Party_tbl) 

forecast_tbl <- refit_tbl %>%
  modeltime_forecast(
    h = "1 year",
    actual_data = Party_tbl,
    conf_interval = 0.95
  ) 

forecast_tbl %>%
  plot_modeltime_forecast(.interactive = TRUE)


# 5 Model Averaging----
mean_forcast_tbl<- forecast_tbl %>%
  filter(.key !="actual")%>%
  group_by(.key,.index) %>% #group all predictions for one date 
  summarise(across(.value:.conf_hi,mean))%>% # summarize the value and conf and compute mean across all models
  mutate(
    .model_id=10,     # numb of models
    .model_desc="Average of Models"
  )

# * Visualization

forecast_tbl %>%
  filter(.key=="actual")%>%
  bind_rows(mean_forcast_tbl)%>%
  plot_modeltime_forecast()


# 5 Selecting Best ones -TRy----

#  Modeltime Forecast Workflow----
model_tbl<- modeltime_table(
  Prophet_model,
  Prophet_boost_model
)

# redo previous steps----
# * Calibration----
# run all models on the testing data 
calibration_tbl<- model_tbl %>%
  modeltime_calibrate(testing(splits))

#plot in an interactive format
calibration_tbl %>%
  modeltime_accuracy()%>%
  table_modeltime_accuracy(resizable=TRUE, bordered=TRUE)


# * Forecast and plot the results----
calibration_tbl%>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = Party_tbl,
    conf_interval = 0.9
  ) %>%
  plot_modeltime_forecast(.legend_show = TRUE,
                          .legend_max_width = 25)

# * Refit ----
# takes a Model time table and run the alg on a new data and fits parms

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = Party_tbl) 

forecast_tbl <- refit_tbl %>%
  modeltime_forecast(
    h = "1 year",
    actual_data = Party_tbl,
    conf_interval = 0.80
  ) 

forecast_tbl %>%
  plot_modeltime_forecast(.interactive = TRUE)


# 5 Model Averaging----
mean_forcast_tbl<- forecast_tbl %>%
  filter(.key !="actual")%>%
  group_by(.key,.index) %>% #group all predictions for one date 
  summarise(across(.value:.conf_hi,mean))%>% # summarize the value and conf and compute mean across all models
  mutate(
    .model_id=2,     # numb of models
    .model_desc="Average of Models"
  )

# * Visualization

forecast_tbl %>%
  filter(.key=="actual")%>%
  bind_rows(mean_forcast_tbl)%>%
  plot_modeltime_forecast()

forecast_tbl$.value


############################ Amelioration_step #######################################
# B Multivariate with Promotion----

# Retrain_models----
# Not all Models support Multivariate

# * Prophet
Prophet_model<- prophet_reg() %>%
  set_engine("prophet")%>%
  fit(
    sales~ date+onpromotion,
    data=training(splits)
  )

# * Prophet Boost
Prophet_boost_model<- prophet_boost() %>%
  set_engine("prophet_xgboost") %>%
  fit(
    sales ~ date + as.numeric(date) + month(date, label = TRUE)+ onpromotion, 
    data = training(splits) 
  )


# * LM
LM_model<- linear_reg()%>%
  set_engine("lm")%>%
  fit(
    sales ~ as.numeric(date) + month(date, label = TRUE)+ onpromotion, #as n umeric for trends
    data=training(splits)
  )



model_tbl<- modeltime_table(
  Prophet_model,
  Prophet_boost_model,
  LM_model
  
)



# run all models on the testing data 
calibration_tbl<- model_tbl %>%
  modeltime_calibrate(testing(splits))

#plot in an interactive format
calibration_tbl %>%
  modeltime_accuracy()%>%
  table_modeltime_accuracy(resizable=TRUE, bordered=TRUE)


# * Forecast and plot the results----
calibration_tbl%>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = Party_tbl,
    conf_interval = 0.95
  ) %>%
  plot_modeltime_forecast(.legend_show = TRUE,
                          .legend_max_width = 25)

# * Refit ----
# takes a Model time table and run the alg on a new data and fits parms
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = Party_tbl) 

#test <- read_csv("Praxis/store-sales-time-series-forecasting/test.csv")


forecast_tbl <- refit_tbl %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = Party_tbl,
    conf_interval = 0.95
  ) 

forecast_tbl %>%
  plot_modeltime_forecast(.interactive = TRUE)

# --> Best model is Prophet XGboost

# 5 Model Averaging----
# --> Not always a good solution just give a try
mean_forcast_tbl<- forecast_tbl %>%
  filter(.key !="actual")%>%
  group_by(.key,.index) %>% #group all predictions for one date 
  summarise(across(.value:.conf_hi,mean))%>% # summarize the value and conf and compute mean across all models
  mutate(
    .model_id=3,     # numb of models
    .model_desc="Average of Models"
  )

# * Visualization 

forecast_tbl %>%
  filter(.key=="actual")%>%
  bind_rows(mean_forcast_tbl)%>%
  plot_modeltime_forecast()

forecast_tbl$.value
