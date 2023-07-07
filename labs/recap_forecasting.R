# Packages used in this script
library(tidyverse)
library(fpp3)
library(lubridate)

# read temporal data

ambulance_demand <- read_rds("data/ambulance_demand.rds")

# 1)check NAs and fix them, 2) check duplicated rows and fix them

# Create a tsibble

ambulance_demand_tsb <- ambulance_demand |> as_tsibble(index = date, key = lhb)

# check for temporal gaps



# time series graphics

# split data into train and test
f_horizon <- 42# forecast horizon is 42 days
percentage_test <- 0.2 #20% of time series for test set

test <- ambulance_demand_tsb |> 
  filter_index(as.character(max(ambulance_demand_tsb$date)-round(percentage_test*length(unique(ambulance_demand_tsb$date)))+1) ~ .)

train <- ambulance_demand_tsb |>
  filter_index(. ~ as.character(max(ambulance_demand_tsb$date)-(round(percentage_test*length(unique(ambulance_demand_tsb$date))))))

train_tscv <- ambulance_demand_tsb |> 
  filter_index(. ~ as.character(max(ambulance_demand_tsb$date)-(f_horizon))) |>
  stretch_tsibble(.init = length(unique(train$date)), .step = 7) # split data into different time series (i.e. origin or id) with increasing size

# Train models
model_tscv <- train_tscv |>
  model(
    mean = MEAN(demand),#total average
    naive = NAIVE(demand),#naive
    snaive = SNAIVE(demand),#seasonal naive
    exponetial_smoothing = ETS(demand),
    arima = ARIMA(demand),
    regression = TSLM(demand)
  ) |> mutate(ets_arima_combined = (exponetial_smoothing+arima)/2)

# Produce forecasts
fcst_tscv <- model_tscv |> forecast(h = f_horizon)

# Evaluate forecast accuracy
fc_accuracy_all <- fcst_tscv |> accuracy(ambulance_demand_tsb, 
                                         measures = list(point_accuracy_measures, interval_accuracy_measures, distribution_accuracy_measures))

fc_accuracy_all |> group_by(.model) |> summarise(RMSE=mean(RMSE))


# optional
fc_accuracy_by_id <- fcst_tscv |>
  accuracy(ambulance_demand_tsb, 
           measures = list(point_accuracy_measures, interval_accuracy_measures, distribution_accuracy_measures),
           by = c(".model",".id"))

fc_h <- fcst_tscv |> 
  group_by(.id,.model) |> 
  mutate(h=row_number()) |> ungroup() |> 
  as_fable(response = "demand", distribution = "demand")

fc_accuracy_h <- fc_h |> 
  accuracy(ambulance_demand_tsb,
           measures = list(point_accuracy_measures, interval_accuracy_measures, distribution_accuracy_measures),
           by = c(".model","h"))

## 

forecast_ambulance_demand_future <- ambulance_demand_tsb |> 
  model(mean = ETS(demand)) |> forecast(h=f_horizon)

forecast_ambulance_demand_future |> 
  autoplot(filter_index(ambulance_demand_tsb,"2019" ~ .), level = NULL)

fct_extract <- forecast_ambulance_demand_future |> hilo(level = 95) |> unpack_hilo(`95%`)
write_csv(fct_extract, "result/fct_extract.csv")
