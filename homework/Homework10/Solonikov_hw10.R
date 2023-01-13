# импорт пакетов
library(prophet)
library(ggplot2)
library(lubridate)
library(dplyr)
library(hydroGOF)

# считываем данные из файла
setwd('data/baikal/')
load('prog_df.RData')

# делаем датафрейм со столбцами, как требует prophet - ds и y
pritok <- prog_df |> 
  mutate(ds = ISOdate(year, month, 1)) |> 
  select(ds, fact) |> 
  arrange(ds) |> 
  rename(y = fact)

# создаём в датафрейме новую переменную - приток за 6 месяцев до даты выпуска
# прогноза
pritok <- pritok |> 
  mutate(y6 = lag(y, 6)) |> 
  filter(!is.na(y6))

# разделяем выборку на обучение (все минус 2 года) и проверку (2 года)
pritok_train <- pritok |> slice(1:(n() - 24))
pritok_test <- pritok |> tail(24)

# инициализируем модель без данных
mymodel <- prophet()

# добавляем новую переменную y6 в качестве дополнительного предиктора
mymodel <- add_regressor(mymodel, 'y6')

# обучаем модель на данных
mymodel <- fit.prophet(mymodel, pritok_train)

# создаём датафрейм для прогноза
future_df <- make_future_dataframe(mymodel, periods = 24, 'month')

# добавляем туда значения из основного датафрейма
future_df$y6 <- pritok$y6

# делаем прогноз и строим графики
forecast_mymodel <- predict(mymodel, future_df)
plot(mymodel, forecast_mymodel)
prophet_plot_components(mymodel, forecast_mymodel)

# оцениваем rmse и nse для обучающего и проверочного ряда
rmse_train <- rmse(sim = forecast_mymodel$yhat[1:594], obs = pritok_train$y)
rmse_test <- rmse(sim = forecast_mymodel$yhat[595:618], obs = pritok_test$y)
nse_train <- NSE(sim = forecast_mymodel$yhat[1:594], obs = pritok_train$y)
nse_test <- NSE(sim = forecast_mymodel$yhat[595:618], obs = pritok_test$y)

