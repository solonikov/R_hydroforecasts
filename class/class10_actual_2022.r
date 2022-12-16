library(prophet)
library(ggplot2)
library(lubridate)
library(dplyr)
setwd('D:/YandexDisk/ИВПРАН/R forecasts/2022/rforecast-main/data/')

load('baikal/prog_df.RData')

# делаем датафрейм со столбцами, как требует prophet - ds и y ----
pritok <- prog_df %>% 
  mutate(ds = ISOdate(year, month, 1)) %>%
  select(ds, fact) %>%
  arrange(ds) %>%
  rename(y = fact)

# разделяем выборку на обучение (все минус 2 года) и проверку (2 года)
pritok_train <- pritok %>%
  slice(1:(n() - 24))

pritok_test <- pritok %>%
  tail(24)

# строим модель, сразу с данными и параметрами
M0 <- prophet(pritok_train, weekly.seasonality = F, 
              daily.seasonality = F, 
              seasonality.mode = 'multiplicative')
M0
# делаем датафрейм для прогноза (датафрейм с датами обучения+проверки)
future_df <- make_future_dataframe(M0, periods = 24, 
                                   freq = 'month')
# прогнозируем по модели
forecast_M0 <- predict(M0, future_df)
# с помощью функции prophet::plot рисуем прогноз по модели
plot(M0, forecast_M0)
# рисуем компоненты модели - тренд и сезонную составляющую
prophet_plot_components(M0, forecast_M0)
# рисуем график с настройкой шкалы времени и наносим на него точки наблюдений
plot(M0, forecast_M0) +
  scale_x_datetime(limits = c(as.POSIXct('2006-01-01'), 
                              as.POSIXct('2016-01-01'))) + 
  geom_point(data = pritok_test, size = 3, 
             aes(x=ds, y=y), col='red')
# делаем новую переменную - приток в предыдущий месяц
pritok <- pritok %>%
  mutate(y1 = lag(y, 1)) %>%
  filter(!is.na(y1))

pritok_train <- pritok %>%
  slice(1:(n() - 24))

pritok_test <- pritok %>%
  mutate(ds = as.POSIXct(ds)) %>%
  tail(24)

# инициализируем экземпляр модели без данных
M1 <- prophet()
# добавляем в модель дополнительный предиктор - новую переменную
M1 <- add_regressor(M1, 'y1')
# обучаем модель на данных
M1 <- fit.prophet(M1, pritok_train)
# делаем новый датафрейм для прогноза
future_df1 <- make_future_dataframe(M1, 
                                    periods = 24, 'month')
# добавляем в него значения предиктора из основного датафрейма
future_df1$y1 <- pritok$y1
# делаем прогноз
forecast_M1 <- predict(M1, future_df1)
plot(M1, forecast_M1)
prophet_plot_components(M1, forecast_M1)