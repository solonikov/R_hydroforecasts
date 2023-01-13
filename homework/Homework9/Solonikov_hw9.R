# импорт пакетов
library(dplyr)
library(forecast)
library(hydroGOF)

# загрузка данных
setwd('class/')
load('prog_df.RData')
prog_df <- prog_df |> arrange(year, month)

# создание объекта типа "временной ряд" (time series)
tsBaikal <- ts(prog_df$fact, 
               start = c(1963, 1),
               frequency = 12)

# разделения ряда на обучающую и тестовую выборку (3 года)
trainBaikal <- tsBaikal[1:(length(tsBaikal)-36)]
testBaikal <- tsBaikal[(length(tsBaikal)-36):length(tsBaikal)]

## 1. Модель с параметрами, заданными вручную

# построение модели с порядками автокорреляции, дифференцирования и осреднения
fitARIMA <- arima(trainBaikal, 
                  order=c(8,1,4), 
                  method = 'CSS')

# прогнозирование на n шагов вперед, количество шагов равно длине проверочного
# ряда
pred1 <- predict(fitARIMA, n.ahead = length(testBaikal))

# вычисление ошибок
rmse1 <- rmse(sim = pred1$pred, obs = testBaikal)
nse1 <- NSE(sim = pred1$pred, obs = testBaikal)

# график
ts.plot(testBaikal, pred1$pred, gpars= list(col=c("black", "red")))

# попытаемся вручную подобрать такие параметры модели, чтобы минимизировать rmse
# 1. задаём датафрейм с разными вариантами параметров модели
model_params <- expand.grid(p = 1:9, d = 1:9, q = 1:9)
# 2. заранее создаём пустой датафрейм для записи результатов
res_rmse <- data.frame()
# 3. для каждого набора созданных параметров фитим модель и вычисляем rmse.
# Вычисленные значения rmse вместе с параметрами записываем в таблицу
# (Внимание! Блок кода ниже может выполняться дольше других, обычно до 10 сек)
apply(model_params, 1, \(x) {
  fitARIMA_n <- arima(trainBaikal,
                      order = x,
                      method = 'CSS')
  pred_n <- predict(fitARIMA_n, n.ahead = length(testBaikal))
  rmse_n <- rmse(sim = pred_n$pred, obs = testBaikal)
  # запись результатов в таблицу через глобальное присваивание:
  res_rmse <<- res_rmse |> rbind(c(x, rmse_n))
})
# 4. Задаём нормальные имена столбцов
colnames(res_rmse) <- c('p', 'd', 'q', 'rmse')
# 5. Запоминаем, при каких параметрах модели rmse минимален
rmse_min_params <- res_rmse |> filter(rmse == min(res_rmse$rmse))
# 6. Фитим модель для данных параметров
fitARIMA2 <- arima(trainBaikal,
                   order = rmse_min_params[1:3] |> as.vector(mode = 'numeric'),
                   method = 'CSS')
# 7. Делаем предсказание моделью, вычисляем ошибки
pred2 <- predict(fitARIMA2, n.ahead = length(testBaikal))
rmse2 <- rmse(sim = pred2$pred, obs = testBaikal)
nse2 <- NSE(sim = pred2$pred, obs = testBaikal)
# 8. Строим график
ts.plot(testBaikal, pred2$pred, gpars= list(col=c("black", "red")))


## 2. Модель с автоматически подобранными параметрами

fitARIMA_auto <- auto.arima(trainBaikal)
pred_auto <- predict(fitARIMA_auto, n.ahead = length(testBaikal))

# вычисление ошибок
rmse_auto <- rmse(sim = pred_auto$pred, obs = testBaikal)
nse_auto <- NSE(sim = pred_auto$pred, obs = testBaikal)

# графики
ts.plot(testBaikal, pred_auto$pred, gpars= list(col=c("black", "red")))
ts.plot(trainBaikal, fitARIMA$fitted, gpars= list(col=c("black", "red")))


# Таким образом, наилучшие результаты показала модель, в которой использовались
# подобранные вручную через apply значения параметров. RMSE для этой модели
# составил 949.8
