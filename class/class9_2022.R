Sys.setlocale("LC_ALL","Russian")
# модель авторегрессии 1 порядка
c <- 0 # свободный член
alpha <- 1 # коэффициент авторегрессии
b <- vector() # вектор для моделирования 
b[1] <- 0 # начальное значение 
# цикл для моделирования
for(i in seq(1000)){
    b[i+1] <- c + alpha * b[i] + rnorm(1, mean = 0, 
                                       sd = 1) 
  }
plot(b, type = 'l') # график полученного ряда

acf(b) # оценка автокорелляционной функции 
acf(b, plot = F) # без вывода графика
pacf(b) # оценка автокорреляции дифференцированного ряда x(t) - x(t-1) 
pacf(b, plot = F) # без вывода графика
acf(diff(b), plot = F) # то же самое
b[1:2]
diff(b)[1]

setwd("D:/YandexDisk/ИВПРАН/R forecasts/2022/rforecast-main/data/baikal/")
load('prog_df.RData')

# проверяем, если у нас данные наблюдений отсортированы неправильно по годам
# и месяцам, то сортируем правильно с помощью arrange по двум столбцам
library(dplyr)
prog_df <- prog_df %>%
  arrange(year, month)
# save(prog_df, file = 'prog_df.RData')

plot(prog_df$fact, type = 'l') 
acf(prog_df$fact)

# создаем объект типа "временной ряд" - time series object
tsBaikal <- ts(prog_df$fact, 
               start = c(1963, 1),
               frequency = 12)
tsBaikal
str(tsBaikal)
plot(tsBaikal) # график наблюдений  
plot(diff(tsBaikal)) # график дифференцированных наблюдений 1 порядка
acf(tsBaikal) # оценка акф 
acf(tsBaikal, lag.max = 12, plot = FALSE) 
acf(diff(tsBaikal))
acf(diff(tsBaikal), lag.max = 12, plot = FALSE)
# разделение ряда на аддитивные компоненты - сезонный, тренд и случайный
components.tsBaikal <- decompose(tsBaikal)
plot(components.tsBaikal)
deseas <- tsBaikal - components.tsBaikal$seasonal # вычитание сезонной составляющей 
plot(deseas) 
acf(deseas) # оценка акф 
acf(deseas)
acf(deseas, lag.max = 12, plot = FALSE)

# разделения ряда на обучение и проверку (3 года)
trainBaikal <- tsBaikal[1:(length(tsBaikal)-36)]
testBaikal <- tsBaikal[(length(tsBaikal)-36):length(tsBaikal)]
# построение модели с порядками автокорреляции, дифференцирования и осреднения
fitARIMA <- arima(trainBaikal, 
                  order=c(6,0,3), 
                  method = 'CSS')
fitARIMA
# прогнозирование на n шагов вперед, количество шагов равно длине проверочного ряда
pred1 <- predict(fitARIMA, n.ahead = length(testBaikal))
# график
ts.plot(testBaikal, 
        pred1$pred, 
        gpars= list(col=c("black", "red")))
lines(pred1$pred + pred1$se, col='blue')
lines(pred1$pred - pred1$se, col='blue')

# пакет для автоматизированного прогнозирования
# install.packages("forecast")
library(forecast)
# функция автоматизированного подбора параметров
fitARIMA1 <- auto.arima(trainBaikal)
summary(fitARIMA1)
pred2 <- predict(fitARIMA1, n.ahead = length(testBaikal))
# график
ts.plot(trainBaikal, fitARIMA1$fitted, gpars= list(col=c("black", "red")))
ts.plot(testBaikal, pred2$pred, gpars= list(col=c("black", "red")))
lines(pred2$pred + pred2$se)
lines(pred2$pred - pred2$se)
