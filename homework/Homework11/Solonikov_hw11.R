# импорт пакетов
library(ncdf4)
library(chron)
library(ncdf4.helpers)
library(reshape2)
library(PCICt)
library(raster)
library(lubridate)
library(dplyr)
library(ggplot2)

# открываем соединение с файлом netcdf
setwd('data/netcdf/')
ncfile <- nc_open('era5_1979_Jan-Dec.nc')

# считываем данные о времени и преобразуем в правильный формат
times <- nc.get.time.series(ncfile) |> as.POSIXct.PCICt()

# закрываем соединение с файлом
nc_close(ncfile)

# загружаем netcdf как трёхмерный объект типа brick, сразу выбираем переменную
br <- brick('era5_1979_Jan-Dec.nc', var = 'tp')

# преобразуем трёхмерный объект brick в датафрейм
df <- raster::as.data.frame(br)

# транспонирование
df <- data.frame(t(df))
rownames(df) <- NULL  
colnames(df) <- c(1:ncol(df))
# столбец с датами
df$date <- times

df <- melt(df, id.vars = 'date')

# рассчитываем осадки за каждый месяц
df_prec <- df |>
  group_by(month = month(date), variable) |> 
  summarize(prec_sum = sum(value*1000, na.rm = T)) |> 
  group_by(month) |> 
  summarize(prec = mean(prec_sum, na.rm = T))

# строим график внутригодового хода сумм осадков по месяцам
df_prec |> 
  ggplot(aes(x = factor(month), y = prec)) + 
  geom_col(fill = '#1A69AF') +
  scale_x_discrete(labels = month.abb) +
  labs(x = 'month', y = 'precipitation, mm') +
  theme_bw()

# вычисление годовой суммы
sum_prec <- sum(df_prec$prec)
sum_prec

