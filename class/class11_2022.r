Sys.setlocale("LC_ALL","Russian")
library(reshape2)
library(ggplot2)
setwd('d:/YandexDisk/ИВПРАН/R forecasts/netcdf')
library(ncdf4)
# открываем соединение с файлом netcdf
ncfile <- nc_open('era5_1979_Jan-Dec.nc')
# просмотр заголовка файла - сведения о измерениях и переменных
ncfile
# просмотр атрибутов переменной
ncatt_get(nc = ncfile, varid = 't2m')
# получение самого ряда
ncvar_get(nc = ncfile, varid = 't2m')
# просмотр атрибутов даты
ncatt_get(ncfile, 'time')
# получение ряда дат
ncvar_get(ncfile, 'time')

library(ncdf4.helpers)
# переменные
nc.get.variable.list(ncfile)
# время в почти правильном формате
times <- nc.get.time.series(ncfile)
times
class(times)
library(PCICt)
# время в совсем правильном формате
times <- as.POSIXct.PCICt(times)
class(times)
# соединение с файлом следует закрыть (через какое-то время закрывается автоматически)
nc_close(ncfile)


library(raster)
# загрузка netcdf как трехмерного объекта 
# NB: без указания переменной устанавливается по умолчанию первая из списка
br <- brick('era5_1979_Jan-Dec.nc')
# для выбора возвращаемой переменной следует указать ее название
br <- brick('era5_1979_Jan-Dec.nc', var ='tp')
# просмотр атрибутов, в т.ч. проекции сетки
br
library(leaflet)
# первый слой трехмерного объекта (первая дата)
leaflet() %>%
  addTiles() %>%
  addRasterImage(br[[1]], opacity = 0.5)
# палитра для легенды, диапазон по значениям переменной 
pal <- colorNumeric(palette = 'Spectral', domain = values(br[[1]]),  na.color = "transparent")
# карта с легендой
leaflet()%>%
  addTiles() %>%
  addRasterImage(br[[1]], colors = pal, opacity = 0.6) %>%
  addLegend(pal = pal, values = values(br[[1]]),
            title = paste0("Температура воздуха, °K <br>", 
                           getZ(br[[1]])))
# из трехмерного объекта в датафрейм
df <- raster::as.data.frame(br)
colnames(df)
# транспонирование
df <- data.frame(t(df))
rownames(df) <- NULL  
colnames(df) <- c(1:ncol(df))
# столбец с датами
df$date <- times
# 
df <- melt(df, id.vars = 'date')
summary(df)

df %>%
  group_by(date = date(date), variable) %>%
  summarise(value = sum(value * 1000, na.rm = T)) %>%
  group_by(date) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(tm = sum(value, na.rm = T)) %>%
  group_by(year) %>%
  summarise(sum = sum(tm))
  ggplot(aes(x=factor(month), y=tm, fill = tm)) + geom_col()
