Sys.setlocale('LC_ALL', 'Russian')
library(ncdf4)
library(chron)
library(raster)
library(ncdf4.helpers)
library(leaflet)
library(ggplot2)
library(reshape2)

ncfile <- nc_open('data/netcdf/era5_1979_Jan-Dec.nc')
ncfile

ncatt_get(nc = ncfile, varid = 't2m')
ncvar_get(nc = ncfile, varid = 't2m')
ncatt_get(ncfile, 'time')

nc.get.variable.list(ncfile)
times <- nc.get.time.series(ncfile)
times
class(times)

nc.get.dim.axes(ncfile)

nc_close(ncfile)    # закрываем файл

# br <- brick('data/netcdf/era5_1979_Jan-Dec.nc')
# Наши данные организованы по сетке -- много разных переменных по одной и той
# же сетке. brick позволяет набить все эти данные в один "кирпич" -- по сути
# трёхмерный массив (функция из пакета raster). Двумерные таблицы (матрицы)
# организованы по слоям (layers), образующим третье измерение

# возьмём t2m:
br <- brick('data/netcdf/era5_1979_Jan-Dec.nc', var = 't2m')
crs(br)    # brick сразу определяет координатную систему - смотрим этой ф-цией

leaflet() |> 
  addTiles() |> 
  addRasterImage(br[[1]], opacity = 0.5)


# pal <- colorNumeric(palette = 'spectral', domain = values(br[[1]]))
# leaflet() |> 
#   addTiles() |> 
#   addRasterImage(br[[1]], colors = pal, opacity = 0.6) |> 
#   addLegend(pal = pal, values = values(br[[1]]),
#             title = paste0("Температура воздуха, K <br>", getZ(br[[1]])))


# netcdf в высоком разрешении можно скачивать с сайта ECMWF, но теперь только
# через vpn


# делаем из брика датафрейм:
df <- as.data.frame(br)
# У нас сейчас получилось, что столбцы -- это временные таймстемпы.
# Исправим это:
df <- data.frame(t(df))

# Добавим даты в датафрейм в удобном формате:
rownames(df) <- NULL
colnames(df) <- c(1:ncol(df))
df$date <- times
df <- melt(df, id.vars = 'date')
summary(df)
ggplot(df, aes(x = date, y = value, group = 1)) + geom_line()

