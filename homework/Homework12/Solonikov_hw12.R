# импорт пакетов
library(sf)
library(lubridate)
library(dplyr)
library(leaflet)
library(tidyverse)
library(reshape2)
library(readxl)
library(writexl)
library(airGRteaching)

# загрузка данных
load('data/rivers/river_data.RData')
load('data/rivers/weather_data.RData')

# выбор гидропоста и метеостанции (по датафрейму intersect из class7)
my_hp <- 82060    # КАЗАЧИЙ БРОД - Р.МЗЫМТА
my_ws <- 37099    # СОЧИ 

# выбираем данные по своему гидропосту и метеостанции
my_q <- river_data |> filter(index == my_hp)
my_w <- weather_data |> filter(index == my_ws)

# соединяем их в один датафрейм
df <- merge(my_q, my_w, by = 'date', suffixes = c('_hydro', '_meteo'))

# сохраняем исходные данные
save(df, file = 'data/rivers/my_river_data_Solonikov_hw12.RData')

# вычисляем потенциальное испарение и строим график
df$En <- PE_Oudin(JD = yday(df$date), 
                  Temp = df$mean_temp, 
                  Lat = 43.58, LatUnit = "deg")

df |> 
  select(!contains('index')) |> 
  pivot_longer(!date, names_to = 'variable', values_to = 'value') |> 
  ggplot(aes(x=date, y=value, col=variable)) + 
  geom_line() + 
  facet_wrap(variable~., ncol = 1, scales = 'free_y')

# пересчитываем расход в слой стока
df$q_mm <- df$q * 86400 / 839 / 1000

# выбираем переменные, назначаем удобные имена столбцов, делаем преобразования
# даты в POSIXct
df <- df |> 
  select(date, prec, mean_temp, En, q_mm) |> 
  magrittr::set_colnames(c('DatesR', 'Precip',
                           'TempMean', 'PotEvap', 'Qobs')) |>
  mutate(DatesR = as.character(DatesR)) |> 
  mutate(DatesR = as.POSIXct(strptime(DatesR, format = '%Y-%m-%d', tz = 'UTC')))

# подготовка инпута для модели
PREP <- PrepGR(DatesR = df$DatesR, Precip = df$Precip, 
               PotEvap = df$PotEvap, Qobs = df$Qobs, 
               TempMean = df$TempMean, 
               HydroModel = "GR4J", CemaNeige = TRUE)

# строим график
plot(PREP, main = "Фактические осадки и сток", 
     xlab = 'Дата', ylab = c('Осадки, мм', 'Расход воды, мм'), 
     plot.na = F)

# калибровка модели по критерию NSE
CAL <- CalGR(PrepGR = PREP, CalCrit = "NSE", 
             WupPer = c("2008-01-01", "2008-12-31"), 
             CalPer = c("2009-01-01", "2014-12-31"))

# строим графики
plot(CAL)
plot(CAL, which = "iter")

# запуск модели; в качестве первых двух параметров передаём заранее
# подготовленный инпут PREP и калибровочные параметры CAL
SIM <- SimGR(PrepGR = PREP, Param = CAL, EffCrit = "NSE",
             WupPer = c("2008-01-01", "2013-12-31"), 
             SimPer = c("2014-01-01", "2014-12-31"))

# строим графики
plot(SIM)
plot(SIM, which='ts')

# запуск в интерактивном режиме
ShinyGR(DatesR = df$DatesR, Precip = df$Precip, 
        PotEvap = df$PotEvap, 
        Qobs = df$Qobs, TempMean = df$TempMean, 
        SimPer = c("2008-01-01", "2014-12-31"), 
        NamesObsBV = 'Мзымта')

# далее необходимо нанести на карту гидропост и метеостанцию; для этого загрузим
# файлы, которые мы использовали в class7
hp <- read.table(file = 'data/np_hydropost_utf8.csv',  # гидропосты
                 sep = ";", 
                 header = T, 
                 check.names = F, 
                 stringsAsFactors = F)

syn_meta <- read.csv('data/np_meteost_utf8.csv')    # метеостанции

# ищем нужные пост и станцию по индексу и преобразуем в пространственные объекты
hp_sf <- hp |> 
  filter(index == my_hp) |> 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

syn_sf <- syn_meta |> 
  filter(index == my_ws) |> 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)

# наносим на карту
leaflet() |> 
  addTiles() |> 
  addCircleMarkers(data = hp_sf, radius = 5, color = 'blue', opacity = .7) |> 
  addCircleMarkers(data = syn_sf, radius = 5, color = 'red', opacity = .7) |> 
  addPopups(39.98, 43.5, 'КАЗАЧИЙ БРОД<br/>Р.МЗЫМТА') |> 
  addPopups(39.75986, 43.57769, 'метеостанция<br/>СОЧИ')

