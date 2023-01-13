# импорт пакетов
library(dplyr)
library(sf)
library(leaflet)
library(rnaturalearth)
library(ggplot2)
library(tmaptools)

setwd('data/')

## ЗАДАНИЕ 1

# читаем данные по гидропостам
hp <- read.table(file = 'np_hydropost_utf8.csv', 
                 sep = ";", 
                 header = T, 
                 check.names = F, 
                 stringsAsFactors = F)


hp_sf <- st_as_sf(hp, coords = c('lon', 'lat'), crs = 4326)

# получаем полигон территории России
russ_pol <- ne_countries(country = 'Russia', returnclass = 'sf')

# палитра для раскраски гидропостов
mypal <- c(get_brewer_pal('Set1', n = 20, plot = F),
           get_brewer_pal('Set2', n = 4, plot = F))

# карту гидропостов раскрашиваем по принадлежности к разным управлениям
# гидрометслужбы
ggplot() +
  geom_sf(data = russ_pol) +
  geom_sf(data = hp_sf, aes(color = property)) +
  scale_color_manual(name = 'Управление гидрометслужбы',
                    values = mypal) +
  coord_sf(crs = 32645) +
  theme_bw()

## ЗАДАНИЕ 2

# с помощью expand.grid создаём датафрейм с точками по регулярной сетке
# широта/долгота на произвольную область
points_regular <- expand.grid(Y = c(45:50), X = c(50:55))

# на основе полученного датафрейма создаём пространственный объект
points_regular_sf <- st_as_sf(points_regular, coords = c('Y', 'X'), crs = 4326)

# экспортируем его в шейп-файл
st_write(obj = points_regular_sf,
         dsn = 'points_regular.shp',
         delete_layer = T,
         layer_options = 'ENCODING=UTF8')

# получаем контуры приграничных государств (в данном случае только Казахстан)
neighbors <- ne_countries(country = 'Kazakhstan', returnclass = 'sf')

# получаем ограничивающий прямоугольник по слою points_regular_sf, чтобы затем
# сузить охват карты (сразу строим его в нужной проекции)
mybox <- st_bbox(points_regular_sf) |> 
            st_as_sfc() |> 
            st_transform(crs = 32645) |> 
            st_bbox()

# строим карту
ggplot() +
  geom_sf(data = russ_pol, fill = '#FCCCB8') +
  geom_sf(data = neighbors, fill = '#D8F0EF') +
  geom_sf(data = points_regular_sf, color = 'gray40') +
  scale_x_continuous(limits = mybox[c(1, 3)], expand = c(.3, .3)) +
  scale_y_continuous(limits = mybox[c(2, 4)], expand = c(.3, .3)) +
  coord_sf(crs = 32645) +
  theme_bw()

## ЗАДАНИЕ 3

# загружаем данные из шейп-файла с полигонами бассейнов
setwd('ecomag_basins/')
ecomag_basins <- st_read('ecomag_basins.shp', quiet = T)

# выбираем оттуда бассейн Лены
lena_basin <- ecomag_basins |> filter(name_en == 'Lena')

# выбираем гидропосты, расположенные в пределах бассейнс Лены
hp_lena <- hp_sf[lena_basin,]

# наносим на карту Leaflet и сохраняем как отдельный файл
leaflet() |> 
  addTiles() |> 
  addPolygons(data = lena_basin) |> 
  addCircleMarkers(data = hp_lena,
                    radius = 2,
                    color = 'red', 
                    opacity = 1,
                    popup = ~name)

# сохраняем гидропосты в пределах бассейна Лены в отдельный файл
setwd('../')
st_write(obj = hp_lena,
         dsn = 'hydroposts_lena.shp',
         delete_layer = T,
         layer_options = 'ENCODING=UTF8')

