Sys.setlocale("LC_ALL","ru_RU.UTF-8")
# install.packages("sp", "sf", "leaflet", "rnaturalearth")
library(dplyr)
library(sp)
library(sf)
library(leaflet)
library(rnaturalearth)
library(ggplot2)
setwd('data/')
# расположение гидропостов в РФ
hp <- read.table(file = 'np_hydropost_utf8.csv', 
                 sep = ";", 
                 header = T, 
                 check.names = F, 
                 stringsAsFactors = F)


hp_sf <- st_as_sf(hp, coords = c('lon', 'lat'), 
                  crs = 4326)
hp_sf
# статичные карты
hp_map <- ggplot()  + 
  geom_sf(data = hp_sf)
hp_map
russ_pol <- ne_countries(country = 'Russia', 
                         returnclass = 'sf')
class(russ_pol)

hp_map + geom_sf(data = russ_pol)
hp_map + coord_sf(crs = 32645)
hp_map <- hp_map + geom_sf(data = russ_pol, fill='Red',
                           col='blue', alpha = 0.5) +
  coord_sf(crs = 32645)
hp_map
ggplot() + 
  geom_sf(data = russ_pol) +
  geom_sf(data = hp_sf) +
  coord_sf(crs = 32645)





# интерактивные карты
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = hp, lng = ~lon, lat = ~lat)

# классы площади
hp$size <- cut(hp$f, 
              labels = c('Очень малая', 
                         'Малая', 
                         'Средняя', 
                         'Большая'),
              breaks = c(-Inf, 2000, 20000, 50000, Inf), 
              ordered_result = T)
levels(hp$size)
# палитра
pal <- colorFactor(palette = 'Spectral', 
                   domain = hp$size)

leaflet(hp) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat,
                   popup = ~paste(name, f, sep = ', '),
                   radius = 2,
                   color = ~pal(size),
                   opacity = 1 ) %>%
  addLegend(colors = ~pal(levels(size)), 
            labels = ~levels(size))

# выбираем один класс
small_r <- hp %>%
  filter(size == 'Очень малая')
# датафрейм в пространственный объект
sp_posts <- st_as_sf(small_r, coords = c('lon', 'lat'), crs = 4326)
st_crs(sp_posts)
# изменение СК
sp_posts <- st_transform(sp_posts, crs = 3857)
# построение буфера вокруг точки
buf <- st_buffer(x = sp_posts, dist = 50000)

leaflet(st_transform(buf, crs = 4326)) %>%
  addTiles() %>%
  addPolygons() %>%
  addCircleMarkers(data = st_transform(sp_posts, 
                                       crs = 4326), 
                   radius = 1, color = 'red', 
                   opacity = 1, popup = ~name)

# расположение метеостанций 
syn_meta <- read.csv('np_meteost_utf8.csv')
syn_sf <- st_as_sf(syn_meta, 
                   coords = c('lon', 'lat'), crs = 4326)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = st_transform(buf, crs = 4326)) %>%
  addCircleMarkers(data = st_transform(sp_posts, 
                                       crs = 4326), 
                   radius = 1, color = 'red', 
                   opacity = 1, popup = ~name) %>%
  addCircleMarkers(data = syn_meta, lng = ~lon, lat = ~lat, 
                   radius = 1, color = 'green', opacity = 1)
# пространственное пересечение
intersect <- st_intersection(buf, st_transform(syn_sf, crs = 3857))
save(intersect, file = 'intersect.Rdata')

palbuf <- colorFactor(palette = 'Spectral', 
                   domain = hp$property)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = st_transform(intersect, crs = 4326), 
                   label = ~paste0(index.1, ', ', name.1),
                   fillColor = ~palbuf(property),
                   fillOpacity = 1,
                   stroke = F,
                   popup = ~paste(index, name, index.1, name.1, f, sep = ', '),
                   radius = ~f*0.005)
# экспорт в шейп
st_write(hp_sf, 'hydropost.shp', delete_layer = T, layer_options = "ENCODING=UTF8")
newhp <- st_read('hydropost.shp')
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = newhp)
