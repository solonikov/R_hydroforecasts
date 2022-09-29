Sys.setlocale("LC_ALL","Russian")
library(sp)
library(sf)
library(leaflet)
setwd('C:/R_pr/hydr_forecasts/rforecast/data')
gmvo <- read.csv('year_count_gmvo.csv')
hydropost <- read.table(file = 'np_hydropost.csv', sep = ";", header = T, check.names = F, stringsAsFactors = F)
df <- merge(hydropost, gmvo, by = 'index')

df$size = cut(df$f, labels = c('Очень малая', 'Малая', 'Средняя', 'Большая'),
              breaks = c(-Inf, 2000, 20000, 50000, Inf), ordered_result = T)

pal = colorFactor(palette = 'Spectral', domain = df$size)

leaflet(df) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat,
                   popup = ~paste(name, f, sep = ', '),
                   radius = 2,
                   color = ~pal(size),
                   opacity = 0.5)

small_r = df[df$size == 'Очень малая',]
sp_posts = st_as_sf(small_r, coords = c('lon', 'lat'), crs = 4326)
st_crs(sp_posts)
sp_posts <- st_transform(sp_posts, crs = 3857)
buf = st_buffer(sp_posts, 50000)

pal_buf = colorFactor(palette = 'Spectral', domain = buf$property)

leaflet(st_transform(buf, crs = 4326)) %>%
  addTiles() %>%
  addPolygons() %>%
  addMarkers(data = st_transform(sp_posts, crs = 4326))

leaflet(st_transform(buf, crs = 4326)) %>%
  addTiles() %>%
  addPolygons(color = ~pal_buf(property),
              opacity = 1) %>%
  addLegend(pal = pal_buf, values = ~property,
            title = "Принадлежность",
            opacity = 1)

syn_meta <- read.csv('np_meteost.csv')

leaflet(st_transform(buf, crs = 4326)) %>%
  addTiles() %>%
  addPolygons(color = ~pal_buf(property),
              opacity = 1) %>%
  addLegend(pal = pal_buf, values = ~property,
            title = "Принадлежность",
            opacity = 1) %>%
  addMarkers(data = syn_meta, lng = ~lon, lat = ~lat)

intersect = st_intersection(buf, 
                            st_transform(st_as_sf(syn_meta, 
                                                       coords = c('lon', 'lat'), crs = 4326), crs = 3857))

intersect_lonlat = st_transform(intersect, crs = 4326)


leaflet(intersect_lonlat) %>%
  addTiles() %>%
  addCircleMarkers(label = ~paste0(index.1, ', ', name.1),
                   fillColor = ~pal_buf(property),
                   fillOpacity = 1,
                   stroke = F,
                   popup = ~paste(index, name, index.1, name.1, f, sep = ', '),
                   radius = ~f*0.005)
#-------------------------------------------------------------------------------

syn_meta <- read.csv('np_meteost.csv')
leaflet(hydropost[hydropost$f <= 2000 & hydropost$lon <= 60,]) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, popup = ~paste(name, f, sep = ', '), color = ~pal(f)) %>%
  addLegend(pal = pal, values = ~f,
            title = "Площадь водосбора",
            opacity = 1) %>%
  addCircleMarkers(data = syn_meta[syn_meta$lon <= 60, ], lng = syn_meta$lon, lat = syn_meta$lat, 
                   popup = syn_meta$name, color = 'Red', radius = 2) %>%
  addPolygons(data = st_transform(buf, crs = 4326), color = 'Yellow')

intersc <- st_intersection(buf, st_transform(st_as_sf(syn_meta, coords = c('lon', 'lat'), crs = 4326), crs = 3857))
inters_lonlat <- st_transform(intersc, crs = 4326)
leaflet(inters_lonlat) %>%
  addTiles() %>%
  addCircleMarkers(color = 'Yellow') %>%
  addMarkers(data = st_transform(sp_posts, 4326))
  
