Sys.setlocale("LC_ALL","Russian")
# install.packages("sp", "sf", "leaflet")
library(sp)
library(sf)
library(leaflet)
getwd()
setwd('c:/Users/morey/Documents/R_Projects/rforecast/data')
gmvo <- read.csv('year_count_gmvo.csv')
hydropost <- read.table(file = 'np_hydropost.csv', sep = ";", header = T, check.names = F, stringsAsFactors = F)
df <- merge(hydropost, gmvo, by = 'index')

leaflet(hydropost) %>%
  addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~name)

bins <-c(0, 1000, 2000, 5000, 10000, 50000, 100000, 500000, 3000000) 
pal <- colorBin("YlGnBu", bins=bins, na.color = "#aaff56", reverse = T)

leaflet(hydropost) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, popup = ~paste(name, f, sep = ', '), radius = 1,  
                   color = ~pal(f), opacity = 1, fillColor =~pal(f))

leaflet(hydropost) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, popup = ~paste(name, f, sep = ', '), radius = 1, color = ~pal(f),  opacity = 1, fillColor =~pal(f)) %>%
  addLegend(pal = pal, values = ~f,
            title = "Площадь водосбора",
            opacity = 1)

leaflet(hydropost[hydropost$f <= 2000 & hydropost$lon <= 60,]) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, popup = ~paste(name, f, sep = ', '), radius = 1, color = ~pal(f),  opacity = 1, fillColor =~pal(f)) %>%
  addLegend(pal = pal, values = ~f,
            title = "Площадь водосбора",
            opacity = 1)

library(sf)
sp_hydr <- st_as_sf(hydropost, coords = c('lon', 'lat'), crs = 4326)
st_crs(sp_hydr)
sp_hydr <- st_transform(sp_hydr, crs = 3857)
st_crs(sp_hydr)
buf <- st_buffer(sp_hydr, 10000)

leaflet(st_transform(buf, crs = 4326)) %>%
  addTiles() %>%
  addPolygons() %>%
  addMarkers(data = hydropost)

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
  addMarkers(hydropost, lng = hydropost$lon, lat = hydropost$lat)
