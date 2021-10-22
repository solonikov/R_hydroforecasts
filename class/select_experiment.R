Sys.setlocale("LC_ALL","Russian")
# install.packages("sp", "sf", "leaflet")
library(sp)
library(sf)
library(leaflet)
getwd()
setwd('c:/Users/morey/Documents/R_Projects/rforecast/data')
gmvo <- read.csv('year_count_gmvo.csv')
hydropost <- read.csv(file = 'np_hydropost.csv',encoding = "UTF-8", sep = ";", header = T, check.names = F, stringsAsFactors = F)
df <- merge(hydropost, gmvo, by = 'index')
df$cat <- cut(x = df$f, breaks = c(-Inf, 2000, 20000, 50000, Inf), 
    labels = c('очень малая', 'малая', 'средняя', 'большая'), 
    ordered_result = T)
summary(df)
str(df)

leaflet(df) %>%
  addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~name)

# bins <-c(0, 1000, 2000, 5000, 10000, 50000, 100000, 500000, 3000000) 
# pal <- colorBin("YlGnBu", bins=bins, na.color = "#aaff56", reverse = T)

pal <- colorFactor(palette = 'Spectral', domain = df$cat)

leaflet(df) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, 
                   popup = ~paste(name, f, sep = ', '), 
                   radius = ~q,  
                   fill = T, 
                   fillColor = ~pal(cat), 
                   fillOpacity = 1, stroke = T, 
                   color = 'Black', weight = 0.5, 
                   clusterOptions = markerClusterOptions())

leaflet(df) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, 
                   popup = ~paste(name, f, sep = ', '), 
                   radius = ~q*0.5,  
                   fill = T, 
                   fillColor = ~pal(cat), 
                   fillOpacity = 1, stroke = T, 
                   color = 'Black', weight = 0.5) %>%
addLegend(pal = pal, values = ~cat, 
            title = "Размер реки",
            opacity = 1)

small_r <- df[df$cat == 'очень малая',]

sp_hydr <- st_as_sf(small_r, coords = c('lon', 'lat'), crs = 4326)
st_crs(sp_hydr)
sp_hydr <- st_transform(sp_hydr, crs = 3857)
st_crs(sp_hydr)
st_write(sp_hydr, 'sp_hydr.shp')
buf <- st_buffer(sp_hydr, 50000)

leaflet(st_transform(buf, crs = 4326)) %>%
  addTiles() %>%
  addPolygons() %>%
  addMarkers(data = st_transform(sp_hydr, crs = 4326))

pal1 <- colorFactor(palette = 'viridis', 
                    domain = buf$property, 
                    na.color = 'Red')

leaflet(st_transform(buf, crs = 4326)) %>%
  addTiles() %>%
  addPolygons(color = ~pal1(property),
              fill = T, 
              fillColor = ~pal1(property), opacity = 1,
              fillOpacity = 1, stroke = T) %>%
  addLegend(pal = pal1, values = ~property, 
            title = "Принадлежность",
            opacity = 1)

syn_meta <- read.csv('np_meteost.csv', encoding = "UTF-8")
summary(syn_meta)
leaflet(st_transform(buf, crs = 4326)) %>%
  addTiles() %>%
  addPolygons(color = ~pal1(property),
              fill = T, 
              fillColor = ~pal1(property), opacity = 0.5,
              fillOpacity = 0.5, stroke = T) %>%
  addLegend(pal = pal1, values = ~property, 
            title = "Принадлежность",
            opacity = 1) %>%
  addMarkers(data = syn_meta, lng = ~lon, lat = ~lat)


intersc <- st_intersection(buf, 
                           st_transform(
                             st_as_sf(syn_meta, 
                                      coords = c('lon', 'lat'), 
                                      crs = 4326), 
                             crs = 3857))

inters_lonlat <- st_transform(intersc, crs = 4326)
leaflet(inters_lonlat) %>%
  addTiles() %>%
  addCircleMarkers(radius = ~f * 0.01, fillColor = ~pal1(property), 
                   fillOpacity = 1, 
                   stroke = T, color = 'Black',
                   opacity = 1, weight = 0.3,
                   popup = ~paste(index, name, f, index.1, name.1, 
                                  sep = ', '))


