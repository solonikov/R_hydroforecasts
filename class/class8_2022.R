Sys.setlocale("LC_ALL","Russian")
library(dplyr)
library(ggplot2)
library(sp)
library(sf)
library(leaflet)
library(lubridate)
setwd('D:/YandexDisk/ИВПРАН/R forecasts/2022/rforecast-main/data/')
hp <- st_read('rivers/hydropost_2000.shp')
load('rivers/river_data.RData')
load('rivers/weather_data.RData')

river_data %>%
  group_by(index, year = year(date)) %>%
  summarise(obs = sum(!is.na(value), na.rm = T)) %>%
  ggplot(aes(x=year, y=factor(index), fill=obs)) + geom_tile() +
  geom_text(aes(label=obs), col='White')


posts <- hp %>%
  filter(index %in% unique(river_data$index))




mypost <- 77327

myriver <- hp %>%
  filter(index == mypost)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = st_transform(posts, crs = 4326), 
                   radius = 3, label = ~name, color = 'Red') %>%
  addCircleMarkers(data = st_transform(myriver, crs = 4326), 
                   radius = 5, label = ~name, color = 'Yellow')



myriverdata <- river_data %>%
  filter(index == mypost)

myweatherdata <- weather_data %>%
  filter(index == myriver$index_1)

df <- merge(mydata, myweather, by = 'date')

df <- select(df, date, value, mean_temp, prec)
ggplot(melt(df, id.vars = 'date'), aes(x=date, y=value, col=variable)) + 
  geom_line() + 
  facet_wrap(variable~., scales = 'free_y', ncol = 1) +
  theme_light(base_size = 20)
