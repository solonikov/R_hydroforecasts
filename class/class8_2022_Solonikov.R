library(dplyr)
library(sp)
library(sf)
library(leaflet)
library(rnaturalearth)
library(ggplot2)
library(tidyr)
setwd('data/')

hp <- st_read('hydropost.shp')
load('river_data.RData')
load('weather_data.RData')
load('intersect.Rdata')

posts <- hp |> 
  filter(index %in% unique(river_data$index))

myriver_index <- 75254    # индекс гидропоста (мой вариант)

myriver <- intersect |> 
  filter(index == myriver_index)

my_weather <- weather_data |> 
  filter(index == myriver$index.1)

my_riverdata <- river_data |> 
  filter(index == myriver$index)

df <- merge(my_riverdata, my_weather, by = 'date') |> 
  select(date, value, mean_temp, prec)

df |> 
  pivot_longer(cols = c(value, mean_temp, prec),
              names_to = 'variable',
              values_to = 'value') |> 
  ggplot(aes(x = date, y = value, col = variable)) +
  geom_line() +
  facet_wrap(variable~., scales = 'free_y', ncol = 1)
