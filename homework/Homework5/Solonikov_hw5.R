setwd("data/baikal/")
# загружаем сделанный ранее большой датафрейм с данными по Байкалу
load('prog_df.RData')

library(tidyverse)
library(dplyr)
library(hydroGOF)

prog_df %>%
  group_by(month) %>%
  mutate(err = pred - fact) %>%
  summarise(me = mean(err))


err_df <- prog_df %>%
  group_by(month) %>%
  summarise(me = me(sim = pred, obs = fact), 
            mae = mae(sim = pred, obs = fact),
            rmse = rmse(sim = pred, obs = fact),
            nse = NSE(sim = pred, obs = fact),
            cor = cor(x = fact, y = pred), 
            sd = sd(x = fact)) 