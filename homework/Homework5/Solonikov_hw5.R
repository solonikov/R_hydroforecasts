# загружаем сделанный ранее большой датафрейм с данными по Байкалу
load('data/baikal/prog_df.RData')

# импортируем пакеты
library(tidyverse)
library(dplyr)
library(hydroGOF)
library(ggplot2)

# определяем функцию для расчета S/σ
s_sigma <- function(sim, obs) {
  return( rmse(sim, obs) / sd(obs) )
}

# рассчитываем метрики, записываем в один датафрейм
err_df <- prog_df |> 
  group_by(month) |> 
  summarise(me = me(sim = pred, obs = fact), 
            mae = mae(sim = pred, obs = fact),
            rmse = rmse(sim = pred, obs = fact),
            nse = NSE(sim = pred, obs = fact),
            cor = cor(x = fact, y = pred), 
            sd = sd(x = fact),
            ssigma = s_sigma(sim = pred, obs = fact)) 

# создаём объект labeller с подписями, чтобы использовать на графике
mylabeller <- as_labeller(c('rmse' = 'Среднеквадратическая\nошибка',
                            'cor' = 'R', 
                            'ssigma' = 'S/σ'))

# выбираем из датафрейма нужные переменные и нужные месяцы, приводим к длинному
# виду и строим график
err_df |> 
  select(month, rmse, cor, ssigma) |> 
  filter(month %in% 5:9) |>  # выбираем месяцы с мая по сентябрь
  pivot_longer(cols = !month,
               names_to = 'variable',
               values_to = 'value') |> 
  ggplot(aes(x = month, y = value, fill = variable)) +
    geom_col() +
    facet_wrap(variable~., scales = 'free_y', labeller = mylabeller) +
    labs(x = 'месяц', y = 'значение') +
    theme_bw() +
    theme(legend.position = 'none')