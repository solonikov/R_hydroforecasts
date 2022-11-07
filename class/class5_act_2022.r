setwd('D:/YandexDisk/ИВПРАН/R forecasts/2022/rforecast-main/data/baikal')
# загружаем сделанный ранее большой датафрейм с данными по Байкалу
load('prog_df.RData')
# загружаем необходимые пакеты
library(tidyverse)
library(dplyr)
library(hydroGOF)
# основной синтаксис пайплайна
prog_df %>%
  group_by(month) %>% # группировка по признаку
  mutate(err = pred - fact) %>% # эквивалентная запись создания нового столбца prog_df$err <- prog_df$pred - prog_df$fact
  summarise(me = mean(err)) # осреднение по новой переменной

# создание нового датафрейма в результате вычислений
err_df <- prog_df %>%
  group_by(month) %>%
  summarise(me = me(sim = pred, obs = fact), 
            mae = mae(sim = pred, obs = fact),
            rmse = rmse(sim = pred, obs = fact),
            nse = NSE(sim = pred, obs = fact),
            cor = cor(x = fact, y = pred), 
            sd = sd(x = fact)) 

# додвыборки строк и столбцов в результате логических операций
err_df %>%
  group_by(month) %>%
  filter(month == 5) %>% # ==, >=, <=, !=, &, |, %in%
  select(month, cor, rmse, sd)

# превращение широкого формата в длинный 
err_long <- err_df %>%
  pivot_longer(cols = !month, # список столбцов, которые "вытягиваются", в данном случае - все, кроме месяца
               names_to = 'variable',# как будет называться новая переменная-признак
               values_to = 'value') # как будет называться новая переменная значений

# рисуем графики метрик
library(ggplot2)
ggplot(err_long, aes(x=month, y=value, fill=variable)) + 
  geom_col() + 
  facet_wrap(variable~., scales = 'free_y') +
  theme_light(base_size = 20, base_family = 'serif') + 
  labs(x='Месяц', y='Значение', fill='')

# делаем человеческие подписи подграфикам 
labeller <- as_labeller(c('me' = 'Средняя ошибка',
                         'mae' = 'Абсолютная ошибка',
                         'rmse' = 'Среднеквадратическая ошибка',
                         'nse' = 'NSE',
                         'cor' = 'R', 
                         'sd' = 'СКО'))
# добавляем человеческие подписи
ggplot(err_long, aes(x=month, y=value, fill=variable)) + 
  geom_col() + 
  facet_wrap(variable~., scales = 'free_y', 
             labeller = labeller) +
  theme_light(base_size = 20, base_family = 'serif') + 
  labs(x='Месяц', y='Значение', fill='')
# сохраняем последний рисунок в рабочую папку
ggsave(filename = 'baikal_plot.png', device = 'jpg', 
       width = 25, height = 10, units = 'cm', dpi = 300)
