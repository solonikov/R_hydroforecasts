library(tidyverse)
library(hydroGOF)
library(ggplot2)

setwd('/Volumes/T7/мои файлы с юлиного компьютера/R_hydroforecasts/Ivan_Solonikov/class')
source('class4_2022.r')
prog_df <- df_final

prog_df %>% 
  group_by(month) %>% 
  mutate(err = pred - fact) %>% 
  summarize(me = mean(err))

err_df <- prog_df %>% 
  group_by(month) %>% 
  summarize(
    me = me(sim = pred, obs = fact),
    mae = mae(sim = pred, obs = fact),
    rmse = rmse(sim = pred, obs = fact),
    nse = NSE(sim = pred, obs = fact),
    cor = cor(x = fact, y = pred),
    sd = sd(x = fact)
  )
# %>% 
#   filter(month == 5 | month == 6) %>% 
#   select(month, cor)

err_df_long <- err_df %>% 
  pivot_longer(
    cols = !month,    # указываем либо колонки, которые будут трансформированы в длинный вид, либо, как здесь, через символ "!" колонки, которые функция не должна менять
    names_to = 'variable',
    values_to = 'value'
    )

labeller = as_labeller(c(
  'me' = 'Средняя ошибка',
  'mae' = 'Абсолютная ошибка',
  'rmse' = 'Среднеквадратическая ошибка',
  'nse' = 'NSE',
  'cor' = 'R',
  'sd' = 'СКО'
))    # можем создавать вот такие объекты для подписи величин на графике, который мы строим ниже

ggplot(err_df_long,
       aes(x = month, y = value, fill = variable)) +
  geom_col() +
  facet_wrap(variable~., scales = "free_y", labeller = labeller) +
  theme_light(base_size = 20) +
  labs(x = "Месяц", y = "Значение", fill = "Характеристика")

setwd('/Volumes/T7/мои файлы с юлиного компьютера/R_hydroforecasts/Ivan_Solonikov/class')
ggsave('plots/baikal_plot.png', device = 'png', width = 30, height = 15,
       units = 'cm', dpi = 150)

acf(prog_df$fact)


