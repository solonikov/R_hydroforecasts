# импортируем пакеты
library(directlabels)
library(ggplot2)
library(dplyr)

# создаём датафрейм с произвольными значениями осеннего увлажнения и зимних
# влагозапасов
df <- data.frame(year = seq(1951,2000), 
                 zim_vlag = runif(50, 1, 250),
                 os_uvl = runif(50, 1, 50))

# уравнение для долгосрочного прогноза весеннего стока в виде функции
forecast_fn <- function(zimvlag, osuvl) {
  zimvlag * exp(-(140-3*osuvl)/zimvlag)
}

# рассчитываем новый столбец с прогнозными значениями стока
df <- df |>
        mutate(Y = forecast_fn(zim_vlag, os_uvl))

# строим график
df |> 
  ggplot(aes(x = zim_vlag, y = Y)) +
  geom_point(size = .8) +
  labs(x = 'зимние влагозапасы, мм',
       y = 'сток, мм',
       title = expression(График~зависимости~Y==S * e^-{(140 - 3*U)/S})) +
  theme_classic()

