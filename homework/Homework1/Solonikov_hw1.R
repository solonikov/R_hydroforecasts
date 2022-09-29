rm(list = ls())

#### Задание 1
## сделать из вектора названий месяцев матрицы 3х4 двух видов - с заполнением по строкам и по столбцам
months_by_row <- matrix(data = month.name, nrow = 3, ncol = 4, byrow = T)
months_by_column <- matrix(data = month.name, nrow = 3, ncol = 4, byrow = F)
print(months_by_row)
print(months_by_column)

#### Задание 2
## сделать датафрейм со столбцами (названия столбцов любые): названия месяцев, данные от трех переменных (всего 4 столбца) - три вектора нормально или равномерно распределенных величин
season_col <- sapply(X = c('winter', 'spring', 'summer', 'fall'),
                     FUN = rep, times = 3) |> c()
season_col <- c(season_col[-1], season_col[1])
month_data <- data.frame(months = month.name,
                         season = season_col |> as.factor(),
                         a = rnorm(12, mean = 5, sd = 7.5) * 4,
                         b = seq(-10, 10, length.out = 12) * 3,
                         c = rnorm(12, mean = -5, sd = 20) * 5)
print(month_data)

#### Задание 3
## "вытянуть" датафрейм из широкого формата в длинный
library(reshape2)
month_data_long <- melt(month_data)
print(month_data_long)

#### Задание 4
## нарисовать точечно-линейный график от этих трех переменных
library(ggplot2)
ggplot(data = month_data_long, aes(x = months, y = value, color = variable, group = variable)) +
  geom_point(size = 2) + 
  geom_line(size = .75) +
  scale_x_discrete(limits = month.name)

