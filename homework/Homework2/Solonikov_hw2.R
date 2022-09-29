rm(list = ls())

library(ggplot2)
library(reshape2)

## а) сделать датафрейм с произвольным количеством данных (строк), в котором должны
## быть столбец с датой и две независимые переменные (например, сгенерированные)

date <- seq.Date(from = as.Date('2020-06-01'), 
                 to = as.Date('2022-07-01'), by = 'day')

turbidity <- abs(rnorm(n = length(date), mean = 50, sd = 30))
discharge <- runif(n = length(date), min = 10, max = 150)

df <- data.frame(date, turbidity, discharge)

## б) сделайте две переменных-факторов на основании значений численных переменных
## по нескольким классам (не менее 3)

df$turbidityClass <- cut(df$turbidity,
                         breaks = c(0, 35, 65, 200),
                         labels = c('низкая мутность', 'средняя мутность', 'высокая мутность'))

df$dischargeClass <- cut(df$discharge,
                         breaks = c(0, 50, 100, 200),
                         labels = c('низкий расход', 'умеренный расход', 'высокий расход'))

## в) вытяните датафрейм в длинный формат, взяв в качестве переменных для группировки
## дату и ваши два фактора

dfLong <- melt(data = df, id.vars = c('date', 'turbidityClass', 'dischargeClass'))

## г) исследуйте, как меняются графики в зависимости от того, какие переменные вы
## выбираете в качестве аргументов для facet_grid и facet_wrap. 
ggplot(data = dfLong, aes(x = date, y = value, color = variable)) +
  geom_point(size=2) + geom_line(size=1) + 
  facet_grid(turbidityClass~variable, scales = 'free_y') +
  labs(x = 'дата', y = 'значение', col = 'параметр')

ggplot(data = dfLong, aes(x = date, y = value, color = variable)) +
  geom_point(size=2) + geom_line(size=1) + 
  facet_wrap(turbidityClass~variable, scales = 'free_y') +
  labs(x = 'дата', y = 'значение', col = 'параметр')

ggplot(data = dfLong, aes(x = date, y = value, color = variable)) +
  geom_point(size=2) + geom_line(size=1) + 
  facet_grid(dischargeClass~variable, scales = 'free_y') +
  labs(x = 'дата', y = 'значение', col = 'параметр')

ggplot(data = dfLong, aes(x = date, y = value, color = variable)) +
  geom_point(size=2) + geom_line(size=1) + 
  facet_wrap(dischargeClass~variable, scales = 'free_y') +
  labs(x = 'дата', y = 'значение', col = 'параметр')
