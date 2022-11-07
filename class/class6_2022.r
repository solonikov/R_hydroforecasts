# загружаем необходимые пакеты
library(tidyverse)
library(dplyr)
library(hydroGOF)
setwd('D:/YandexDisk/ИВПРАН/R forecasts/2022/rforecast-main/data/baikal')
# загружаем сделанный ранее большой датафрейм с данными по Байкалу
load('err_long.RData')
comment(err_long) <- 'Значения метрик ошибок прогноза'
comment(err_long)

labeller <- as_labeller(c('me' = 'Средняя ошибка',
                          'mae' = 'Абсолютная ошибка',
                          'rmse' = 'Среднеквадратическая ошибка',
                          'nse' = 'NSE',
                          'cor' = 'R', 
                          'ssigma' = 'S/sigma'))
ggplot(err_long, aes(x=month, y=value, fill=variable)) + 
  geom_col() + 
  facet_wrap(variable~., scales = 'free_y', 
             labeller = labeller) +
  theme_light(base_size = 20, base_family = 'serif') + 
  labs(x='Месяц', y='Значение', fill='')

# делаем человеческие подписи подграфикам - через изменение названий переменных в датафрейме
err_long$variable <- factor(err_long$variable, 
                            levels = unique(err_long$variable), 
                            labels = c("ME", "MAE", "S", "NSE", "R", expression(S / sigma)))

# добавляем человеческие подписи
ggplot(err_long, aes(x=month, y=value, fill=variable)) + 
  geom_col() + 
  facet_wrap(variable~., scales = 'free_y', 
             labeller = label_parsed) +
  theme_light(base_size = 20, base_family = 'serif') + 
  labs(x='Месяц', y='Значение', fill='') +
  scale_fill_discrete(labels = parse(text = levels(err_long$variable)))

demo(plotmath)

library(directlabels)
setwd('D:/YandexDisk/ИВПРАН/R forecasts/')
df <- data.frame(expand.grid(seq(0.001, 250, 10), 
                             seq(0.001, 100, 10)))
colnames(df) <- c('x', 'p0')
df$y <- df$x * exp(-df$p0 / df$x)
p <- ggplot(df, aes(x=x, y=y, group=p0)) + geom_line() 
p

p <- p + annotate("text", x = 215, y = 235, 
                  label = "italic(P[0])",
                  parse = T)
p
p <- p + theme_light(base_size = 16) + 
  theme(legend.position = 'bottom') 
p
p <- p + geom_dl(aes(label=round(p0)), 
                 method = list("last.points"))
p
p <- p + labs(x='Осадки X, мм', y='Сток Y, мм', col=expression('Параметр P'[0]*', мм'),
              title = expression(График~зависимости~Y==X %*% exp(-P[0]/X)), subtitle =  
                "\n (В кн.: \"Руководство по гидрологическим прогнозам. 
          Выпуск 1. Долгосрочные прогнозы элементов водного режима рек и 
          водохранилищ.\" Стр. 123, ф-ла 3.12.)")
p  
ggsave(filename = 'formula_3.12_prognozy.png', path = getwd(), 
       dpi = 300, device = 'png', height = 10, width = 10, limitsize = F)

