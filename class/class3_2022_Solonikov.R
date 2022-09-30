library(tidyverse)
library(readxl)

rm(list = ls())
setwd("/Volumes/T7/мои файлы с юлиного компьютера/R_hydroforecasts/Ivan_Solonikov")

a <- c(2, 5, 6, 3, 1, 4)

a %>% 
  mean() %>% 
  sqrt() %>% 
  round(2)

# df <- read_xls(path = 'data/baikal/Долгоср.прогноз_Байкал_студ._7.xls',
#                skip = 10,
#                col_names = c('year',  'pred', 'pred2', 'fact'),
#                col_types = c('numeric', 'numeric', 'skip', 'numeric')) %>%    # здесь мы убрали из датафрейма третий столбец с помощью 'skip'
#       na.omit()    # избавляемся от всех оставшихся строк с отсутствующими данными
#   
# # colnames(df) <- c('one', 'two', 'three', 'four')    # так можно легко менять названия столбцов

# list.files(path = './data/baikal')    # просмотр всех файлов в папке

## Мы хотим прочитать сразу все excel-файлы из папки. Напишем для этого функцию
read_prognoz <- function(myfile) {
  read_xls(path = myfile,
           skip = 10,
           col_names = c('year',  'pred', 'pred2', 'fact'),
           col_types = c('numeric', 'numeric', 'skip', 'numeric')) %>%    # здесь мы убрали из датафрейма третий столбец с помощью 'skip'
    na.omit()
  # по умолчанию функция возвращает последнюю присвоенную переменную, поэтому можно не писать return
}

# df4 <- read_prognoz('data/baikal/Долгоср.прогноз_Байкал_студ._4.xls')

xls_files <- list.files(path = 'data/baikal/', pattern = 'xls')    # сохраняем имена файлов
# создаём пустой датафрейм, в котром будут потом лежать все данные
all_data <- data.frame()
# в цикле перебираем все файлы
for (file in xls_files) {
  df <- read_prognoz(paste('data/baikal/', file, sep = ''))
  all_data <- rbind(all_data, df)
}

# то же самое можно сделать лучше -- с помощью векторизированной функции lapply
setwd('/Volumes/T7/мои файлы с юлиного компьютера/R_hydroforecasts/Ivan_Solonikov/data/baikal')
xls_files <- list.files(pattern = 'xls')
prog_list <- lapply(X = xls_files, FUN = read_prognoz)    # получим список из 12 датафреймов -- один датафрейм на каждый файл
names(prog_list) <- c(1, 10, 11, 12, 2, 3, 4, 5, 6, 7, 8, 9)    # присваиваем имена датафреймам в списке
df_final <- do.call(what = rbind, args = prog_list)


