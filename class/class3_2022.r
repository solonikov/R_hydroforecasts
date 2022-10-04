a <- c(2,5,6,3,1,4)
sort(a)
order(a, decreasing = T)
which.max(a)
which.min(a)
matrix(1:9, byrow = T, ncol = 3)

round(sqrt(mean(a)), digits = 3)

library(tidyverse)
a %>%
  mean() %>%
  sqrt() %>%
  round(2)

library(readxl)
getwd()
setwd('D:/YandexDisk/ИВПРАН/R forecasts/2022/rforecast-main/data/baikal')
list.files()

df <- read_xls(path ='Долгоср.прогноз_Байкал_студ._7.xls', 
               skip = 10, 
               col_names = c('year', 'pred', 'pred2', 'fact'), 
               col_types = c('numeric', 'numeric', 'skip', 'numeric'))
df <- na.omit(df)

read_prognoz <- function(myfile){
  df <- read_xls(path = myfile, 
                 skip = 10, 
                 col_names = c('year', 'pred', 'pred2', 'fact'), 
                 col_types = c('numeric', 'numeric', 'skip', 'numeric'))
  df <- na.omit(df)
}
  
df4 <- read_prognoz(myfile = 'Долгоср.прогноз_Байкал_студ._4.xls')  

xls_files <- list.files(pattern = 'xls')
xls_files

all_data <- data.frame()  
for(file in xls_files){
  df <- read_prognoz(file)
  all_data <- rbind(all_data, df)
}

prog_list <- lapply(X = xls_files, FUN = read_prognoz)

names(prog_list) <- c(1, 10, 11, 12, 2, 3, 4, 5, 6, 7, 8, 9)
df_final <- do.call(what = rbind, args = prog_list)

