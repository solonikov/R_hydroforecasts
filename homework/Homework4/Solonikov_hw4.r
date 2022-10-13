rm(list = ls())

getwd()
setwd('/Volumes/T7/мои файлы с юлиного компьютера/R_hydroforecasts/Ivan_Solonikov/data/baikal')

library(readxl)
library(tibble)

read_prognoz <- function(myfile){
  df <- read_xls(path = myfile, 
                 skip = 10, 
                 col_names = c('year', 'pred', 'pred2', 'fact'), 
                 col_types = c('skip', 'numeric', 'skip', 'numeric'))
  df <- na.omit(df)
  
  AE <- df$pred - df$fact    # абсолютная ошибка (вектор)
  L_1 <- length(AE) - 1    # служебная константа -- длина ряда минус 1 (чтобы не перевычислять в каждом выражении)
  ME <- sum(AE) / L_1    # средняя ошибка
  MAE <- sum(abs(AE)) / L_1    # средняя абсолютная ошибка
  MSE <- sum(AE ** 2) / L_1    # среднеквадратическая ошибка
  sigma <- sqrt(sum((AE - (sum(df$fact) / L_1)) ** 2) / L_1)
  S <- sqrt(MSE)
  `S/sigma` <- S / sigma
  return(tibble(ME, MAE, MSE, sigma, S, `S/sigma`))
}

xls_files <- list.files(pattern = 'xls')
xls_files

prog_list <- lapply(X = xls_files, FUN = read_prognoz)

names(prog_list) <- c(1, 10, 11, 12, 2, 3, 4, 5, 6, 7, 8, 9)

prog_list <- Map(cbind, prog_list, month = names(prog_list))

df_final <- do.call(what = rbind, args = prog_list)
rownames(df_final) <- NULL

print(df_final)


