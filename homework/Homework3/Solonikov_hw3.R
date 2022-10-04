library(tidyverse)
library(readxl)

rm(list = ls())
# setwd(...)

mean_diff_func <- function(file_path) {
  df <- read_excel(path = file_path,
                   skip = 6,
                   col_types = c('numeric', 'numeric', 'skip', 'numeric', rep('skip', 6)),
                   col_names = c("Year", "Prognoz", "", "Fact", rep("", 6))) %>% 
    na.omit() %>%
    mutate(Diff = Prognoz - Fact)
  
  return(mean(df$Diff))
}


mean_diff <- mean_diff_func('data/baikal/Долгоср.прогноз_Байкал_студ._7.xls')
print(mean_diff)
