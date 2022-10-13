getwd()
setwd('/Volumes/T7/мои файлы с юлиного компьютера/R_hydroforecasts/Ivan_Solonikov/data/baikal')

library(readxl)

read_prognoz <- function(myfile, max_out = FALSE){
  df <- read_xls(path = myfile, 
                 skip = 10, 
                 col_names = c('year', 'pred', 'pred2', 'fact'), 
                 col_types = c('numeric', 'numeric', 'skip', 'numeric'))
  df <- na.omit(df)
  if(max_out == TRUE){
    maxpred <- max(df$pred)
    return(maxpred)
  }else{
    return(df)
  }
}

xls_files <- list.files(pattern = 'xls')
xls_files

all_data <- data.frame()  
for(file in xls_files){
  df <- read_prognoz(file, max_out = F)
  all_data <- rbind(all_data, df)
}

prog_list <- lapply(X = xls_files, FUN = read_prognoz)

names(prog_list) <- c(1, 10, 11, 12, 2, 3, 4, 5, 6, 7, 8, 9)

prog_list <- Map(cbind, prog_list, month = names(prog_list))

df_final <- do.call(what = rbind, args = prog_list)
rownames(df_final) <- NULL

library(ggplot2)
df_final$month <- as.integer((df_final$month))
df_final$month <- factor(df_final$month,
                         ordered = T)
str(df_final)
ggplot(df_final, aes(x=year)) + 
  geom_line(aes(y=pred, col='pred')) + 
  geom_line(aes(y=fact, col='fact')) +
  facet_wrap(month~., scales = 'free_y')


