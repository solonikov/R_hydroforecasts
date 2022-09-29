Sys.setlocale("LC_ALL","Russian")
library(dplyr)
library(readxl)
library(ggplot2)
getwd()
# гидрология
load('data/rivers/river_data.RData')

df <- river_data %>%
  filter(index == 78501)

df$q_t <- lag(df$q, 7)
df$delta <- df$q - df$q_t
sd(x = df$delta, na.rm = T)

ggplot(df, aes(x=date)) + 
  geom_line(aes(y=q, col='q')) + 
  geom_line(aes(y=q_t, col='q7')) +
  geom_line(aes(y=delta, col='delta'))
  

ssd <- function(x, t){
  x_t <- lag(x, t)
  delta <- x - x_t
  return(sd(x = delta, na.rm = T))
}

for(t in 1:7){
  print(ssd(df$q, t))
}
