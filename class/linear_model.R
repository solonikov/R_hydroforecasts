Sys.setlocale("LC_ALL","Russian")
library(dplyr)
library(RPostgreSQL)
library(readxl)
library(ggplot2)
getwd()
# гидрология
load('data/rivers/river_data.RData')

# выбираем
df <- river_data %>%
  filter(index == 78519)

# автокорреляция
acf(df$q, na.action = na.pass)
acf(df$prec, na.action = na.pass)

# перемотка
lags <- seq(14)
lag_names <- paste('q', sprintf(fmt = "%02d", lags), sep = "_")
lag_set <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)
lag_df <- df %>% mutate_at(vars(q), funs_(lag_set))

q_cor <- cor(select(lag_df, contains('q')), 
             use = 'complete.obs')

ggplot(melt(q_cor), aes(X1, X2, fill=value)) + geom_tile() + geom_text(aes(label=round(value, 2)), col='White')

