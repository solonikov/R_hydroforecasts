Sys.setlocale("LC_ALL","Russian")
library(dplyr)
library(readxl)
library(ggplot2)
getwd()
load('data/rivers/river_data.RData')
load('data/rivers/weather_data.RData')
rivers <- read_xlsx('data/rivers/rivers_list.xlsx')
my_hp <- 78501
my_ws <- 34438

# выбираем
my_q <- river_data %>%
  filter(index == my_hp)
my_w <- weather_data %>%
  filter(index == my_ws)

# соединяем
df <- merge(my_q, my_w, by = 'date', suffixes = c('_hydro', '_meteo'))
df <- df[order(df$date),]
df <- df[,c(1, 3, 5, 6)]
summary(df)
# автокорреляция
acf(df$q, na.action = na.pass)
acf(df$prec, na.action = na.pass)

# перемотка
shift = 7
shift_data <- function(x, shift){
  for(col in colnames(x)[-1]){
    for(i in seq(1, shift, 1)){
      print(col)
      print(i)
      col_lag <- paste0(col, i)
      x[[col_lag]] <- lag(x[[col]], n = i)
    }
  }
  return(x)
}

lag_df <- shift_data(df, shift)
lag_df <- lag_df[shift+1:nrow(lag_df),]
# корреляция
q_cor <- cor(select(lag_df, -date), 
             use = 'complete.obs')

ggplot(melt(q_cor), aes(Var1, Var2, fill=value)) + 
  geom_tile() + 
  geom_text(aes(label=round(value, 2)), col='White')

train_df <- filter(lag_df, year(date) > 2013 & 
                     year(date) < 2020)
test_df <- filter(lag_df, year(date) <= 2013)
val_df <- filter(lag_df, year(date) >= 2020)
summary(train_df)
summary(test_df)

# построение множественной линейной регрессии
pred_q <- function(x, tau){
  tau <- 3
  x <- train_df
  predictors <- paste(colnames(x[,-c(1,2)])[tau:(length(colnames(x[,-c(1,2)]))-tau)], collapse = '+')
  predictors
  formula <- as.formula(q ~ predictors)
  mod <- nls(formula = formula, 
             start = list(a=1000, b=0, c=0),
             control = nls.control(maxiter = 1000, minFactor = 0.0003, printEval = T, warnOnly = T),
             trace=T, data = .)
  summary(mod1)
  formula(mod1)
  coef(mod1)
  return(predict(mod1, test_df))
}