Sys.setlocale("LC_ALL","Russian")
library(dplyr)
library(RPostgreSQL)
library(readxl)
library(ggplot2)
getwd()
# гидрология
load('data/rivers/river_data.RData')
rivers <- read_xlsx('data/rivers/rivers_list.xlsx')
# выбираем
df <- river_data %>%
  filter(index == 78501)
df <- df[order(df$date),]
# автокорреляция
acf(df$q, na.action = na.pass)
acf(df$prec, na.action = na.pass)

# перемотка
lags <- seq(7)
lag_names <- paste('q', sprintf(fmt = "%02d", lags), 
                   sep = "")
lag_set <- setNames(paste("dplyr::lag(., ", lags, ")"), 
                    lag_names)
lag_df <- df %>% mutate_at(vars(q), 
                           funs_(lag_set))

lag_df <- lag_df[8:nrow(lag_df),]


q_cor <- cor(select(lag_df, contains('q')), 
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
  predictors <- colnames(x[,-c(1,2)])[tau:length(colnames(x))-tau]
  formula <- as.formula(q ~ )
  mod <- lm(data = x[,-c(1, 2)], 
             formula = q ~ q01 + q02 + q03 + q04 + q05 + q06 + q07)
  summary(mod1)
  formula(mod1)
  coef(mod1)
  return(predict(mod1, test_df))
}

test_df$pred1 <- 
