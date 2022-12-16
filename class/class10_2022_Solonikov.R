library(tidyverse)
library(lubridate)
library(prophet)
library(ggplot2)

df1 <- data.frame(
  date = seq.Date(from = as.Date('2022-01-01'),
                  length.out = 12,
                  by = 'month'),
  q1 = rnorm(12)
)

df2 <- data.frame(
  date = seq.POSIXt(from = as.POSIXct('2022-01-01',
                                      tz = 'GMT'),
                  length.out = 12,
                  by = 'month'),
  q2 = rnorm(12)
)

df <- merge(df1,
            df2 |> mutate(date = as.Date(date)),
            by = 'date')

##################################################

load('class/prog_df.RData')

pritok <- prog_df |> 
  mutate(ds = make_date(year, month, 1)) |> 
  select(ds, fact) |> 
  arrange(ds) |> 
  rename(y = fact)

pritok_train <- pritok |> 
  slice(1:(n() - 24))

pritok_test <- pritok |> 
  tail(24)

MO <- prophet(pritok_train,
              weekly.seasonality = F,
              daily.seasonality = F,
              seasonality.mode = 'multiplicative')

#future_df <- make_future_dataframe(MO, periods = )
# НЕ ЗАКОНЧИЛ -- смотри полную версию в файле этого занятия от преподавателя



