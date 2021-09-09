Sys.setlocale("LC_ALL","Russian")
library(dplyr)
library(hydroGOF)
library(ggplot2)
setwd('d:/YandexDisk/ИВПРАН/R forecasts/байкал')
load('prog_df.RData')

# подсчеты
prog_df %>%
  dplyr::count()

prog_df %>%
  dplyr::count(year)

prog_df %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(n = n())

prog_df %>%
  dplyr::count(month)

prog_df %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(n = n())

prog_df %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(mean_obs = mean(obs, na.rm = T), 
                   sum_obs = sum(obs, na.rm = T),
                   n_obs = n(), 
                   mean_obs1 = sum_obs / n_obs)

prog_df %>%
  dplyr::count(obs, sort = T)


# бинарные оценки
setwd('d:/YandexDisk/ИВПРАН/R forecasts/2020')
load('model7.RData')

summary(train_df)
quantile(train_df$Absolute_Levels, probs = c(0.75))
q90 <- quantile(train_df$Absolute_Levels, probs = c(0.90))
ggplot(train_df, aes(x=Absolute_Levels)) + stat_ecdf() + 
  scale_y_continuous(breaks = seq(0, 1, 0.1)) + 
  geom_vline(xintercept = q90)
binary_df <- test_df %>%
  dplyr::select(Dates, `Absolute_Levels(t+7)`, pred7) %>%
  dplyr::mutate(obs_flood = ifelse(test = `Absolute_Levels(t+7)` >= q90, yes = 1, no = 0),
                pred_flood = ifelse(pred7 >= q90, 1, 0))
summary(binary_df)
str(binary_df)
library(verification)
verif7 <- verify(obs = binary_df$obs_flood, pred = binary_df$pred_flood, obs.type = "binary", frcst.type = "binary")
verif7$tab
verif7$POD
verif7$FAR
verif7$BIAS
verif7$PC
verif7$TS
