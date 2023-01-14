Sys.setlocale("LC_ALL","Russian")
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(reshape2)
library(readxl)
library(writexl)
library(airGRteaching)
#setwd('d:/YandexDisk/ИВПРАН/R forecasts/2022/rforecast-main/')
load('data/rivers/river_data.RData')
load('data/rivers/weather_data.RData')

my_hp <- 75367
my_ws <- 26896

# выбираем
my_q <- river_data %>%
  filter(index == my_hp)
my_w <- weather_data %>%
  filter(index == my_ws)

# соединяем
df <- merge(my_q, my_w, by = 'date', 
            suffixes = c('_hydro', '_meteo'))
summary(df)

# сохраняем исходные данные
save(df, file = 'data/rivers/my_river_data.RData')

# потенциальное испарение
df$En <- PE_Oudin(JD = yday(df$date), 
                 Temp = df$mean_temp, 
                 Lat = 53.88, LatUnit = "deg")

df %>%
  select(!contains('index')) %>%
  pivot_longer(!date, names_to = 'variable', 
               values_to = 'value') %>%
  ggplot(aes(x=date, y=value, col=variable)) + 
         geom_line() + 
  facet_wrap(variable~., ncol = 1, scales = 'free_y')


# расход в слой стока
df$q_mm <- df$value * 86400 / 1900 / 1000
df <- df %>%
  select(date, prec, mean_temp, En, q_mm)
colnames(df) <- c('DatesR', 'Precip', 'TempMean', 
                  'PotEvap', 'Qobs')
df$DatesR <- as.character(df$DatesR)
df$DatesR <- as.POSIXct(strptime(df$DatesR, 
                                 format = '%Y-%m-%d', 
                                 tz = 'UTC'))

summary(df)

PREP <- PrepGR(DatesR = df$DatesR, Precip = df$Precip, 
               PotEvap = df$PotEvap, Qobs = df$Qobs, 
               TempMean = df$TempMean, 
                HydroModel = "GR4J", CemaNeige = TRUE)
plot(PREP)
plot(PREP, main = "Фактические осадки и сток", 
     xlab = 'Дата', ylab = c('Осадки, мм', 'Расход воды, мм'), 
     plot.na = F)
# png(filename="protva_obs_plot.png")
dev.off()

CAL <- CalGR(PrepGR = PREP, CalCrit = "NSE", 
             WupPer = c("2008-01-01", "2008-12-31"), 
             CalPer = c("2009-01-01", "2014-12-31"))
# png(filename="protva_cal_plot.png")
plot(CAL)
plot(CAL, which = "iter")
dev.off()

SIM <- SimGR(PrepGR = PREP, Param = CAL, EffCrit = "NSE",
             WupPer = c("2008-01-01", "2008-12-31"), 
             SimPer = c("2015-01-01", "2016-07-31"))

# png(filename="protva_sim_plot.png", width = 15, height = 21, units = 'cm', res = 300)
plot(SIM) 
plot(SIM, which='ts')
# dev.off() 


ShinyGR(DatesR = df$DatesR, Precip = df$Precip, 
        PotEvap = df$PotEvap, 
        Qobs = df$Qobs, TempMean = df$TempMean, 
        SimPer = c("2008-01-01", "2014-12-31"), 
        NamesObsBV = 'Жиздра')
