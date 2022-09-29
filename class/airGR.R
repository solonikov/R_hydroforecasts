Sys.setlocale("LC_ALL","Russian")
library(lubridate)
library(ggplot2)
library(dplyr)
library(reshape2)
library(readxl)
library(writexl)
library(airGRteaching)
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
summary(df)

# сохраняем исходные данные
save(df, file = 'data/rivers/my_river_data.RData')

# потенциальное испарение
df$En <- PE_Oudin(JD = yday(df$date), 
                 Temp = df$mean_temp, Lat = rivers[rivers$weather_id == my_ws,]$lat_weather, LatUnit = "deg")

ggplot(melt(df[,-c(2, 4)], id.vars = 'date'), aes(x=date, y=value, col=variable)) + 
         geom_line() + facet_wrap(variable~., ncol = 1, scales = 'free_y') + theme_dark()


# расход в слой стока
df$q_mm <- df$q * 86400 / rivers[rivers$weather_id == my_ws,]$f / 1000
df <- na.omit(df)
df <- df[, c(1, 6, 5, 7, 8)]
colnames(df) <- c('DatesR', 'Precip', 'TempMean', 'PotEvap', 'Qobs')
df$DatesR <- as.character(df$DatesR)
df$DatesR <- as.POSIXct(strptime(df$DatesR, format = '%Y-%m-%d', tz = 'UTC'))

summary(df)

PREP <- PrepGR(DatesR = df$DatesR, Precip = df$Precip, PotEvap = df$PotEvap, Qobs = df$Qobs, TempMean = df$TempMean,
                HydroModel = "GR4J", CemaNeige = F)
# png(filename="protva_obs_plot.png")
plot(PREP)
plot(PREP, main = "Фактические осадки и сток", xlab = 'Дата', ylab = c('Осадки, мм', 'Расход воды, мм'), plot.na = F)
# dev.off() 

CAL <- CalGR(PrepGR = PREP, CalCrit = "NSE",
             WupPer = c("2009-01-01", "2011-12-31"), CalPer = c("2008-01-01", "2008-12-31"))
# png(filename="protva_cal_plot.png")
plot(CAL, which = "perf")
plot(CAL, which = "iter")
# dev.off()

SIM <- SimGR(PrepGR = PREP, Param = CAL, EffCrit = "NSE",
             WupPer = c("2011-01-01", "2011-12-31"), SimPer = c("2012-01-01", "2014-12-31"))

# png(filename="protva_sim_plot.png", width = 15, height = 21, units = 'cm', res = 300)
plot(SIM) 
plot(SIM, which='ts')
# dev.off() 


ShinyGR(DatesR = df$DatesR, Precip = df$Precip, PotEvap = df$PotEvap, 
        Qobs = df$Qobs, TempMean = df$TempMean, SimPer = c("2008-01-01", "2014-12-31"), 
        NamesObsBV = rivers[rivers$hydro_id == my_hp,]$hydro_name)
