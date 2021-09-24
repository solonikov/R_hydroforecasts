Sys.setlocale("LC_ALL","Russian")
library(readxl)

# произвольный датафрейм
df <- data.frame(obs = rnorm(10),
                 pred = rnorm(10))
# новый столбец - последовательность дат
newcol <- seq.Date(from = as.Date('2021-09-21'), by = '1 day', length.out = 10)
newcol
# добавляем к дф
df <- cbind(df, newcol)
df
# новые данные - строка
newrow <- c(rnorm(1), rnorm(1), '2021-10-01')
newrow
# добавляем и смотрим на тип данных, в который превратились столбцы дф
df <- rbind(df, newrow)

# правильный список для добавления
newrow <- list(rnorm(1), rnorm(1), '2021-10-01')
df <- rbind(df, newrow)

df$obs <- as.numeric(df$obs)

# читаем данные из excel
df <- read_xlsx('data/oka.xlsx')
summary(df)
head(df, 10)
tail(df, 10)

# отсутствующие данные
df <- df[-4, ]
clean_df <- na.omit(df)
# базовая графика
plot(x = df$dist, y = df$len)
plot(x = df$dist, y = df$area)
plot(x = df$len, y = df$area)
#гистограммы
hist(df$area)
hist(df$area, labels = T, breaks = 10)
hist(df$area, freq = F)
hist(df$area, plot = F)
# факторы
plot(x = df$side, y = df$area)
df$side <- factor(df$side)
levels(df$side)
plot(x = df$side, y = df$area)
# создание факторов разбиением
df$size <- cut(df$area, labels = c('малая','средняя','крупная'), 
               breaks = c(0, 3500, 10000, 100000), ordered_result = T)
plot(x = df$size, y = df$len)
# графика ggplot
library(ggplot2)
ggplot(df, aes(x=size, y=len, col=size)) + geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') 
ggplot(df, aes(x=area, fill=side)) + geom_histogram(binwidth = 10000, position = 'dodge')
ggplot(df, aes(x=len, y=area, col=side)) + geom_point(size=5)
ggplot(df, aes(x=len, y=area)) + geom_point(size=5) + 
  geom_smooth(method = 'lm', formula = y ~ x, se = F)
# линейная аппроксимация
area_model <- lm(data = df, formula = area ~ len)
df$pred_area <- predict(area_model)

ggplot(df, aes(x=len)) + geom_point(aes(y=area, col='Факт'), size=5) +
  geom_line(aes(y=pred_area, col='Модель'), size=5) +
  geom_text(aes(x = 100, y=40000, label=))

coef_a <- as.character(round(coef(area_model)[2], 2))
coef_b <- as.character(round(coef(area_model)[1], 2))
cor_coef <- as.character(round(cor(df$area, df$pred_area), 2))

model_text <- paste("y = ", coef_a, " * x ", coef_b, ", R = ", cor_coef)
model_text

p <- ggplot(df, aes(x=len)) + geom_point(aes(y=area, col='Факт'), size=5) +
  geom_line(aes(y=pred_area, col='Модель'), size=5) +
  geom_text(aes(x = 100, y=40000, label=model_text))
p
ggsave(plot = p, filename = 'linear_model.png', device = 'png', 
       width = 10, height = 8, units = 'in', dpi = 300)

