Sys.setlocale("LC_ALL","Russian")
library(dplyr)
library(lubridate)
library(reshape)
library(ggplot2)

load('data/kama/kama_q.RData')
summary(kama_q)

df1 <- kama_q %>%
  select(date, `76256`, `76553`) 

df2 <- kama_q %>%
  select(date, `76256`, `76553`) %>%
  filter(year(date) == 2009)

df3 <- kama_q %>%
  group_by(year = year(date)) %>%
  summarise(across(colnames(kama_q), mean))
  
df4 <- kama_q %>%
  group_by(year = year(date)) %>%
  summarise(across(where(is.numeric), mean))

kama_long <- melt(kama_q, id.vars = 'date', 
                  variable_name = 'index')  

df5 <- kama_long %>%
  group_by(index) %>%
  summarise(n())

df6 <- kama_long %>%
  mutate(w_km = value * 86400 / 1000000000) 
  
df6 %>%
  group_by(index, year = year(date)) %>%
  summarise(vol = sum(w_km, na.rm = T)) %>%
  ggplot(., aes(x=factor(year), y=vol, fill=factor(index))) + 
  geom_bar(stat='identity') +
  facet_wrap(index~., scales = 'free', ncol = 4)
