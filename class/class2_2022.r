my_df <- data.frame(month = month.name, 
                    norm = rnorm(12, 0, 1), 
                    ravn = runif(12, 5, 11), 
                    exp = rexp(12, 2.8)) 
summary(my_df)
my_df$month <- factor(x = my_df$month, 
                    levels = my_df$month, 
                    ordered = T)

my_df$classes <- cut(x = my_df$ravn, 
                     breaks = c(0, 8, 10, 15), 
                     labels = c('low', 'mid', 'high'))
my_df$classes


date <- seq.Date(from = as.Date('2020-06-01'), 
                 to = as.Date('2020-07-01'), by = 'day')
date

stvor <- sample(x = c('Коломна', 'Серпухов', 'Кашира'), 
                size = length(date), replace = T)
stvor
t <- rnorm(n = length(date), mean = 25, sd = 0.5)
h <- runif(length(date))

df <- data.frame(date, stvor, t, h)

df$tfactor <- cut(x = df$t, breaks =  c(0, 20, 25, 30), labels = c('cool', 'ok', 'hot'))

df_long <- melt(df, id.vars = c('date', 'stvor'))

ggplot(df_long, aes(x=date, y=value, col=variable)) + 
  geom_point(size=3) + geom_line(size=2) + 
  facet_grid(stvor~variable, scales = 'free_y')
