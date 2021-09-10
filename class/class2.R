Sys.setlocale("LC_ALL","Russian")
# матрица: продолжение
my_matrix <- matrix(rnorm(n = 12, mean = 0, sd = 1), nrow = 4, byrow = F)
my_matrix
class(my_matrix)
typeof(my_matrix)
dim(my_matrix)
str(my_matrix)
ncol(my_matrix)
nrow(my_matrix)
length(my_matrix)
summary(my_matrix)

my_matrix[3, 1]
my_matrix[,1]
my_matrix[1,]

my_matrix > 1.5
my_matrix[my_matrix > 1.5]

# списки
month.name[7]
month.abb
my_list <- list(month.name, my_matrix, runif(10))
my_list
class(my_list)
typeof(my_list)
str(my_list)
dim(my_list)
summary(my_list)

my_list
my_list[1]
my_list[2]
my_list[3]
my_list[[1]]
my_list[[1]][2]


# датафреймы
my_df <- data.frame(id = 1:30, 
                    a = rnorm(30, 0, 1), 
                    b = rnorm(30, 15, 1), 
                    c = rnorm(30, 30, 1), 
                    d = 'L')
my_df
View(my_df)
class(my_df)
typeof(my_df)
str(my_df)
summary(my_df)

my_df$c <- my_df$c + 4
my_df$c

my_df[,1]
my_df[1,]

my_df$a[5] <- NA
my_df
mean(x = my_df$a, na.rm = T)
is.na(my_df$a)
my_df$a[is.na(my_df$a)] <- 5
my_df

# преобразование из широкого в длинный формат
install.packages("reshape2")
library(reshape2)
my_df_long <- melt(my_df[,-5], id.vars = 'id')
my_df_long

# графика
install.packages("ggplot2")
library(ggplot2)
ggplot(my_df_long, aes(x=value, fill=variable)) + 
  stat_density(alpha = 0.9)

ggplot(my_df_long, aes(x=id, y=value, col=variable)) +
  geom_line() +
  geom_point() 

# факторы
my_df_long$variable <- factor(my_df_long$variable)
str(my_df_long)

# графика
library(ggplot2)
ggplot(my_df_long, aes(x=value, fill=variable)) + stat_density(alpha = 0.9)


my_df_long$variable <- factor(my_df_long$variable, levels = c('b', 'c', 'a'), 
                              ordered = T)
str(my_df_long)

# графика
ggplot(my_df_long, aes(x=value, fill=variable)) + stat_density(alpha = 0.9)
ggplot(my_df_long, aes(x=id, y=value, col=variable)) +
  geom_line() + geom_point()



# запись/чтение в/из файл(ов) с разделителем-запятой
getwd()
write.csv2(my_df, file = 'my_df.csv', sep = '\t', quote = F)

write.csv(my_df, file = 'my_df.csv', quote = FALSE, row.names = F)

new_df <- read.csv('my_df.csv')

my_df_long <- melt(new_df, id.vars = 'id')


# запись/чтение в excel
library(readxl)
library(writexl)

write_xlsx(my_df_long, path = 'my_df_long.xlsx')

df1 <- read_xlsx('my_df_long.xlsx')

my_list <- list(1:10, 1:20, 1:30)
my_list[[2]][3]
