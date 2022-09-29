Sys.setlocale(category = "LC_ALL", locale = "Russian")
# сложение
2 + 6
# вычитание
6 - 2
# умножение
6 * 2
# деление
6 / 2
# степень
2 ^ 3
# взятие корня
sqrt(9) 
#возведение в степень
3 ^ 2
# присвоение переменной значения: числовое
x <- 2
y = 3.5
x + y 
# текстовое
me <- "Сева"
val <- 10

ls <- 1

# вектор текстовый
students <- c("Вася", "Петя", "Маша", "Коля", "Женя", "Эдуард")
length(students)
class(students)
# числовой вектор
height <- c(180, 185, 170, 182, 168, 198)
length(height)
ls()
rm(students)
rm(list = ls())
# именованный вектор
names(height) <- students
height
names(height)
# обращение к элементу
height[1]
h1 <- height[1 : 3] + 20
height[-1]
height[-1:-3]
height[c(-1,-3)]

# простые статистики
max(height)
min(height)
mean(height)
var(height)
sd(height)
# сортировка
sort(height)
sort(height, decreasing = TRUE)
sort(height, decreasing = TRUE)[1:3]

which(height <= mean(height))

# выборка 
height >= 180
mask_180 <- height >= 180
mask_180
typeof(mask_180)
height[mask_180]
height[height <= 180]

# приведение типов
num <- c(1, 4, 7)
nom <- c(1, 4, '7')
typeof(num)
typeof(nom)
nom <- as.integer(nom)
typeof(nom)

# цикл по элементам вектор
for (i in students){
  print(i)
}
for (k in height) {
  print(k + 100)
}

# apply
sapply(height, FUN = function(x) x+2)

# функция
maxval <- function(x){
  mv <- sort(x, decreasing = T)[1:3]
  print(mv)
} 
maxval(height)

# сделать матрицу 3х3, последовательно заполненную числами от 1 до 9
matrix(1:9)
matrix(1:9, byrow = TRUE, nrow = 3) # byrow - вид заполнения, по строкам или по столбцам
matrix(1:8, byrow = FALSE, nrow = 4)
mat <- matrix(1:9, byrow = TRUE, nrow = 3)
mat[2,3]
matrix(1:12, byrow = FALSE, nrow = 4)

seq(from = 0, to = 1, by = 0.2)
seq(from = 0, to = 1, length.out = 100)

set.seed(1234567)
rnorm(n = 9, mean = 0, sd = 1)
set.seed(1234567)
matrix(rnorm(n = 9, mean = 0, sd = 1), byrow = TRUE, nrow = 3)



