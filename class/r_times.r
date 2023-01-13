# время в R ---- обратите внимание на сообщения 
# в консоли после каждой строки кода
Sys.time()
now <- Sys.time()
now

class(now)
unclass(now)

date1 <- as.POSIXct(now, origin="1970-01-01", 
                    tz="Europe/Moscow")
date2 <- as.POSIXlt(now, origin="1970-01-01", 
                    tz="Europe/Moscow")
date1; date2
as.POSIXlt(now, origin="1970-01-01", tz="GMT")

class(date1)
class(date2)

unclass(date1)
unclass(date2)

as.Date(date1)
as.Date(date2)

text_date <- '2022-11-18'
date_from_text <- as.Date(text_date)
date_from_text
class(date_from_text)
unclass(date_from_text)
date_from_text - as.Date("1970-01-01")

unformatted_date <- '18.11.2022' 
as.Date(unformatted_date)
formatted_date <- strptime(unformatted_date, 
                           format = "%d.%m.%Y")
?strptime
formatted_date
class(formatted_date)
unclass(formatted_date)
format(formatted_date, 
       format = "%d %B %Y, день недели: %A")

ISOdate(2022, 11, 18)
class(ISOdate(2022, 11, 18))
library(lubridate)
make_date(2022, 11, 18)
class(make_date(2022, 11, 18))

year(now)
month(now)
day(now)
yday(now)
format(now, "%d")

df1 <- data.frame(date = seq.Date(from = as.Date('2022-01-01'), 
                                  length.out = 12, 
                                  by = 'month'), 
                  q1 = rnorm(12))
df2 <- data.frame(date = seq.POSIXt(as.POSIXct('2022-01-01', 
                                               tz = "GMT"),
                                    length.out = 12, 
                                    by = 'month'), 
                  q2 = rnorm(12))
df <- merge(df1, df2, by = 'date')

df2$date <- as.Date(df2$date)

df <- merge(df1, df2, by = 'date')
