library("ggplot2");

d <- read.csv("nobel_laureates3.csv", sep = ';',  header=T, na.string="NA");

fivenum(d$age)
mean(d$age)

ggplot(d, aes(x = age)) + geom_histogram(bins = nclass.Sturges(d$age))
ggplot(d, aes(x = age)) + geom_histogram(binwidth = 10)
ggplot(d, aes(x = age)) + geom_histogram(binwidth = 5)
## rozkład
ggplot(d, aes(x = age)) + geom_histogram(binwidth = 1)

