library("ggplot2");

d <- read.csv("PL_powiaty_2017.csv", sep = ';',  header=T, na.string="NA");

fivenum(d$przychodMF)
mean(d$przychodMF)

ggplot(d, aes(x = przychodMF)) + geom_histogram(bins = nclass.Sturges(d$przychodMF))
ggplot(d, aes(x = przychodMF)) + geom_histogram(binwidth = 40)

