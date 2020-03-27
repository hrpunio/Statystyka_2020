library("ggplot2");
library("dplyr");

d <- read.csv("nobel_laureates3.csv", sep = ';',  header=T, na.string="NA");

fivenum(d$age)
m <- mean(d$age, na.rm=T)
s <- sd(d$age, na.rm=TRUE)
#m

allN <- nrow(d)
dSS <- d %>% filter(age > m - s & age < m +s )  %>% as.data.frame
allS <- nrow(dSS)
p <- allS/allN * 100


## rozkład
ggplot(d, aes(x = age)) + geom_histogram(binwidth = 1, fill="steelblue", alpha=.6) +
geom_freqpoly(binwidth = 2, color="red", size=1) +
geom_vline(xintercept = m, colour="forestgreen", size=2) +
geom_vline(xintercept = m - 3 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m + 3 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m - 1 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m + 1 * s, colour="forestgreen", size=1) +
ggtitle("wiek laureatów nagrody Nobla 1901--2018 w momencie otrzymania nagrody") +
annotate("text", x=65, y=60, label=sprintf("śr=%.2f s=%.2f sr+/-s=%.2f", m, s, p ), size=5, color="black", hjust = 0)

ggsave(file="NobelWiek.png")
