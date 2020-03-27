library("ggplot2");
library("dplyr")

d <- read.csv("ws2018_k_CC.csv", sep = ';',  header=T, na.string="NA");

fivenum(d$lkw)
m <- mean(d$lkw)

s <- sd(d$lkw)
allN <- nrow(d)
dSS <- d %>% filter(lkw > m - s & lkw < m +s )  %>% as.data.frame
allS <- nrow(dSS)
p <- allS/allN * 100

## rozkład
ggplot(d, aes(x = lkw)) + geom_histogram(binwidth = 1) +
  geom_vline(xintercept = m, colour="forestgreen", size=2) +
  geom_vline(xintercept = m - 3 * s, colour="forestgreen", size=1) +
  geom_vline(xintercept = m + 3 * s, colour="forestgreen", size=1) +
  geom_vline(xintercept = m - 1 * s, colour="forestgreen", size=1) +
  geom_vline(xintercept = m + 1 * s, colour="forestgreen", size=1) +
  ggtitle("Liczba głosów/obwód do sejmików wojewódzkich 2018") +
  annotate("text", x=2, y=100, label=sprintf("śr=%.2f os=%.2f śr+/-os%%=%.2f", m, s, p), size=6, color="red")


