library("ggplot2");
library("dplyr");

d <- read.csv("rwc1999-2019.csv", sep = ';',  header=T, na.string="NA");

fivenum(d$weight)

m <- mean(d$age, na.rm=T)
s <- sd(d$age, na.rm=TRUE)

allN <- nrow(d)
dSS <- d %>% filter(age > m - s & age < m +s )  %>% as.data.frame
allS <- nrow(dSS)
p <- allS/allN * 100

ggplot(d, aes(x = age)) + geom_histogram(binwidth = 1, fill="steelblue", alpha=.6) +
  geom_freqpoly(binwidth = 1, color="red", size=1) +
  geom_vline(xintercept = m, colour="forestgreen", size=1) +
  ##
  geom_vline(xintercept = m - 3 * s, colour="forestgreen", size=1) +
  geom_vline(xintercept = m + 3 * s, colour="forestgreen", size=1) +
  geom_vline(xintercept = m - 1 * s, colour="forestgreen", size=1) +
  geom_vline(xintercept = m + 1 * s, colour="forestgreen", size=1) +
  ggtitle("Wiek uczestników Pucharu Światy w Rugby (1999-2019)") +
  annotate("text", x=30, y=330, label=sprintf("śr=%.2f s=%.2f sr+/-s=%.2f", m, s, p ), size=5, color="black", hjust = 0)

ggsave("Rugby.png")
