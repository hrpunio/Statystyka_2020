library("ggplot2");
library("dplyr");

d <- read.csv("hotele_caloroczne_PL.csv", sep = ';',  header=T, na.string="NA");

fivenum(d$hotele2017)
m <- mean(d$hotele2017)
s <- sd(d$hotele2017)

allN <- nrow(d)
dSS <- d %>% filter(hotele2017 > m - s & hotele2017 < m +s )  %>% as.data.frame
allS <- nrow(dSS)
p <- allS/allN * 100


## # rozkład
ggplot(d, aes(x = hotele2017)) + geom_histogram(binwidth = 1, fill="steelblue") +
##geom_freqpoly(binwidth = 2, color="red", size=1) +
geom_vline(xintercept = m, colour="forestgreen", size=2) +
geom_vline(xintercept = m - 3 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m + 3 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m - 1 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m + 1 * s, colour="forestgreen", size=1) +
ggtitle("Powiaty w Polsce wg liczby hoteli (2017)") +
annotate("text", x=60, y=30, label=sprintf("śr=%.2f s=%.2f sr+/-s=%.2f", m, s, p ), size=5, color="black", hjust = 0)


ggsave("hotele.png")

