library("ggplot2");
library("dplyr");

d <- read.csv("PL_powiaty_2017.csv", sep = ';',  header=T, na.string="NA");

d$areakm <- floor(d$areakm / 10 )
fivenum(d$areakm)
m <- mean(d$areakm)
s <- sd(d$areakm)

allN <- nrow(d)
dSS <- d %>% filter(areakm > m - s & areakm < m +s )  %>% as.data.frame
allS <- nrow(dSS)
p <- allS/allN * 100

## # rozkład
ggplot(d, aes(x = areakm)) + geom_histogram(binwidth = 1, fill="steelblue") +
geom_freqpoly(binwidth = 4, color="red", size=1) +
geom_vline(xintercept = m, colour="forestgreen", size=2) +
geom_vline(xintercept = m - 3 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m + 3 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m - 1 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m + 1 * s, colour="forestgreen", size=1) +
ggtitle("Powiaty w Polsce wg powierzchni (2017)") +
xlab("km2 x 10") +
annotate("text", x=100, y=15, label=sprintf("śr=%.2f s=%.2f sr+/-s=%.2f", m, s, p ), size=5, color="black", hjust = 0)


ggsave("PowiatyAreaAll.png")

d <- d %>% filter(status != "c") %>% as.data.frame
