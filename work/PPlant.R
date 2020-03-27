library("ggplot2");
library("dplyr")

d <- read.csv("global_pp_short.csv", sep = ';',  header=T, na.string="NA");
d <- d %>% filter(grepl("GER|FRA|GBR", d$code) )  %>% as.data.frame

N <- nrow(d)

fivenum(d$capacity_mw)
m <- mean(d$capacity_mw, na.rm=T)
m.min <- min(d$capacity_mw, na.rm=T)
m.max <- max(d$capacity_mw, na.rm=T)
m.me <- median(d$capacity_mw, na.rm=T)

s <- sd(d$capacity_mw, na.rm=T)
allN <- nrow(d)
dSS <- d %>% filter(capacity_mw > m - s & capacity_mw < m +s )  %>% as.data.frame
allS <- nrow(dSS)
p <- allS/allN * 100

## rozkład
ggplot(d, aes(x = capacity_mw)) + geom_histogram(binwidth = 1) +
  geom_vline(xintercept = m, colour="forestgreen", size=2) +
  geom_vline(xintercept = m - 3 * s, colour="forestgreen", size=1) +
  geom_vline(xintercept = m + 3 * s, colour="forestgreen", size=1) +
  geom_vline(xintercept = m - 1 * s, colour="forestgreen", size=1) +
  geom_vline(xintercept = m + 1 * s, colour="forestgreen", size=1) +
  ggtitle("Moc elektrowni w GER/FRA/GBR 2019", subtitle=sprintf("N=%d", N)) +
  annotate("text", x=2000, y=100, label=sprintf("śr=%.2f s=%.2f śr+/-s%%=%.2f", m, s, p), size=5, color="red", hjust=0) +
  annotate("text", x=2000, y=60, label=sprintf("min=%.2f max=%.2f me=%.2f", m.min, m.max, m.me), size=5, color="red", hjust=0)

ggsave("PPlant.png")
