library("ggplot2");
library("dplyr");
library("ggpubr");

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
nobel <- ggplot(d, aes(x = age)) + geom_histogram(binwidth = 1, fill="steelblue", alpha=.6) +
geom_freqpoly(binwidth = 2, color="red", size=1) +
geom_vline(xintercept = m, colour="forestgreen", size=2) +
geom_vline(xintercept = m - 3 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m + 3 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m - 1 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m + 1 * s, colour="forestgreen", size=1) +
ggtitle("wiek laureatów nagrody Nobla 1901--2018 w momencie otrzymania nagrody") +
annotate("text", x=51, y=60, label=sprintf("śr=%.2f s=%.2f sr+/-s=%.2f", m, s, p ), size=4, color="black", hjust = 0)

#ggsave(file="NobelWiek.png")
#library("ggplot2");
#library("dplyr");

d <- read.csv("rwc1999-2019.csv", sep = ';',  header=T, na.string="NA");

fivenum(d$weight)

m <- mean(d$age, na.rm=T)
s <- sd(d$age, na.rm=TRUE)

allN <- nrow(d)
dSS <- d %>% filter(age > m - s & age < m +s )  %>% as.data.frame
allS <- nrow(dSS)
p <- allS/allN * 100

rugby <- ggplot(d, aes(x = age)) + geom_histogram(binwidth = 1, fill="steelblue", alpha=.6) +
  geom_freqpoly(binwidth = 1, color="red", size=1) +
  geom_vline(xintercept = m, colour="forestgreen", size=1) +
  ##
  geom_vline(xintercept = m - 3 * s, colour="forestgreen", size=1) +
  geom_vline(xintercept = m + 3 * s, colour="forestgreen", size=1) +
  geom_vline(xintercept = m - 1 * s, colour="forestgreen", size=1) +
  geom_vline(xintercept = m + 1 * s, colour="forestgreen", size=1) +
  ggtitle("Wiek uczestników Pucharu Światy w Rugby (1999-2019)") +
  annotate("text", x=20, y=380, label=sprintf("śr=%.2f s=%.2f sr+/-s=%.2f", m, s, p ), size=4, color="black", hjust = 0)

##ggsave("Rugby.png")
##library("ggplot2");
##library("dplyr");
###################
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
powiatyA <- ggplot(d, aes(x = areakm)) + geom_histogram(binwidth = 1, fill="steelblue") +
geom_freqpoly(binwidth = 4, color="red", size=1) +
geom_vline(xintercept = m, colour="forestgreen", size=2) +
geom_vline(xintercept = m - 3 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m + 3 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m - 1 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m + 1 * s, colour="forestgreen", size=1) +
ggtitle("Powiaty w Polsce wg powierzchni (2017)") +
xlab("km2 x 10") +
annotate("text", x=100, y=15, label=sprintf("śr=%.2f s=%.2f sr+/-s=%.2f", m, s, p ), size=5, color="black", hjust = 0)

##ggsave("PowiatyAreaAll.png")

d <- d %>% filter(status != "c") %>% as.data.frame

fivenum(d$areakm)
m <- mean(d$areakm)
s <- sd(d$areakm)

allN <- nrow(d)
dSS <- d %>% filter(areakm > m - s & areakm < m +s )  %>% as.data.frame
allS <- nrow(dSS)
p <- allS/allN * 100

## # rozkład
powiatyP <- ggplot(d, aes(x = areakm)) + geom_histogram(binwidth = 1, fill="steelblue") +
geom_freqpoly(binwidth = 4, color="red", size=1) +
geom_vline(xintercept = m, colour="forestgreen", size=2) +
geom_vline(xintercept = m - 3 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m + 3 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m - 1 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m + 1 * s, colour="forestgreen", size=1) +
ggtitle("Powiaty w Polsce wg powierzchni (2017) bez miejskich") +
xlab("km2 x 10") +
annotate("text", x=100, y=15, label=sprintf("śr=%.2f s=%.2f sr+/-s=%.2f", m, s, p ), size=5, color="black", hjust = 0)

##ggsave("PowiatyAreaPow.png")
##

################
d <- read.csv("hotele_caloroczne_PL.csv", sep = ';',  header=T, na.string="NA");

fivenum(d$hotele2017)
m <- mean(d$hotele2017)
s <- sd(d$hotele2017)

allN <- nrow(d)
dSS <- d %>% filter(hotele2017 > m - s & hotele2017 < m +s )  %>% as.data.frame
allS <- nrow(dSS)
p <- allS/allN * 100


## # rozkład
hotele <- ggplot(d, aes(x = hotele2017)) + geom_histogram(binwidth = 1, fill="steelblue") +
##geom_freqpoly(binwidth = 2, color="red", size=1) +
geom_vline(xintercept = m, colour="forestgreen", size=2) +
geom_vline(xintercept = m - 3 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m + 3 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m - 1 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m + 1 * s, colour="forestgreen", size=1) +
ggtitle("Powiaty w Polsce wg liczby hoteli (2017)") +
annotate("text", x=50, y=30, label=sprintf("śr=%.2f s=%.2f sr+/-s=%.2f", m, s, p ), size=4, color="black", hjust = 0)


#ggsave("hotele.png")
#library("ggplot2");
#library("dplyr")

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
pp <- ggplot(d, aes(x = capacity_mw)) + geom_histogram(binwidth = 1) +
  geom_vline(xintercept = m, colour="forestgreen", size=2) +
  geom_vline(xintercept = m - 3 * s, colour="forestgreen", size=1) +
  geom_vline(xintercept = m + 3 * s, colour="forestgreen", size=1) +
  geom_vline(xintercept = m - 1 * s, colour="forestgreen", size=1) +
  geom_vline(xintercept = m + 1 * s, colour="forestgreen", size=1) +
  ggtitle("Moc elektrowni w GER/FRA/GBR 2019", subtitle=sprintf("N=%d (http://datasets.wri.org/dataset/globalpowerplantdatabase)", N)) +
  annotate("text", x=1200, y=100, label=sprintf("śr=%.2f s=%.2f śr+/-s%%=%.2f", m, s, p), size=4, color="red", hjust=0) +
  annotate("text", x=1200, y=60, label=sprintf("min=%.2f max=%.2f me=%.2f", m.min, m.max, m.me), size=4, color="red", hjust=0)

ggarrange(nobel, rugby, ncol = 2, nrow = 1)
ggsave("rozklady61.png", width=12)
ggarrange(powiatyA, powiatyP, ncol = 2, nrow = 1)
ggsave("rozklady62.png", width=12)
ggarrange(hotele, pp, ncol = 2, nrow = 1)
ggsave("rozklady63.png", width=12)

