library("ggplot2");
library("dplyr")

d <- read.csv("kandydaci_ws_2018_3.csv", sep = ';',  header=T, na.string="NA");

fivenum(d$wiek)
m <- mean(d$wiek)

s <- sd(d$wiek)

ggplot(d, aes(x = wiek)) + geom_histogram(bins = nclass.Sturges(d$wiek))
ggplot(d, aes(x = wiek)) + geom_histogram(binwidth = 10)
ggplot(d, aes(x = wiek)) + geom_histogram(binwidth = 5)
## rozkład
ggplot(d, aes(x = wiek)) + geom_histogram(binwidth = 1) +
geom_vline(xintercept = m, colour="forestgreen", size=2) +
geom_vline(xintercept = m - 3 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m + 3 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m - 1 * s, colour="forestgreen", size=1) +
geom_vline(xintercept = m + 1 * s, colour="forestgreen", size=1) +
ggtitle("Wiek kandydatów do sejmików wojewódzkich 2018")



dPiSPO <- d %>% filter(komitet=="PO-N" | komitet=="PiS")   %>% as.data.frame
ggplot(dPiSPO, aes(x = wiek)) + geom_histogram(binwidth = 1) +
geom_freqpoly(binwidth = 2, color="red", size=1) +
ggtitle("PiS/PO")

dPSL <- d %>% filter(komitet=="PSL" )   %>% as.data.frame
ggplot(dPSL, aes(x = wiek)) + geom_histogram(binwidth = 1) +
geom_freqpoly(binwidth = 2, color="red", size=1) +
ggtitle("PSL")


dw <- d %>% filter(nr>3 )  %>% as.data.frame
ggplot(dw, aes(x = wiek)) + geom_histogram(binwidth = 1) +
geom_freqpoly(binwidth = 2, color="red", size=1) +
ggtitle("nr>3")
