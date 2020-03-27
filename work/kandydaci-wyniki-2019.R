## Ocena zależności pomiędzy odsetkiem głosów nieważnych
## a uzyskanym wynikiem w wyborach samorządowych do sejmików wojewódzkich 2014
## 
require (ggplot2)
library(dplyr)

#######################################
## Dane ###############################
#######################################
d <- read.csv("kandydaci-wyniki-2019.csv",  sep = ';',  header=T, na.string="NA");

fivenum(d$glosy, na.rm=T)
m <- mean(d$glosy, na.rm=T)
s <- sd(d$glosy, na.rm=T)

allN <- nrow(d)
dSS <- d %>% filter(glosy > m - s & glosy < m +s )  %>% as.data.frame
dLL <- d %>% filter(glosy < m )  %>% as.data.frame
allS <- nrow(dSS)
allL <- nrow(dLL)
p <- allS/allN * 100
pL <- allL/allN * 100

d <- d %>% filter(glosy < 100000 )  %>% as.data.frame

ggplot(d, aes(x=glosy)) +
  ggtitle("Wybory 2019: Kandydaci wg liczby głosów (bez rekordzistów)") +
  xlab(label="x") + ylab(label="N") +
  ##scale_y_continuous(limits = c(0, 440)) +
  geom_histogram(binwidth=100, fill="steelblue",  alpha=.5) +
  geom_vline(xintercept = m, colour="forestgreen", size=1) +
  annotate("text", x=50000, y=100, label=sprintf("śr=%.2f os=%.2f śr+/-os%%=%.2f (%.2f)", m, s, p, pL), size=5, color="red")

ggsave(file="Kandydaci.png", width=10)
