library(ggplot2)
library(dplyr)
library(tidyr)
options(dplyr.print_max = 1e9)

fbs <- read.csv("faostat_meat_old.csv", sep = ';',  header=T, na.string="NA");
## Remove unit
fbs$unit <- NULL

str(fbs)

## zamień format wąski na szeroki
## (daje niepoprawny wynik jeżeli nie usuniemy kolumny unit)
fbsx <- fbs %>% spread(var, value, drop=T)
## zamień FatSQ z gramów/dzień w kg/rok
fbsx$FatSQ <- fbsx$FatSQ * 365 / 1000
str(fbsx)
##head(fbsx, 20)

## Tylko dla PL/DK/US
fbsx.PL <- fbsx %>% filter(country == "Poland") %>% as.data.frame
fbsx.DK <- fbsx %>% filter(country == "Denmark") %>% as.data.frame
fbsx.US <- fbsx %>% filter(country == "United States of America") %>% as.data.frame

## Tylko dla AU/NZ/JP
fbsx.AU <- fbsx %>% filter(country == "Australia") %>% as.data.frame
fbsx.NZ <- fbsx %>% filter(country == "New Zealand") %>% as.data.frame
fbsx.JP <- fbsx %>% filter(country == "Japan") %>% as.data.frame

str(fbsx.PL)
##fbsx.PL$FoodSQ

cor(fbsx.PL$FatSQ, fbsx.PL$FoodSQ, use = "complete.obs")
cor(fbsx.DK$FatSQ, fbsx.DK$FoodSQ, use = "complete.obs")
cor(fbsx.US$FatSQ, fbsx.US$FoodSQ, use = "complete.obs")
cor(fbsx.AU$FatSQ, fbsx.AU$FoodSQ, use = "complete.obs")

fbsx.AU$FatSQ

fbsx.AU$FoodSQ

cor(fbsx.NZ$FatSQ, fbsx.NZ$FoodSQ, use = "complete.obs")
cor(fbsx.JP$FatSQ, fbsx.JP$FoodSQ, use = "complete.obs")
##
cor(fbsx.PL$ProteinSQ, fbsx.PL$FoodSQ, use = "complete.obs")
cor(fbsx.DK$ProteinSQ, fbsx.DK$FoodSQ, use = "complete.obs")
cor(fbsx.US$ProteinSQ, fbsx.US$FoodSQ, use = "complete.obs")
cor(fbsx.AU$ProteinSQ, fbsx.AU$FoodSQ, use = "complete.obs")
cor(fbsx.NZ$ProteinSQ, fbsx.NZ$FoodSQ, use = "complete.obs")
cor(fbsx.JP$ProteinSQ, fbsx.JP$FoodSQ, use = "complete.obs")

summary(fbsx.PL$FatSQ); summary(fbsx.PL$FoodSQ); summary(fbsx.PL$ProteinSQ);
summary(fbsx.US$FatSQ); summary(fbsx.US$FoodSQ); summary(fbsx.US$ProteinSQ); 
summary(fbsx.DK$FatSQ); summary(fbsx.DK$FoodSQ); summary(fbsx.DK$ProteinSQ)
summary(fbsx.AU$FatSQ); summary(fbsx.AU$FoodSQ); summary(fbsx.AU$ProteinSQ);
summary(fbsx.US$FatNZ); summary(fbsx.NZ$FoodSQ); summary(fbsx.NZ$ProteinSQ); 
summary(fbsx.DK$FatJP); summary(fbsx.JP$FoodSQ); summary(fbsx.JP$ProteinSQ);

## Wykres

p1 <- ggplot() +
    ggtitle("FAO: mięso/proteiny (kropkowana)/tłuszcz (przerywana) per capita/rok [kg]") +
    ##
    geom_line( data = fbsx.PL, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =FoodSQ, colour='PL' ), alpha=.5, size=.8) +
    geom_line( data = fbsx.US, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =FoodSQ, colour='US' ), alpha=.5, size=.8) +
    geom_line( data = fbsx.DK, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =FoodSQ, colour='DK' ), alpha=.5, size=.8) +
    ##
    geom_line( data = fbsx.PL, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =FatSQ, colour='PL'), linetype = "dashed", alpha=.5, size=.8) +
    geom_line( data = fbsx.US, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =FatSQ, colour='US'), linetype = "dashed", alpha=.5, size=.8) +
    geom_line( data = fbsx.DK, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =FatSQ, colour='DK'), linetype = "dashed", alpha=.5, size=.8) +
    ##
    geom_line( data = fbsx.PL, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =ProteinSQ, colour='PL'), linetype = "dotted", alpha=.5, size=.8) +
    geom_line( data = fbsx.US, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =ProteinSQ, colour='US'), linetype = "dotted", alpha=.5, size=.8) +
    geom_line( data = fbsx.DK, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =ProteinSQ, colour='DK'), linetype = "dotted", alpha=.5, size=.8) +
    ##
    ylab(label="kg") +
    xlab(label="") +
    labs(colour = "") +
    theme(legend.position="top") +
    theme(legend.text=element_text(size=12));

p2 <- ggplot() +
    ggtitle("FAO: mięso/proteiny (kropkowana)/tłuszcz (przerywana) per capita/rok [kg]") +
    ##
    geom_line( data = fbsx.AU, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =FoodSQ, colour='AU' ), alpha=.5, size=.8) +
    geom_line( data = fbsx.NZ, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =FoodSQ, colour='NZ' ), alpha=.5, size=.8) +
    geom_line( data = fbsx.JP, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =FoodSQ, colour='JP' ), alpha=.5, size=.8) +
    ##
    geom_line( data = fbsx.AU, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =FatSQ, colour='AU'), linetype = "dashed", alpha=.5, size=.8) +
    geom_line( data = fbsx.NZ, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =FatSQ, colour='NZ'), linetype = "dashed", alpha=.5, size=.8) +
    geom_line( data = fbsx.JP, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =FatSQ, colour='JP'), linetype = "dashed", alpha=.5, size=.8) +
    ##
    geom_line( data = fbsx.AU, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =ProteinSQ, colour='AU'), linetype = "dotted", alpha=.5, size=.8) +
    geom_line( data = fbsx.NZ, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =ProteinSQ, colour='NZ'), linetype = "dotted", alpha=.5, size=.8) +
    geom_line( data = fbsx.JP, mapping = aes(x=as.Date(as.factor(year), format="%Y"), y =ProteinSQ, colour='JP'), linetype = "dotted", alpha=.5, size=.8) +
    ##
    ylab(label="kg") +
    xlab(label="") +
    labs(colour = "") +
    theme(legend.position="top") +
    theme(legend.text=element_text(size=12));

p3 <- ggplot() +
    ggtitle("FAO: mięso vs tłuszcz er capita/rok [kg]/AU)") +
    geom_point( data = fbsx.AU, mapping = aes(x=FatSQ, y =FoodSQ, colour='fat' ), alpha=.5, size=2) +
    geom_smooth(data = fbsx.AU, mapping = aes(x=FatSQ, y =FoodSQ), method=lm) +
    ##
    ylab(label="kg") +
    xlab(label="") +
    labs(colour = "") +
    theme(legend.position="top") +
    theme(legend.text=element_text(size=12));
p3p <- ggplot() +
    ggtitle("FAO: mięso vs protein / protein per capita/rok [kg]/AU)") +
    geom_point( data = fbsx.AU, mapping = aes(x=ProteinSQ, y =FoodSQ, colour='protein' ), alpha=.5, size=2) +
    geom_smooth(data = fbsx.AU, mapping = aes(x=ProteinSQ, y =FoodSQ), method=lm) +
    ##
    ylab(label="kg") +
    xlab(label="") +
    labs(colour = "") +
    theme(legend.position="top") +
    theme(legend.text=element_text(size=12));

p4 <- ggplot() +
    ggtitle("FAO: mięso vs tłuszcz per capita/rok [kg]/US)") +
    geom_point( data = fbsx.US, mapping = aes(x=FatSQ, y =FoodSQ, colour='fat' ), alpha=.5, size=2) +
    geom_smooth(data = fbsx.US, mapping = aes(x=FatSQ, y =FoodSQ), method=lm) +
    ##
    ylab(label="kg") +
    xlab(label="") +
    labs(colour = "") +
    theme(legend.position="top") +
    theme(legend.text=element_text(size=12));

p4p <- ggplot() +
    ggtitle("FAO: mięso vs protein / protein per capita/rok [kg]/US)") +
    geom_point( data = fbsx.US, mapping = aes(x=ProteinSQ, y =FoodSQ, colour='protein' ), alpha=.5, size=2) +
    geom_smooth(data = fbsx.US, mapping = aes(x=ProteinSQ, y =FoodSQ), method=lm) +
    geom_smooth(method=lm) +
    ##
    ylab(label="kg") +
    xlab(label="") +
    labs(colour = "") +
    theme(legend.position="top") +
    theme(legend.text=element_text(size=12));

p5 <- ggplot() +
    ggtitle("FAO: mięso vs tłuszcz / protein per capita/rok [kg]/PL)") +
    geom_point( data = fbsx.PL, mapping = aes(x=FatSQ, y =FoodSQ, colour='fat' ), alpha=.5, size=2) +
    geom_smooth(data = fbsx.PL, mapping = aes(x=FatSQ, y =FoodSQ), method=lm) +
    ##
    ylab(label="kg") +
    xlab(label="") +
    labs(colour = "") +
    theme(legend.position="top") +
    theme(legend.text=element_text(size=12));

p5p <- ggplot() +
    ggtitle("FAO: mięso vs tłuszcz / protein per capita/rok [kg]/PL)") +
    geom_point( data = fbsx.PL, mapping = aes(x=ProteinSQ, y =FoodSQ, colour='protein' ), alpha=.5, size=2) +
    geom_smooth(data = fbsx.PL, mapping = aes(x=ProteinSQ, y =FoodSQ), method=lm) +
    geom_smooth(method=lm) +
    ##
    ylab(label="kg") +
    xlab(label="") +
    labs(colour = "") +
    theme(legend.position="top") +
    theme(legend.text=element_text(size=12));

p1

p2

p3
p3p

p4 
p4p

p5
p5p
