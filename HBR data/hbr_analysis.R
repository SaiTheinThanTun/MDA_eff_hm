#HBR analysis
library(ggplot2)
setwd("~/OneDrive/MORU/Projects/TCE_MDA effect/MDA_eff_hm/HBR data/")
hbr <- read.csv("hbr_20170914.csv")
names(hbr) 
#1 "Village"   2"Survey"    3"Site"      4"HBR"       5"survMaggr" 6"CatchSite"

head(hbr)

plot(hbr$CatchSite,hbr$HBR)
hbr[grepl("TOT" ,hbr$CatchSite),]

png("HBR_overtime_TOT.png")
ggplot(hbr[hbr$Village=="TOT",], aes(x=Survey, y=HBR,col=CatchSite))+
  geom_line()
dev.off()

TOT <- hbr[hbr$Village=="TOT",]
max(TOT$HBR)

ggplot(TOT,aes(x=Survey, y=HBR))+
  geom_boxplot(aes(group=Survey))
