#2
tr <- 10000
sm <- 40
x1 <- matrix(data = rep(tr.sm, rate = 10), nrow = tr, ncol = sm)
x2 <- apply(x1, 1, cumsum) %>%t()

x3 <- x2<1
x<- rowSums(x3)
hist(x)

f<- function(x)
  dpois(x,10)


x4<- 0:25
y <- f(x)
ggplot()+geom_col(aes(x14,y))
ggplot()+geom_histogram(aex(x), breaks=seq(0,25,1))
max(x)


#3
library(tidyverse)
set.seed(100)
to_rep<- function(){
n<- 10000
pmt<- 115
term<- 240
ben<-1000000
r<- .065/12
df<- data.frame(
  tod<-rweibull(10000,4.5,39)*12,
  laps<-rgeom(n,0.003) %>%pmin(term)
) %>%
  mutate(
    payout= tod<laps,
    pmts=pmin(tod,laps) %>%floor(),
    npv_ben=payout*ben/(1+r)^ceiling(tod),
    npv_prms= pmt*(1-(1+r)^-pmts)/r,
    npv= npv_prms-npv_ben
  )
}


df_list<- replicate(1000,to_rep(), simplify = F)
npvs<- sapply(df_list,function(df) df$npv %>% sum())

mean(npvs)


