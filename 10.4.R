library(fitdistrplus)
library(tidyverse)

m<- fitdist(data = , distr = "norm")

cdfcomp(m)
qqcomp(m) 
# quantiles to quantiles
ppcomp(m)
#prob to prob

gofstat(m)
#godness of fit method


m<- fitdist(data = , distr = "gamma")
dist_list<- c('cauchy', 'gamma', "logis", "lnorm", "norm", "weibull")
m_list<- lapply(x = distlist, FUN = fitdist, data = x)
gofstat(m_list)



h_data<- c(42,45,40,46,43,43,46,42,44,43,47,41,41,45,51,43,45,43,44,48)
m<- fitdist(data = h_data, distr = "nbinom")
m$estimate
gofstat(m)

shift<- function(i){
  m<-fitdist(h_data-35, "poid")
return(c(i,
         m$estimate,
         gofstat(m)$chisqpvalue))
}
pvals <-sapply(x=31:40,
               FUN = shift) %>% t()
lambda<- 


library(e1071)
library(tidyverse)
library(ggplot2)
x <- rdiscrete(2000, probs=c(.25,.50,.24,.01), values=1:4)

a<- rdunif(1000, 200,350)
b<- rdunif(1000, 200,350)
c<- rdunif(1000, 350,450)
d<- rdunif(1000, 450,500)
y<- case_when(x==1~a,
              x==2~b,
              x==3~c,
              x==4~d)

ggplot()+geom_histogram(mapping=aes(x=y,y=..density..),fill="blue",color="black",
                   breaks=seq(0,600,25))

mean(y<500)


qdp<- function(p,probs,ints)
  ifelse(p<probs[i],
         qunif(p/probs[1]),
         qbp(p-probs[1],probs[-1],ints[-1]))

rdp<- function(n,probs,ints)
  q<-runif(n)
  sapply(qdp,q,probs,ints)
  
x<- qbp(n=100000,
        probs=c(.25,.5,.24,.01),
        ints=c(100,200,350,450,500))

