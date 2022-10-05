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


