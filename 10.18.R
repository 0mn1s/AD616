library(MultiRNG)
library(Matrix)
library(matrixcalc)
library(tidyverse)
library(fitdistrplus)

m<- matrix(c(1,.9,-.3,
             .9,1,.9,
             -.3,.9,1),
           3,3)

is.positive.semi.definite(m)

m<- nearPD(m, corr=T)
m<- m$mat %>%as.matrix()

m

M<- draw.d.variate.uniform(100,2,matrix(c(1,.6,.6,1),2,2))
cor(M)
df<- data.frame(
  X=qnorm(M[,1], 150,20),
  Y=qexp(M[,2],10)
)
with(df,plot(X,Y))


#example question

hdata<- read_csv('Zappos Historical Sales.csv') %>%select(2:3)
dist_list<- c('norm','lnorm', 'weibull', 'logis', 'cauchy')

c_list<- lapply(X = dist_list,
                FUN = fitdist,
                data = hdata$Cotton)
gofstat(c_list)

f_list<- lapply(X = dist_list,
                FUN = fitdist,
                data = hdata$Flannel)

gofstat(f_list)


c_list[[3]]$estimate

f_list[[4]]$estimate

co<- cor(hdata$Cotton,hdata$Flannel, method='spearman')
cm <- matrix(c(1.0,co,.25,.25,
               co,1.0,.25,0.0,
               .25,.25,1,-.5,
               .25,0.0,-.5,1.0),4,4)
is.positive.semi.definite(cm)

cm<- nearPD(cm, corr=T)
cm<- cm$mat %>%as.matrix()


df<- data.frame(
  c= qweibull(
    p=U[,1]
  )
)
