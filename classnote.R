p <- 0.6
n_miss <- 2

sim_shots <- function(p, nmiss){
  still_not_shots <- TRUE
  shots <- rbinom(1, 1, p)
  while (still_not_shots){
    shots <- c(shots, rbinom(1, 1, p))
    if (all(tail(shots, 2)==0))
        still_not_shots <- FALSE
      }
return(shots)
}


a<- c(1,1,0,0)
sum(tail(a,2))==0
