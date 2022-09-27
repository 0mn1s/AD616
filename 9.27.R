library(tidyverse)
library()


npy <- function(cfs,r){
  sum(cfs/(1+r)^(1:ny))
}
ny <- 5
in_m_size <- 2
on_m_share <- 0.08
rd <- 700
ct <- 500
gm_size <- 1.03
gm_share <- 1.2
u_rev <- (130-40)*12
dscr <- 1.09
df <- data.frame(
  year = 1:ny) %>%
  mutate(
    sales = in_m_size*gm_size^(year-1)*in_m_share*gm_share^(year-1),
    rev = u_rev*sales,
    cum_pft = cumsum(rev)-rd-ct
    )

cfs <- df$rev
cfs


net_present_value <- npy(df$rev, r)-cd-ct
