library(tidyverse)
library()


npy <- function({})
  
ny <- 5
in_m_size <- 2
on_m_share <- 0.08
rd <- 700
ct <- 500
gm_size <- 1.03
gm_share <- 1.2
u_rev <- (140-30)*12
dscr <- 1.09
df <- data.frame(
  year = 1:ny) %>%
  mutate(
    m_size = in_m_size*gm_size^(year-1),
    m_share = in_m_share*gm_share^(year-1)
    )