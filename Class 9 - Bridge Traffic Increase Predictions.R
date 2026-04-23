library(tidyverse)

source("mta_daily_prep.R")
source("SetID.R")

brdgs <- c(1, 21, 2, 22, 3, 23, 4, 24, 5, 25, 6, 26, 9, 29, 11, 30)

d_df %>%
  filter(ID %in% brdgs) %>%
  mutate(Year = year(mdy(Date))) %>%
  mutate(ID = setID4(ID)) %>%
  group_by(ID, Year) %>%
  summarize(EZTot = sum(EZPass), Cash = sum(Cash)) %>%
  mutate(TotTraffic = (EZTot + Cash)) %>%
  arrange(Year) -> b_df

b_df %>%
  print(n = 999)
