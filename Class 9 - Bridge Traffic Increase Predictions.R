library(tidyverse)

source("mta_daily_prep.R")
source("SetID.R")

brdgs <- c(1, 21, 2, 22, 3, 23, 4, 24, 5, 25, 6, 26, 9, 29, 11, 30)

d_df %>%
  filter(ID %in% brdgs) %>%
  mutate(Weekday = wday(mdy(Date))) %>%
  filter(Weekday %in% c(2:6)) %>%
  mutate(Year = year(mdy(Date))) %>%
  mutate(ID = setID4(ID)) %>%
  group_by(ID, Year) %>%
  summarize(EZTot = sum(EZPass), Cash = sum(Cash)) %>%
  mutate(TotTraffic = (EZTot + Cash)) %>%
  arrange(Year) -> b_df

b_df %>%
  print(n = 999)

model <- lm(TotTraffic ~ Year + ID, data = b_df)

predictions <- predict(model, newdata = b_df)


future_data <- read.csv("Bridges and Future Dates.csv")
futurePredictions <- predict(model, newdata = future_data)
