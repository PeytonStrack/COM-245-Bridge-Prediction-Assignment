library(tidyverse)
library(sysfonts)
library(showtext)
library(caret)
library(lattice)
library(randomForest)

font_add_google("Racing Sans One", family = "Race")
showtext_auto()

source("mta_daily_prep.R")
source("SetID.R")


brdgs <- c(1, 21, 2, 22, 3, 23, 4, 24, 5, 25, 6, 26, 9, 29, 11, 30)

d_df %>%
  filter(ID %in% brdgs) %>%
  mutate(Weekday = wday(mdy(Date)), Year = year(mdy(Date)), ID = setID4(ID), TotTraffic = EZPass + Cash) %>%
  filter(Year <= 2024 & Year >= 2017) %>%
  filter(Weekday %in% 2:6) %>%
  group_by(ID, Year) %>%
  summarise(TotTraffic = sum(TotTraffic), .groups = "drop") -> b_df

b_df$ID <- as.factor(b_df$ID)

print(b_df, n = 999)

model <- lm(TotTraffic ~ Year * ID, data = b_df)

predictions <- predict(model, newdata = b_df)


future_data <- read.csv("Bridges and Future Dates.csv")

future_data$ID <- factor(future_data$ID, levels = levels(b_df$ID))
future_data$Year <- as.numeric(future_data$Year)

futurePredictions <- predict(model, newdata = future_data)

future_data$PredTraf <- futurePredictions

future_data %>%
  arrange(ID, Year) %>%
  group_by(ID) %>%
  mutate(pct_increase = (PredTraf / lag(PredTraf) - 1) * 100) %>%
  filter(Year >= 2026) -> increase_df

print(increase_df, n = 999)

ggplot(increase_df, aes(x = Year, y = pct_increase, fill = ID)) +
  geom_col(position = "dodge", color = "#000") +
  scale_fill_manual(values = c("BWB" = "#ffe2f4", "CBB" = "#ffd4ee", "HHB" = "#ffc5e9", "MPB" = "#ffb7e3", "TBM" = "#ffa8dd", "TBX" = "#ff9ad8", "TNB" = "#ff8bd2", "VNB" = "#ff7dcd")) +
  labs(title = "Predicted Percent Traffic Increase Across NYC Bridges", subtitle = "For 2026 to 2030", y = "Percent Traffic Increase") +
  theme(text = element_text(family = "Race", size = 14, color = "#751a46"), panel.background = element_rect(fill = "#f19cbb"), panel.grid = element_line(color = "#e591b0"), plot.background = element_rect(fill = "#de6fa1"), plot.title = element_text(size = 18), axis.text = element_text(color = "#850a42", size = 10))

