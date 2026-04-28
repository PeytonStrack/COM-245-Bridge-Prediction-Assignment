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

future_data$PredTraf <- futurePredictions

future_data %>%
  group_by(ID) %>%
  mutate(pct_increase = (PredTraf - lag(PredTraf)) / lag(PredTraf) * 100) %>%
  arrange(pct_increase) %>%
  filter(Year >= 2026) %>%
  print(n = 999) -> increase_df

ggplot(increase_df, aes(x = Year, y = pct_increase, fill = ID)) +
  geom_col(position = "dodge", color = "#000") +
  scale_fill_manual(values = c("BWB" = "#ffe2f4", "CBB" = "#ffd4ee", "HHB" = "#ffc5e9", "MPB" = "#ffb7e3", "TBM" = "#ffa8dd", "TBX" = "#ff9ad8", "TNB" = "#ff8bd2", "VNB" = "#ff7dcd")) +
  labs(title = "Predicted Percent Traffic Increase Across NYC Bridges", subtitle = "For 2026 to 2030", y = "Percent Traffic Increase") +
  theme(text = element_text(family = "Race", size = 14, color = "#751a46"), panel.background = element_rect(fill = "#f19cbb"), panel.grid = element_line(color = "#e591b0"), plot.background = element_rect(fill = "#de6fa1"), plot.title = element_text(size = 18), axis.text = element_text(color = "#850a42", size = 10))


set.seed(123)

# Split the data into training and testing sets
trainIndex <- createDataPartition(b_df$TotTraffic, p = 0.8, list = FALSE)
trainData <- b_df[trainIndex, ]
testData <- b_df[-trainIndex, ]      

preProc <- preProcess(trainData[,-1], method = c("center", "scale"))
train_scaled <- predict(preProc, trainData)
test_scaled <- predict(preProc, testData)

# Train the Random Forest model
rfModel <- train(TotTraffic ~ ., data = trainData, method = "rf", trControl = trainControl(method = "cv", number = 5))

predictions_rf <- predict(rfModel, newdata = testData)
predictions_rf

rf_df <- data.frame(predictions_rf)

