library(tidyverse)
library(caret)
library(randomForest)
f <- read.csv("train.csv")
head(f)
f$blue <- as.factor(f$blue)
f$dual_sim <- as.factor(f$dual_sim)
f$four_g <- as.factor(f$four_g)
f$three_g <- as.factor(f$three_g)
f$touch_screen <- as.factor(f$touch_screen)
f$wifi <- as.factor(f$wifi)
f$price_range <- as.factor(f$price_range)

# Split the data into training and testing datasets
set.seed(123)
split <- sample.split(f, SplitRatio = 0.8)
tr_data <- subset(f, split == TRUE)
ts_data <- subset(f, split == FALSE)

# Train the Random Forest model
model <- randomForest(price_range ~ ., data = tr_data)

# Make predictions and evaluate performance
predicted <- predict(model, ts_data)
accu <- mean(predicted == ts_data$price_range)
t1 <- table(ts_data$price_range, predicted)
c2 <- confusionMatrix(t1, mode = "everything", positive = "1")
print(c2)

# Get variable importance
importance <- varImp(model)$Overall
importance_df <- data.frame(Feature = row.names(importance), Importance = importance$importance)
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
print(importance_df)
