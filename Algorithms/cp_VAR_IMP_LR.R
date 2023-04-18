library(tidyverse)
library(caret)
library(nnet)
library(e1071)
library(caTools)
library(randomForest)
# Load data and convert columns to factors
f <- read.csv("train.csv")
f$blue <- as.factor(f$blue)
f$dual_sim <- as.factor(f$dual_sim)
f$four_g <- as.factor(f$four_g)
f$three_g <- as.factor(f$three_g)
f$touch_screen <- as.factor(f$touch_screen)
f$wifi <- as.factor(f$wifi)
f$price_range <- as.factor(f$price_range)

# Split data into training and test sets
set.seed(123)
split <- sample.split(f, SplitRatio = 0.8)
tr_data <- subset(f, split == TRUE)
ts_data <- subset(f, split == FALSE)
random_forest_model <- randomForest(price_range ~ ., data = tr_data, importance = TRUE)

# Fit model using only the top 5 important features based on varImp
imp <- varImp(random_forest_model)
top_features <- row.names(imp)[1:10]
model <- nnet::multinom(price_range ~ ram + battery_power + px_height + px_width + mobile_wt + m_dep + sc_h + pc + four_g + three_g , data = tr_data)

# Summarize the model
print(summary(model))

# Make predictions on test set
predicted <- model %>% predict(ts_data)

# Calculate accuracy and confusion matrix
accu <- mean(predicted == ts_data$price_range)
t1 <- table(ts_data$price_range, predicted)
c2 <- confusionMatrix(t1, mode = "everything", positive = "1")
print(c2)

# Plot ROC curve
predicted <- as.integer(predicted)
tr_data$price_range <- as.integer(tr_data$price_range)
s1 <- roc(ts_data$price_range, predicted)
s1
r <- ggroc(s1, colour = "blue")
print(r)
