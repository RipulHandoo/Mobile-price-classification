library(tidyverse)
library(caret)
library(nnet)
library(caTools)

f <- read.csv("train.csv")
f$blue <- as.factor(f$blue)
f$dual_sim <- as.factor(f$dual_sim)
f$four_g <- as.factor(f$four_g)
f$three_g <- as.factor(f$three_g)
f$touch_screen <- as.factor(f$touch_screen)
f$wifi <- as.factor(f$wifi)
f$price_range <- as.factor(f$price_range)

# Define 10-fold cross-validation
set.seed(123)
folds <- createFolds(f$price_range, k = 10)

# Define modeling method
model_method <- "multinom"

# Define train control
train_control <- trainControl(method = "cv", index = folds)

# Train the model using 10-fold cross-validation
model <- train(price_range ~ ., data = f, method = model_method, trControl = train_control)

# Summarize the model
print(summary(model))

# Make predictions on test data
predicted <- predict(model, newdata = ts_data)

# Final part of Confusion Matrix and Accuracy
accu <- mean(predicted == ts_data$price_range)
t1 <- table(ts_data$price_range, predicted)
c2 <- confusionMatrix(t1, mode = "everything", positive = "1")
print(c2)

# ROC
s1 <- roc(ts_data$price_range, predicted)
r <- ggroc(s1, colour = "blue")
print(r)

