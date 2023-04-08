library(tidyverse)
library(caret)
library(nnet)
library(caTools)

# Load the data and preprocess it
f <- read.csv("train.csv")
f$blue <- as.factor(f$blue)
f$dual_sim <- as.factor(f$dual_sim)
f$four_g <- as.factor(f$four_g)

f$three_g <- as.factor(f$three_g)
f$touch_screen <- as.factor(f$touch_screen)
f$wifi <- as.factor(f$wifi)
f$price_range <- as.factor(f$price_range)

# Split the data into train and test sets
set.seed(123)
split <- sample.split(f, SplitRatio = 0.8)
tr_data <- subset(f, split == TRUE)
ts_data <- subset(f, split == FALSE)

# Set up the cross-validation method
ctrl <- trainControl(method = "cv", number = 10)

# Apply multinomial logistic regression with cross-validation
model <- train(price_range ~ ., data = tr_data, method = "multinom", trControl = ctrl)

# Summarize the model
print(summary(model$finalModel))

# Predict on the test set
predicted <- predict(model, ts_data)

# Calculate accuracy
accu <- mean(predicted == ts_data$price_range)
cat("Accuracy:", accu, "\n")

# Calculate confusion matrix
t1 <- table(ts_data$price_range, predicted)
c2 <- confusionMatrix(t1, mode = "everything", positive = "1")
print(c2)

# Calculate ROC
predicted <- as.integer(predicted)
tr_data$price_range <- as.integer(tr_data$price_range)
s1 <- roc(ts_data$price_range, predicted)
s1
r <- ggroc(s1, colour = "blue")
print(r)
