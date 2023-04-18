library(tidyverse)
library(caret)
library(e1071)
library(caTools)
library(randomForest)

# Read the data
f <- read.csv("train.csv")

# Convert categorical variables to factors
f$blue <- as.factor(f$blue)
f$dual_sim <- as.factor(f$dual_sim)
f$four_g <- as.factor(f$four_g)
f$three_g <- as.factor(f$three_g)
f$touch_screen <- as.factor(f$touch_screen)
f$wifi <- as.factor(f$wifi)
f$price_range <- as.factor(f$price_range)

# Split the data into training and testing sets
set.seed(123)
split <- sample.split(f, SplitRatio = 0.8)
tr_data <- subset(f, split == TRUE)
ts_data <- subset(f, split == FALSE)

# Train a random forest model on the full feature set
rf_model <- randomForest(price_range ~ ram + battery_power + px_width + px_height + mobile_wt + m_dep + sc_h + pc + four_g + three_g + n_cores
, data = tr_data, importance = TRUE)
# Set width and height of plotting device
dev.new(width = 10, height = 8)

# Call varImpPlot() function
varImpPlot(rf_model)


# Extract the top 5 important features
top_5_features <- importance(rf_model)[order(importance(rf_model)[, 3], decreasing = TRUE), ][1:5, 1]

# Subset the training and testing data to the top 5 features only
tr_data_filtered <- tr_data %>% select(one_of(top_5_features))
ts_data_filtered <- ts_data %>% select(one_of(top_5_features))

# Train the random forest model on the filtered data
rf_model_filtered <- randomForest(price_range ~ ., data = tr_data_filtered)

# Make predictions on the filtered test data and evaluate performance
predicted <- predict(rf_model_filtered, ts_data_filtered)
accu <- mean(predicted == ts_data$price_range)
t1 <- table(ts_data$price_range, predicted)
c2 <- confusionMatrix(t1, mode = "everything", positive = "1")
print(c2)

