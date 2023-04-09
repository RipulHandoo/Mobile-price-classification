library(tidyverse)
library(caret)
library(e1071)
library(caTools)

# Read the data
f <- read.csv("train.csv")

# Convert categorical variables to factors
f$blue <- as.factor(f$blue)
f$dual_sim <- as.factor(f$dual_sim)
f$four_g <- as.factor(f$four_g)
f$three_g<- as.factor(f$three_g)
f$touch_screen <- as.factor(f$touch_screen)
f$wifi <- as.factor(f$wifi)
f$price_range <- as.factor(f$price_range)

# Split the data into training and testing sets
set.seed(123)
split <- sample.split(f, SplitRatio = 0.8)
tr_data <- subset(f, split == TRUE)
ts_data <- subset(f, split == FALSE)

# Compute the correlation matrix
corr_matrix <- cor(tr_data %>% select_if(is.numeric))

# Find highly correlated pairs of variables (correlation coefficient > 0.7)
high_corr <- findCorrelation(corr_matrix, cutoff = 0.7, verbose = FALSE)

# Identify the names of the highly correlated variables
high_corr_vars <- colnames(tr_data)[high_corr]

# Remove the highly correlated variables from the dataset
tr_data_filtered <- tr_data %>% select(-one_of(high_corr_vars))

# Train the SVM model using a radial kernel
svm_model <- svm(price_range ~ ., data = tr_data_filtered, kernel = "radial")

# Make predictions on the test data and evaluate performance
predicted <- predict(svm_model, ts_data)
accu <- mean(predicted == ts_data$price_range)
t1 <- table(ts_data$price_range, predicted)
c2 <- confusionMatrix(t1, mode = "everything", positive = "1")
print(c2)
