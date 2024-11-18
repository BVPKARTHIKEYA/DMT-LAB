
install.packages('caret', dependencies = TRUE)
# Load necessary libraries
library(e1071)      # For Naive Bayes
library(caret)      # For confusionMatrix

# Load the Iris dataset
data(iris)

# Display the first few rows of the dataset
cat("Iris Dataset:\n")
print(head(iris))

# Split the dataset into features and target variable
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(iris), size = 0.8 * nrow(iris))
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

# Initialize the Gaussian Naive Bayes classifier
gnb_model <- naiveBayes(Species ~ ., data = train_data)

# Make predictions
y_pred <- predict(gnb_model, test_data)

# Evaluate the model
accuracy <- mean(y_pred == test_data$Species)
conf_matrix <- table(test_data$Species, y_pred)
class_report <- confusionMatrix(conf_matrix)

# Print results
cat("\nAccuracy of Naive Bayes classifier:", accuracy, "\n\n")
cat("Confusion Matrix:\n")
print(conf_matrix)
cat("\nClassification Report:\n")
print(class_report)

