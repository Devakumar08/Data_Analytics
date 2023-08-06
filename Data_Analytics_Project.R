# Load the dataset
dataset <- read.csv("C:/Users/Dev/Desktop/dft.csv")

# Set a random seed for reproducibility
set.seed(123)

# Split the dataset into training and test sets (70% for training)
train_indices <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
train_data <- dataset[train_indices, ]
test_data <- dataset[-train_indices, ]

# Linear Regression

# Create a linear regression model
linear_model <- lm(casualty_severity ~ age_of_casualty + casualty_type, data = train_data)

# Make predictions on the test set
predicted_values_lr <- predict(linear_model, newdata = test_data)

# Calculate the mean squared error (MSE) for linear regression
mse_lr <- mean((predicted_values_lr - test_data$casualty_severity)^2)

# Display the MSE for linear regression
print(paste("Mean Squared Error (MSE) for Linear Regression:", mse_lr))


# Convert "sex_of_casualty" to numeric
train_data$sex_of_casualty <- as.numeric(as.character(train_data$sex_of_casualty))
unique(train_data$sex_of_casualty)


# Logistic Regression
# Perform logistic regression
logistic_model <- glm(sex_of_casualty ~ age_of_casualty + casualty_severity, data = train_data, family = "binomial")

# Make predictions on the test set
predicted_values_logreg <- predict(logistic_model, newdata = test_data, type = "response")

# Convert predicted probabilities to predicted class labels
predicted_labels_logreg <- ifelse(predicted_values_logreg >= 0.5, 1, 0)

# Calculate accuracy for logistic regression
actual_labels <- test_data$sex_of_casualty
accuracy_logreg <- sum(predicted_labels_logreg == actual_labels) / length(actual_labels)

# Display the accuracy for logistic regression
print(paste("Accuracy for Logistic Regression:", accuracy_logreg))

# Select the first 100 predicted values and actual labels
# Select the first 100 predicted values and actual labels
predicted_subset <- predicted_labels_logreg[1:100]
actual_subset <- actual_labels[1:100]

# Create a data frame to store predicted and actual values
comparison_df <- data.frame(Actual = actual_subset, Predicted = predicted_subset)

# Create a scatter plot to visualize the comparison
plot(1:100, comparison_df$Actual, pch = 16, col = "blue",
     xlab = "Data Point", ylab = "Sex of Casualty",
     ylim = c(0, 1), main = "Actual vs Predicted (Sex of Casualty)")

points(1:100, comparison_df$Predicted, pch = 16, col = "red")

legend("topleft", legend = c("Actual", "Predicted"), col = c("blue", "red"), pch = 16)

