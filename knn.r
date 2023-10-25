library(palmerpenguins)
library(caTools)
library(class)
library(caret)
library(ggfortify)
data(penguins)

penguins <- na.omit(penguins)

set.seed(123)
# 60/40 split
training_rows <- sample(1:nrow(penguins), replace = F, size = nrow(penguins) * 0.6)


# Training set
train_penguins <- penguins[training_rows, -c(2, 8)]
train_label <- train_penguins$species
train_penguins <- scale(train_penguins[c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")])


# Testing set
test_penguins <- penguins[-training_rows, -c(2, 8)]
test_penguins <- scale(test_penguins[c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")])
test_label <- penguins[-training_rows, 1]

# We are going to test k values 1:10
k <- 1:10
# Initialising our arrays for errors
k_train_errors <- numeric(length(k))
k_test_errors <- numeric(length(k))

for (i in 1:length(k)) {
    # Train a model for every value
    knn <- train(
        x = train_penguins,
        y = train_label,
        method = "knn",
        tuneGrid = data.frame(k = k[i])
    )
    # Calculare the train error and test error
    # Average the error rate from this run
    k_train_errors[i] <- 1 - mean(knn$results$Accuracy)
    k_test_errors[i] <- 1 - confusionMatrix(predict(knn, test_penguins), as.factor(test_label$species))$overall["Accuracy"]
}
# Lets get the best K, based off the test error (as we dont have validation error)
best_k <- which.min(k_test_errors)
# Retrain the model with the best value for performance stats
knn_best <- train(
    x = train_penguins,
    y = train_label,
    method = "knn",
    tuneGrid = data.frame(k = best_k)
)

# Plot our graph of all K values vs Error rates (rememering to supply a sensible scale)
y_min <- min(k_train_errors, k_test_errors)
y_max <- max(k_train_errors, k_test_errors)

plot(k, k_test_errors, type = "l", col = "blue", 
     xlab = "k Value", ylab = "Error Rate (%)", main = "Error rates vs K-value",ylim = c(y_min, y_max))
lines(k, k_train_errors, col = "red")
legend("topright", legend = c("Test Error", "Train Error"),
       col = c("blue", "red"), lty = 1)
# Add a line at the best k value
abline(v = best_k, col = "black", lty = 2)

# Running predictions against our test data
test_predictions <- predict(knn_best, test_penguins)
# Performace matrix and F1 calculations
matrix <- confusionMatrix(data = test_predictions, reference = test_label$species)
# Calculate precision by dividing the true positives by (True postivies + False positives)
precision <- mean(matrix$byClass[, "Pos Pred Value"])
#Calculare recall by dividing the true positives by the (True positives + False Negatives)
recall <- mean(matrix$byClass[, "Recall"])
# F1 is calculated as below
f1_score <- 2 * (precision * recall) / (precision + recall)