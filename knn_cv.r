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

# Testing K 1-10
k <- 1:10
# Setup all our lists including the validation error
k_train_errors <- numeric(length(k))
k_test_errors <- numeric(length(k))
k_val_errors <- numeric(length(k))

# First we loop through every k value
for (i in 1:length(k)) {
    # Setting up some lists to keep track of values within each fold for each k value
    train_fold_errors <- numeric(10)
    val_fold_errors <- numeric(10)
    # Create our folds (10 in this case)
    folds <- createFolds(train_label, k = 10)

    # Now we loop through each fold
    for (fold in 1:length(folds)) {
        # Lets get the training rows for this fold
        train_idx <- unlist(folds[[fold]])
        # And seperate out the validation rows for our calulations later
        val_idx <- setdiff(1:length(train_label), train_idx)

        # Here we are setting the variables for the fold
        # Train fold is our training rows, X and Y are our data and subsequent labels
        X_train_fold <- train_penguins[train_idx,]
        X_val_fold <- train_penguins[val_idx,]
        y_train_fold <- train_label[train_idx]
        y_val_fold <- train_label[val_idx]

        # Train our model on this folds training rows
        knn <- train(
            x = X_train_fold,
            y = y_train_fold,
            method = "knn",
            tuneGrid = data.frame(k = k[i])
        )

        # Make our valdation statistics
        y_val_pred <- predict(knn, X_val_fold)
        # Producing a confusion matrix from our validation rows for this fold
        val_fold_errors[fold] <- (confusionMatrix(y_val_pred, y_val_fold)$overall["Accuracy"])
    }
    # Save all errors to their respective K value, we are calculating error rates by minusing 1
    k_train_errors[i] <- 1 - mean(knn$results$Accuracy)
    # Test error is made from a prediction of our test split, we are testing unseen data
    k_test_errors[i] <- 1 - confusionMatrix(predict(knn, test_penguins), as.factor(test_label$species))$overall["Accuracy"]
    k_val_errors[i] <- 1 - mean(val_fold_errors)
}
# Select our k value by lowest validation error,
# In this case specifically we are adding 1 to prevent overfitting (more detail in report)
best_k <- which.min(k_val_errors) + 1
knn_best <- train(
    x = train_penguins,
    y = train_label,
    method = "knn",
    tuneGrid = data.frame(k = best_k)
)

# Plot our graph with sensible y limitations
y_min <- min(k_train_errors, k_test_errors)
y_max <- max(k_train_errors, k_test_errors)
plot(k, k_test_errors, type = "l", col = "blue", 
     xlab = "k Value", ylab = "Error Rate (%)", main = "Error rates vs K-value with CV", ylim = c(y_min, y_max))
lines(k, k_train_errors, col = "red")
lines(k, k_val_errors, col = "green")
legend("topright", legend = c("Test Error", "Train Error", "Validation Error"),
       col = c("blue", "red", "green"), lty = 1)
# Plot our K value
abline(v = best_k, col = "black", lty = 2)

# Make our predictions and calculate F1, more detail on calculations in knn.r
test_predictions <- predict(knn_best, test_penguins)
matrix <- confusionMatrix(test_predictions,  test_label$species)
precision <- mean(matrix$byClass[, "Pos Pred Value"])
recall <- mean(matrix$byClass[, "Recall"])
f1_score <- 2 * (precision * recall) / (precision + recall)