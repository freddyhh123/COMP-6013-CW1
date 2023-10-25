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

#PCA
pca_data <- prcomp(train_penguins, center = TRUE, scale. = TRUE)
# Plot our "arm bend"
plot(pca_data, type = "l")

#These were picked as they make up 97% of the cumlative proportion
rotate <- pca_data$rotation[, 1:3]
# Rotate the columns to maxamise variance
pca_train <- as.matrix(train_penguins) %*% (rotate)
pca_test <- as.matrix(test_penguins) %*% (rotate)

# testing K 1:10 again
k <- 1:10
# Setup our error lists
k_train_errors <- numeric(length(k))
k_test_errors <- numeric(length(k))

for (i in 1:length(k)) {

    # Train the model for each K value
    knn <- train(
        x = pca_train,
        y = train_label,
        method = "knn",
        tuneGrid = data.frame(k = k[i])
    )

    # Calculate our error rates
    k_train_errors[i] <- 1 - mean(knn$results$Accuracy)
    k_test_errors[i] <- 1 - confusionMatrix(predict(knn, newdata = pca_train), train_label)$overall["Accuracy"]
}

# Select a k value based off the lowest validation error rate
best_k <- which.min(k_val_errors)
# Retrain our model with the best k
knn_best <- train(
    x = pca_train,
    y = train_label,
    method = "knn",
    tuneGrid = data.frame(k = best_k)
)

# Plot the graph, remembering to make sensible boundaries for y
y_min <- min(k_train_errors, k_test_errors)
y_max <- max(k_train_errors, k_test_errors)
plot(k, k_test_errors, type = "l", col = "blue", 
     xlab = "k Value", ylab = "Error Rate (%)", main = "Error rates vs K-value with PCA",ylim = c(y_min, y_max))
lines(k, k_train_errors, col = "red")
legend("topright", legend = c("Test Error", "Train Error"),
       col = c("blue", "red", "green"), lty = 1)
# show best K value
abline(v = best_k, col = "black", lty = 2)

# Run our predictions on test data and calculate F1 
# More indepth descriptions of caluclations in knn.r
test_predictions <- predict(knn_best, pca_test)
matrix <- confusionMatrix(test_predictions, test_label$species)
precision <- mean(matrix$byClass[, "Pos Pred Value"])
recall <- mean(matrix$byClass[, "Recall"])
f1_score <- 2 * (precision * recall) / (precision + recall)