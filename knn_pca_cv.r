library(palmerpenguins)
library(caTools)
library(class)
library(caret)
library(ggfortify)
data(penguins)

penguins <- na.omit(penguins)

set.seed(122)
training_rows <- sample(1:nrow(penguins), replace = F, size = nrow(penguins) * 0.7)


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

#These were picked as they make up 97% of the cumlative proportion
rotate <- pca_data$rotation[, 1:3]
pca_train <- as.matrix(train_penguins) %*% (rotate)
pca_test <- as.matrix(test_penguins) %*% (rotate)

k <- 1:25
k_train_errors <- numeric(length(k))
k_test_errors <- numeric(length(k))
k_val_errors <- numeric(length(k))

for (i in 1:length(k)) {
    train_fold_errors <- numeric(10)
    val_fold_errors <- numeric(10)
    folds <- createFolds(train_label, k = 10)

    for (fold in 1:length(folds)) {
        train_idx <- unlist(folds[[fold]])
        val_idx <- setdiff(1:length(train_label), train_idx)

        X_train_fold <- pca_train[train_idx,]
        X_val_fold <- pca_train[val_idx,]
        y_train_fold <- train_label[train_idx]
        y_val_fold <- train_label[val_idx]

        knn <- train(
            x = X_train_fold,
            y = y_train_fold,
            method = "knn",
            tuneGrid = data.frame(k = k[i])
        )
        y_train_pred <- predict(knn, X_train_fold)
        y_val_pred <- predict(knn, X_val_fold)

        val_fold_errors[fold] <- (confusionMatrix(y_val_pred, y_val_fold)$overall["Accuracy"])
    }
    k_train_errors[i] <- 1 - mean(knn$results$Accuracy)
    k_test_errors[i] <- 1 - confusionMatrix(predict(knn, pca_test), as.factor(test_label$species))$overall["Accuracy"]
    k_val_errors[i] <- 1 - mean(val_fold_errors)
}
plot(k, k_test_errors, type = "l", col = "blue", 
     xlab = "k Value", ylab = "Error Rate (%)", main = "k-NN Error Rates vs. k Values")
lines(k, k_train_errors, col = "red")
lines(k, k_val_errors, col = "green")
legend("topright", legend = c("Test Error", "Train Error", "Validation Error"),
       col = c("blue", "red", "green"), lty = 1)

best_k <- which.min(k_val_errors) + 1
knn_best <- train(
    x = pca_train,
    y = train_label,
    method = "knn",
    tuneGrid = data.frame(k = best_k)
)

test_predictions <- predict(knn_best, pca_test)
matrix <- confusionMatrix(data = test_predictions, reference = test_label$species)
precision <- mean(matrix$byClass[, "Pos Pred Value"])
recall <- mean(matrix$byClass[, "Recall"])
f1_score <- 2 * (precision * recall) / (precision + recall)