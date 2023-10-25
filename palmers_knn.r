library(palmerpenguins)
library(caTools)
library(class)
library(caret)
library(ggfortify)
data(penguins)

penguins <- na.omit(penguins)

set.seed(122)
training_rows <- sample(1:nrow(penguins), replace = F, size = nrow(penguins) * 0.7)
tc <- trainControl(method = "repeatedcv", number = 10,repeats = 5)

# Training set
train_penguins <- penguins[training_rows, -c(2, 8)]
train_label <- train_penguins$species
train_penguins <- scale(train_penguins[c("bill_length_mm","bill_depth_mm","flipper_length_mm","body_mass_g")])

#PCA
pca_data <- prcomp(train_penguins, center = TRUE, scale. = TRUE)

#These were picked as they make up 97% of the cumlative proportion
rotate <- pca_data$rotation[,1:3]
pca_train <- as.matrix(train_penguins) %*% (rotate)


# Testing set
test_penguins <- penguins[-training_rows, -c(2, 8)]
test_penguins <- scale(test_penguins[c("bill_length_mm","bill_depth_mm","flipper_length_mm","body_mass_g")])
test_label <- penguins[-training_rows,1]

pca_test <- as.matrix(test_penguins)%*%(rotate)

kvalue <- train(train_penguins, train_label,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:10),
             trControl  = tc,
             metric     = "Accuracy")

results <- kvalue$results
ver <- 1 - results$Accuracy

test_predictions <- predict(kvalue, test_penguins)

matrix <- confusionMatrix(data = test_predictions, reference = test_label$species)


precision <- mean(matrix$byClass[, "Pos Pred Value"])
recall <- mean(matrix$byClass[, "Recall"])
f1_score <- 2 * (precision * recall) / (precision + recall)

