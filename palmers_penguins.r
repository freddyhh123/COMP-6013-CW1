library(palmerpenguins)
library(caTools)
library(class)
data(penguins)

# any(is.na(penguins))
# empty_rows <- penguins[rowSums(is.na(penguins)) > 0,]

penguins <- na.omit(penguins)
# bar_h <- c(
#    sum(penguins$species == "Adelie"),
#    sum(penguins$species == "Gentoo"),
#    sum(penguins$species == "Chinstrap")
# )
# bar_w <- c("Adélie", "Gentoo", "Chinstrap")
# png(file = "species_chart.png")
# barplot(bar_h, names.arg = bar_w, xlab = "Species", ylab = "Count", col = "lightblue", main="Bar plot of the distribution of penguin species" )
# dev.off()


set.seed(140)
training_rows <- sample(1:nrow(penguins), replace = F, size = nrow(penguins) * 0.7)

# Training set
train_penguins <- penguins[training_rows, -c(2, 8)]
train_penguins$species <- as.numeric(train_penguins$species)
train_penguins$sex <- as.numeric(train_penguins$sex)
train_label <- train_penguins$species

# Testing set
test_penguins <- penguins[-training_rows, -c(2, 8)]
test_penguins$species <- as.numeric(test_penguins$species)
test_penguins$sex <- as.numeric(test_penguins$sex)
test_label <- test_penguins$species

knn.15 <- knn(train = train_penguins, test = test_penguins, cl = train_label, k = 15)
knn.10 <- knn(train = train_penguins, test = test_penguins, cl = train_label, k = 10)
knn.1 <- knn(train = train_penguins, test = test_penguins, cl = train_label, k = 1)

i <- 1
k.optm <- 1

for (i in 1:30) {
  knn.mod <- knn(train = train_penguins, test = test_penguins, cl = train_label, k = i)
  k.optm[i] <- 100 * sum(test_label == knn.mod) / NROW(test_label)
  k <- i
  cat(k, "=", k.optm[i], "")
}

plot(k.optm, type = "b", xlab = "K-Value", ylab = "Accuracy Percentage (%)")
