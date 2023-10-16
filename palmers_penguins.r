library(palmerpenguins)
library(caTools)
library(class)
library(caret)
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


set.seed(122)
training_rows <- sample(1:nrow(penguins), replace = F, size = nrow(penguins) * 0.7)
tc <- trainControl(method = "cv", number = 10)

# Training set
train_penguins <- penguins[training_rows, -c(2, 8)]
train_label <- train_penguins$species
train_penguins <- scale(train_penguins[c("bill_length_mm","bill_depth_mm","flipper_length_mm","body_mass_g")])


# Testing set
test_penguins <- penguins[-training_rows, -c(2, 8)]
test_penguins <- scale(test_penguins[c("bill_length_mm","bill_depth_mm","flipper_length_mm","body_mass_g")])
test_label <- penguins[-training_rows,1]

i <- 1
k.optm <- 1


kvalue <- train(train_penguins, train_label,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:30),
             trControl  = tc,
             metric     = "Accuracy")


#for (i in 1:30) {
#  knn.mod <- knn(train = train_penguins, test = test_penguins, cl = train_label$species, k = i)
#  k.optm[i] <- 100 * sum(test_label$species == knn.mod) / NROW(test_label$species)
#  k <- i
#  cat(k, "=", k.optm[i], "")
#}

plot(kvalue.results, type = "b", xlab = "K-Value", ylab = "Accuracy Percentage (%)")