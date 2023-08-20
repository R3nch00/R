card=read.csv("C:/Users/O M A R/Downloads/Document/Data Science/PomPom/Book1.csv")
print(card)

names(card)

colSums(is.na(card))

summary(card)

sprintf("Rows: %d Columns: %d",nrow(card), length(names(card)))



numeric_cols <- sapply(card, is.numeric)
card[, numeric_cols] <- lapply(card[, numeric_cols], as.numeric)



numeric_cols <- sapply(card, is.numeric)
card[, numeric_cols] <- lapply(card[, numeric_cols], function(col) {
  col[is.na(col)] <- 0
  return(col)
})



cols_to_normalize <- colnames(card)[!(colnames(card) %in% c("Class", "Time"))]
card[, cols_to_normalize] <- scale(card[, cols_to_normalize])



head(card)





card$Amount <- scale(card$Amount)
card$Time <- scale(card$Time)
set.seed(42)
index <- sample(1:nrow(card), 0.7 * nrow(card))
train_data <- card[index, ]
test_data <- card[-index, ]



correlations <- cor(card,method="pearson")
corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black")



card$Class <- factor(card$Class)
card$Class



library(class)
library(caret)
card$Class <- as.factor(card$Class)
set.seed(123)  
train_indices <- sample(1:nrow(card), 0.7 * nrow(card))
train_data <- card[train_indices, ]
test_data <- card[-train_indices, ]



knn_model <- knn(train_data[, -ncol(train_data)], test_data[, -ncol(test_data)], train_data$Class, k = 5)
head(knn_model,100)



confusion_matrix <- confusionMatrix(knn_model, test_data$Class)
print(confusion_matrix)






library(class)
library(caret)
card$Class <- as.factor(card$Class)
num_folds <- 10
set.seed(123) 
cv <- createFolds(card$Class, k = num_folds)
ctrl <- trainControl(method = "cv", number = num_folds)
knn_model <- train(Class ~ ., data = card, method = "knn",
                   trControl = ctrl, tuneLength = 1,
                   preProcess = c("center", "scale"),
                   metric = "Accuracy")

cat("Mean Accuracy:", knn_model$results$Accuracy, "\n")


















