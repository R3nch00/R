card=read.csv("C:/Users/O M A R/Downloads/Document/Data Science/PomPom/C A R D.csv")
print(card)



calculate_roc <- function(verset, cost_of_fp, cost_of_fn, n=100) {
  
  tp <- function(verset, threshold) {sum(verset$predicted >= threshold & verset$Class == 1)}
  fp <- function(verset, threshold) {sum(verset$predicted >= threshold & verset$Class == 0)}
  tn <- function(verset, threshold) {sum(verset$predicted <  threshold & verset$Class == 0)}
  fn <- function(verset, threshold) {sum(verset$predicted <  threshold & verset$Class == 1)}
  tpr <- function(verset, threshold) {sum(verset$predicted >= threshold & verset$Class == 1) / sum(verset$Class == 1)}
  fpr <- function(verset, threshold) {sum(verset$predicted >= threshold & verset$Class == 0) / sum(verset$Class == 0)}
  cost <- function(verset, threshold, cost_of_fp, cost_of_fn) { sum(verset$predicted >= threshold & verset$Class == 0) * cost_of_fp + 
      sum(verset$predicted < threshold & verset$Class == 1) * cost_of_fn}
  threshold_round <- function(value, threshold){return (as.integer(!(value < threshold)))}
  

  auc_ <- function(verset, threshold) { auc(verset$Class, threshold_round(verset$predicted,threshold))}
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tp <- sapply(roc$threshold, function(th) tp(verset, th))
  roc$fp <- sapply(roc$threshold, function(th) fp(verset, th))
  roc$tn <- sapply(roc$threshold, function(th) tn(verset, th))
  roc$fn <- sapply(roc$threshold, function(th) fn(verset, th))
  roc$tpr <- sapply(roc$threshold, function(th) tpr(verset, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(verset, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(verset, th, cost_of_fp, cost_of_fn))
  roc$auc <-  sapply(roc$threshold, function(th) auc_(verset, th))
  return(roc)
}

plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  library(gridExtra)
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=2, alpha=0.5) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
  
  p_auc <- ggplot(roc, aes(threshold, auc)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=2, alpha=0.5) +
    labs(title = sprintf("AUC")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=2, alpha=0.5) +
    labs(title = sprintf("cost function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  sub_title <- sprintf("threshold at %.2f - cost of FP = %d, cost of FN = %d", threshold, cost_of_fp, cost_of_fn)
  
  grid.arrange(p_roc, p_auc, p_cost, ncol=2,sub=textGrob(sub_title, gp=gpar(cex=1), just="bottom"))
}


plot_confusion_matrix <- function(verset, sSubtitle) {
  tst <- data.frame(round(verset$predicted,0), verset$Class)
  opts <-  c("Predicted", "True")
  names(tst) <- opts
  cf <- plyr::count(tst)
  cf[opts][cf[opts]==0] <- "Not Fraud"
  cf[opts][cf[opts]==1] <- "Fraud"
  
  ggplot(data =  cf, mapping = aes(x = True, y = Predicted)) +
    labs(title = "Confusion matrix", subtitle = sSubtitle) +
    geom_tile(aes(fill = freq), colour = "grey") +
    geom_text(aes(label = sprintf("%1.0f", freq)), vjust = 1) +
    scale_fill_gradient(low = "lightblue", high = "blue") +
    theme_bw() + theme(legend.position = "none")
}


sprintf("Rows: %d Columns: %d",nrow(card), length(names(card)))


head(card,10) %>%
  kable( "html", escape=F, align="c") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center")
  

correlations <- cor(card,method="pearson")
corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black")


library(rpart)
library(rpart.plot)
decisionTree_model <- rpart(Class ~ . ,card, method = 'class')
predicted_val <- predict(decisionTree_model,card, type = 'class')
probability <- predict(decisionTree_model, card, type = 'prob')
rpart.plot(decisionTree_model)



card$Class <- factor(card$Class)

set.seed(1998)
samp <- sample(1:nrow(card), round(0.2*nrow(card)))
card <- card[samp, ]
index <- createDataPartition(card$Class, p = 0.75, list = F)
train <- card[index, ]
test <- card[-index, ]


knn1 <- knn(train = train[,-31], test = test[,-31], cl = train$Class, k = 5)
confusionMatrix(knn1, test$Class, positive = "1")


set.seed(1)
split <- sample.split(card$Class, SplitRatio = 0.7)
train <- subset(card, split == T)
cv <- subset(card, split == F)


table(cv$Class)



glm.model <- glm(Class ~ ., data = train, family = "binomial")
glm.predict <- predict(glm.model, cv, type = "response")
table(cv$Class, glm.predict > 0.5)


tree.model <- rpart(Class ~ ., data = train, method = "class", minbucket = 20)
prp(tree.model) 


tree.predict <- predict(tree.model, cv, type = "class")
confusionMatrix(cv$Class, tree.predict)


data.class.0 <- subset(card, card$Class == 0)
data.class.1 <- subset(card, card$Class == 1)
nrow(data.class.0)

nrow(data.class.1)

data.class.0 <- data.class.0[1:10000, ]
nrow(data.class.0)

data <- rbind(data.class.0, data.class.1)
nrow(data)


set.seed(1)
split <- sample.split(data$Class, SplitRatio = 0.7)
train <- subset(data, split == T)
cv <- subset(data, split == F)

table(cv$Class)

glm.model <- glm(Class ~ ., data = train, family = "binomial", control = list(maxit = 50))
glm.predict <- predict(glm.model, cv, type = "response")
table(cv$Class, glm.predict > 0.5)

svm.model <- svm(Class ~ ., data = train, kernel = "radial", cost = 1, gamma = 0.1)
svm.predict <- predict(svm.model, cv)
confusionMatrix(cv$Class, svm.predict)

tree.model <- rpart(Class ~ ., data = train, method = "class", minbucket = 20)
prp(tree.model) 


tree.predict <- predict(tree.model, cv, type = "class")
confusionMatrix(cv$Class, tree.predict)




set.seed(10)
rf.model <- randomForest(Class ~ ., data = train,
                         ntree = 2000, nodesize = 20)

rf.predict <- predict(rf.model, cv)
confusionMatrix(cv$Class, rf.predict)

varImpPlot(rf.model)


library(neuralnet)
ANN_model =neuralnet (Class~.,train_data,linear.output=FALSE)
plot(ANN_model)



set.seed(1998)
samp <- sample(1:nrow(card), round(0.2*nrow(card)))
card <- card[samp, ]
index <- createDataPartition(card$Class, p = 0.75, list = F)
train <- card[index, ]
test <- card[-index, ]

bayes <- naiveBayes(Class~., data = train, laplace = 1)
bayes$apriori

pred <- predict(bayes, test)
confusionMatrix(pred, test$Class, positive = "1")



