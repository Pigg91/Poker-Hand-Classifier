#Final Project

# Poker hands dataset

# After processing, get new features
# Use new feature data

require(caret)
require(randomForest)
require(e1071)
require(gbm)
require(class)
require(tree)


# load data
myData = read.csv("feature_train_sort.csv", header = TRUE)
myData$Class <- as.factor(myData$Class)
summary(myData$Class)

# 80/20 split data to training and testing
# Simple splitting based on the outcome(class), which ensures that we create balanced splits of the data
set.seed(8)
index <- createDataPartition(myData$Class, p=0.8, list = FALSE, times = 1)
train = myData[index,]
test = myData[-index,]

summary(train$Class)
summary(test$Class)


#### Random Forest ####
set.seed(1)
rf_Fit = randomForest(Class~., data = train, mtry = 4, importance=TRUE)
rf_Pred = predict(rf_Fit, newdata = test)
table(rf_Pred, test$Class)
mean(rf_Pred == test$Class)

#### SVM with radial kernel ####
svmFit = svm(Class~., data = train, kernel="radial")
svmPred = predict(svmFit, test)
table(svmPred, test$Class)
mean(svmPred == test$Class)

#### Gradient	Boosting ####
set.seed(1)
gbmFit <- gbm(Class~., data= train, distribution="multinomial",n.trees = 1000,interaction.depth=2)
gbmPred = predict(gbmFit, newdata = test,n.trees = 1000, type = "response")
out <- as.data.frame(gbmPred)
names(out) <- c("0","1","2","3","4","5","6","7","8","9")
gbmPred <- colnames(out)[max.col(out, ties.method = c("random"))]
table(gbmPred, test$Class)
mean(gbmPred == test$Class)

#### K-nearest neighbor ####
set.seed(1)
knnFit = knn(train[,-7], test[,-7],train$Class, k=1, use.all = TRUE)
table(knnFit, test$Class)
mean(knnFit == test$Class)

#### Naive Bayes ####
set.seed(1)
nbFit = naiveBayes(Class~., data = train, laplace = 3)
nbPred = predict(nbFit, test)
table(nbPred, test$Class)
mean(nbPred == test$Class)

#### Classification Tree ####
tFit <- tree(Class~., data = train)
tPred = predict(tFit, newdata = test, type = "class")
table(tPred, test$Class)
mean(tPred == test$Class)


