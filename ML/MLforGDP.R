library(mlbench)
library(caret)
library(corrplot)
set.seed(7)
gdpdata <- read.csv("~/Data Analytics Project/datafile.csv", row.names=1)
gdpdata <- gdpdata[,1:7]

validationIndex <- createDataPartition(gdpdata$Gross.Domestic.Product..in.Rs..Cr..at.2004.05.Prices, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- gdpdata[-validationIndex,]
# use the remaining 80% of data to training and testing the models
dataset <- gdpdata[validationIndex,]
colnames(dataset) <- c("GDP","AAS","Agri","Ind","Mining&Q","Manu","Services")
colnames(validation) <- c("GDP","AAS","Agri","Ind","Mining&Q","Manu","Services")

par(mfrow=c(2,4))
for(i in 1:7) {
  plot(density(dataset[,i]), main=names(dataset)[i])
}

par(mfrow=c(2,4))
for(i in 1:7) {
  hist(dataset[,i], main=names(dataset)[i])
}

par(mfrow=c(2,4))
for(i in 1:7) {
  boxplot(dataset[,i], main=names(dataset)[i])
}

correlations <- cor(dataset[,1:7])
corrplot(correlations,method = "circle")

#Run algorithms using 10-fold cross validation

trainControl <- trainControl(method="repeatedcv", number = 10,repeats = 3)
summary(trainControl)
metric <- "RMSE"

#LR
set.seed(7)
fit.lm <- train(GDP~., data=dataset, method="lm", metric=metric, trControl=trainControl)
# SVM
set.seed(7)
fit.svm <- train(GDP~., data=dataset, method="svmRadial", metric=metric, trControl=trainControl)
# CART
#set.seed(7)
#fit.cart <- train(GDP~., data=dataset, method="rpart", metric=metric, trControl=trainControl)
# KNN
set.seed(7)
fit.knn <- train(GDP~., data=dataset, method="knn", metric=metric, trControl=trainControl)

# Compare algorithms
results <- resamples(list(LM=fit.lm, SVM=fit.svm, KNN=fit.knn))
summary(results)
dotplot(results)

# To see which kernel SVM used
print(fit.svm)
print(fit.lm)
print(fit.knn)


set.seed(7)
x <- dataset[,2:7]
y <- dataset[,1]
valX <- validation[,2:7]
valY <- validation[,1]
finalModel <- lm(formula = GDP ~ AAS + Agri + Ind +`Mining&Q` + Manu + Services,data = dataset )
# use final model to make predictions on the validation dataset
predictions <- predict.lm(finalModel, newdata=valX, neighbors=3)
# calculate RMSE
rmse <- RMSE(predictions, valY)
r2 <- R2(predictions, valY)
print(rmse)
