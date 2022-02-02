

install.packages("caret")
install.packages("ggplot2")
install.packages("lattice")
library(caret)
library(lattice)
library(ggplot2)

library(randomForest)
library(rpart)
library(gbm)


training <-  pml.training

testing<-pml.testing

dim(training)

dim(testing)

## Cleaning data

#If the variable contains some data that is definited as NA, so, i will remove.

datataining <- training[, colSums(is.na(training)) == 0]

dim(datataining)

datatesting <- testing[, colSums(is.na(testing)) == 0]

dim(datatesting)

#Also i removed the variables that had zero variance
nzv<-nearZeroVar(datataining)
nzv1<-nearZeroVar(datatesting)

datataining1 <- datataining[, -nzv]

datatesting1 <- datatesting[,-nzv1]

dim(datataining1)

dim(datatesting1)

# Moreover, i removed rows like raw time stamp, name, etc. Those variables dont keep relation with how
# someone does some exercise

datatrain<-datataining1[, -c(1:6)]
datatest<-datatesting1[, -c(1:6)]


intrain <- createDataPartition(datatrain$classe, 
                               p=0.7, list = FALSE)

train<-datatrain[intrain,]
test <- datatrain[-intrain,]

# cross validation

crossval <- trainControl(method = "cv", number = 10, verboseIter = FALSE)

#Build model

mod<-train(classe~., method="rpart", data = train, trControl= crossval)
fancyRpartPlot(mod$finalModel)
pred<-predict(mod, test)
dtree<-confusionMatrix(pred,  factor(test$classe))
dtree$overall["Accuracy"]#0.4868309
plot(mod)




mod1<-train(classe~., method="rf", data = train, trControl= crossval)
pred1<-predict(mod1, test)
rf<-confusionMatrix(pred1,factor(test$classe))
rf$overall["Accuracy"]#0.4868309 
plot(mod1)

mod2<-train(classe~., method="gbm", data = train, trControl= crossval)
pred2<-predict(mod2, test)
gbm<-confusionMatrix(pred2,factor(test$classe))
gbm$overall["Accuracy"]# 0.9587086 
plot(mod2)

mod3<-train(classe~.,method="svmLinear", data=train, trControl=crossval)
predict3<-predict(mod3, test)
svm<-confusionMatrix(predict3,factor(test$classe))
svm$overall["Accuracy"]#0.7753611
plot(mod3)


table(rf$overall["Accuracy"],gbm$overall["Accuracy"],svm$overall["Accuracy"],dtree$overall["Accuracy"])


# In this case the Gradient Bossted trees has the best performance in relation with the other models
#Also, it shows the most higher value of accuracy that is 0.9587
# I made my predictions

predic<-predict(mod2, datatest)
predic
