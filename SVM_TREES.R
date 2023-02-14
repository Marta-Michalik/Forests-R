library(readr)
library(tidyverse)
library(caret)
library(e1071)
library(Metrics)
library(randomForest)


#load data
mieszkania <- read_csv("D:/CYRK/Eksploracja Danych/Zadanie/mieszkania.csv", 
                       col_types = cols(Nazwa = col_skip()))


mieszkania$dzielnica <- as.factor(mieszkania$dzielnica)
mieszkania$typ <- as.factor(mieszkania$typ)
mieszkania$parking <- as.factor(mieszkania$parking)


#normalize
normalized = (mieszkania[,2]-min(mieszkania[,2]))/(max(mieszkania[,2])-min(mieszkania[,2]))
mieszkania$cena <- normalized[,1]


#training
set.seed(3033)
intrain <- createDataPartition(y = mieszkania$cena, p=0.7, list = FALSE)
training <- mieszkania[intrain,]
testing <- mieszkania[-intrain,]


#finding best values for cost and gamma
svm_tune <- tune(svm, cena~., data=training,
                 kernel="linear", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)


#SVM
classifier = svm(formula = cena ~ .,
                 data = training,
                 kernel="polynomial",
                 cost=0.1,
                 scale=T,
                 gamma=0.5)


svmfit <- predict(classifier, newdata = testing)

#plotting results
plot(mieszkania$powierzchnia, mieszkania$cena)
points(testing$powierzchnia, svmfit, col='red', pch=4)

#finding RMSE
svmPredictionRMSE <- rmse(mieszkania$cena,svmfit)
svmPredictionRMSE
# RMSE value for normalized data is 0.15, so the model is not bad



#tree
require(tree)
tree.cena = tree(cena~powierzchnia, data = training, model = T)
summary(tree.cena)
plot(tree.cena)
text(tree.cena, pretty=T)

tree.pr <- predict(tree.cena, newdata = testing)

p_tree <- prune.misclass(tree.cena, k = NULL, best = NULL, newdata=testing,
               nwts, loss, eps = 1e-3)

plot(mieszkania$powierzchnia, mieszkania$cena)
points(testing$powierzchnia, tree.pr, col='red', pch=4)


treePredictionRMSE <- rmse(mieszkania$cena,tree.pr)
treePredictionRMSE
#value for RMSE slightly higher than for SVM (0.17), but still not bad




set.seed(1000)
model.forest <- randomForest(cena ~ ., data = training, importance = T)
importance(model.forest)

price.forest <-predict(model.forest, newdata = testing)

forestPredictionRMSE <- rmse(mieszkania$cena,price.forest)
forestPredictionRMSE

plot(mieszkania$powierzchnia, mieszkania$cena)
points(testing$powierzchnia, price.forest, col='brown')


#all models
plot(mieszkania$powierzchnia, mieszkania$cena)
points(testing$powierzchnia, svmfit, col='red', pch=4)
points(testing$powierzchnia, tree.pr, col='blue', pch=4)
points(testing$powierzchnia, price.forest, col='brown')

