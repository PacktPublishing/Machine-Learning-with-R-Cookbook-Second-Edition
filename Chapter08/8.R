library(e1071)
model  =svm(churn~., data = trainset, kernel="radial", cost=1, gamma = 1/ncol(trainset))
summary(model)
install.packages("klaR")
library(klaR)
getwd()
model.light  = svmlight(churn~., data = trainset, ker-nel="radial", cost=1, gamma = 1/ncol(trainset))
iris.subset = subset(iris, select=c("Sepal.Length", "Se-pal.Width", "Species"), Species %in% c("setosa","virginica"))
plot(x=iris.subset$Sepal.Length,y=iris.subset$Sepal.Width, col=iris.subset$Species, pch=19)
svm.model = svm(Species ~ ., data=iris.subset, ker-nel='linear', cost=1, scale=FALSE)
w = t(svm.model$coefs) %*% svm.model$SV
b = -svm.model$rho
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="red", lty=5)
plot(x=iris.subset$Sepal.Length,y=iris.subset$Sepal.Width, col=iris.subset$Species, pch=19)
svm.model = svm(Species ~ ., data=iris.subset, type='C-classification', kernel='linear', cost=10000, scale=FALSE)
points(iris.subset[svm.model$index,c(1,2)],col="blue",cex=2)
w = t(svm.model$coefs) %*% svm.model$SV
b = -svm.model$rho
abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="red", lty=5)
data(iris)
model.iris  = svm(Species~., iris)
plot(model.iris, iris, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))
plot(model, trainset, total_day_minutes ~ total_intl_charge)
svm.pred = predict(model, testset[, !names(testset) %in% c("churn")])
svm.table=table(svm.pred, testset$churn)
svm.table
classAgreement(svm.table)
library(caret)
confusionMatrix(svm.table)
library(car)
data(Quartet)
model.regression = svm(Quartet$y1~Quartet$x,type="eps-regression")
predict.y = predict(model.regression, Quartet$x) 
predict.y
plot(Quartet$x, Quartet$y1, pch=19)
points(Quartet$x, predict.y, pch=15, col="red")
tuned = tune.svm(churn~., data = trainset, gamma = 10^(-6:-1), cost = 10^(1:2))
summary(tuned)
model.tuned = svm(churn~., data = trainset, gamma = tuned$best.parameters$gamma, cost = tuned$best.parameters$cost)
summary(model.tuned)
svm.tuned.pred = predict(model.tuned, testset[, !names(testset) %in% c("churn")])
svm.tuned.table=table(svm.tuned.pred, testset$churn)
svm.tuned.table
classAgreement(svm.tuned.table)
confusionMatrix(svm.tuned.table)
install.packages('neuralnet')
library("neuralnet")
input <- as.data.frame(sample(1:100, 50))
output <- sqrt(input)
input <- as.data.frame(sample(1:100, 50))
output <- sqrt(input)
data <- cbind(input,output)
colnames(data)<-c("Input", "Output")
neuralsqrt = neuralnet(output~input,data,hidden=10, thresh-old = 0.01)                   
test=as.data.frame(c(25,64))
result=compute(neuralsqrt,test)
result$net.result
data(iris)
ind = sample(2, nrow(iris), replace = TRUE, prob=c(0.7, 0.3))
trainset = iris[ind == 1,]
testset = iris[ind == 2,]
install.packages("neuralnet")
library(neuralnet)
trainset$setosa = trainset$Species == "setosa"
trainset$virginica = trainset$Species == "virginica"
trainset$versicolor = trainset$Species == "versicolor"
network = neuralnet(versicolor + virginica + setosa~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, trainset, hidden=3)
network
network$result.matrix
head(network$generalized.weights[[1]])
plot(network)
par(mfrow=c(2,2))
gwplot(network,selected.covariate="Petal.Width")
gwplot(network,selected.covariate="Sepal.Width")
gwplot(network,selected.covariate="Petal.Length")
gwplot(network,selected.covariate="Petal.Width")
net.predict = compute(network, testset[-5])$net.result
net.prediction = c("versicolor", "virginica", "se-tosa")[apply(net.predict, 1, which.max)]
predict.table = table(testset$Species, net.prediction)
predict.table
classAgreement(predict.table)
confusionMatrix(predict.table)
install.packages("nnet")
library(nnet)
data(iris)
set.seed(2)
ind = sample(2, nrow(iris), replace = TRUE, prob=c(0.7, 0.3))
trainset = iris[ind == 1,]
testset = iris[ind == 2,]
iris.nn = nnet(Species ~ ., data = trainset, size = 2, rang = 0.1, decay = 5e-4, maxit = 200)
summary(iris.nn)
iris.predict = predict(iris.nn, testset, type="class")
nn.table = table(testset$Species, iris.predict)
confusionMatrix(nn.table)


