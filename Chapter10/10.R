install.packages("SuperLearner")
install.packages("caret")
install.packages("glmnet")
install.packages("xgboost")
install.packages("randomForest")
library(SuperLearner)
data(Boston, package="MASS")
training = sample(nrow(Boston), 100)
X = Boston[training, ]
X_Hold = Boston[-training, ]
Y = Boston$medv[training, ]
Y_Hold = Boston$medv[-training, ]
sl_glm = SuperLearner(Y = Y, X = X, family = gaussian(), SL.library = "SL.glmnet")
sl_glm
sl_randomf = SuperLearner(Y = Y, X = X, family = gaussian(), SL.library = "SL.randomForest")
sl_randomf
install.packages("SuperLearner")
library(SuperLearner)
sl_models = c("SL.xgboost", "SL.randomForest", "SL.glmnet", "SL.nnet", "SL.rpartPrune", "SL.lm", "SL.mean")
superlearner = SuperLearner(Y = Y, X = X, family = gaussian (),
                              SL.library = sl_models)
superlearner
r = CV.SuperLearner(Y = Y, X = X, family = gaussian(),
                    +                   SL.library = sl_lib)
plot(r)
pred = predict(superlearner, X_holdout, onlySL = T) 
install.packages("adabag")
library(adabag)
set.seed(2)
churn.bagging = bagging(churn ~ ., data=trainset, mfinal=10)
churn.bagging$importance
churn.predbagging= predict.bagging(churn.bagging, newdata=testset)
churn.predbagging$confusion
churn.predbagging$error
install.packages("ipred")
library(ipred)
churn.bagging = bagging(churn ~ ., data = trainset, coob = T)
churn.bagging
mean(predict(churn.bagging) != trainset$churn)
churn.prediction = predict(churn.bagging, newdata=testset, type="class")
prediction.table = table(churn.prediction, testset$churn)
churn.baggingcv = bagging.cv(churn ~ ., v=10, data=trainset, mfinal=10)
churn.baggingcv$confusion
churn.baggingcv$error
set.seed(2)
churn.boost = boosting(churn ~.,data=trainset,mfinal=10, co-eflearn="Freund", boos=FALSE , control=rpart.control(maxdepth=3))
churn.boost.pred = pre-dict.boosting(churn.boost,newdata=testset)
churn.boost.pred$confusion
churn.boost.pred$error
library(mboost)
install.packages("pROC")
library(pROC)
set.seed(2)
ctrl = trainControl(method = "repeatedcv", repeats = 1, classProbs = TRUE, summaryFunction = twoClassSummary)
ada.train = train(churn ~ ., data = trainset, method = "ada", metric = "ROC", trControl = ctrl)
ada.train$result
plot(ada.train)
ada.predict = predict(ada.train, testset, "prob")
ada.predict.result = ifelse(ada.predict[1] > 0.5, "yes", "no")
table(testset$churn, ada.predict.result)
churn.boostcv = boosting.cv(churn ~ ., v=10, data=trainset, mfinal=5,control=rpart.control(cp=0.01))
churn.boostcv = boosting.cv(churn ~ ., v=10, data=trainset, mfinal=5,control=rpart.control(cp=0.01))
churn.boostcv$confusion
churn.boostcv$error
install.packages("gbm")
library(gbm)
trainset$churn = ifelse(trainset$churn == "yes", 1, 0)
set.seed(2)
churn.gbm = gbm(formula = churn ~ .,distribution = "bernoulli",data = trainset,n.trees = 1000,interaction.depth = 7,shrinkage = 0.01, cv.folds=3)
summary(churn.gbm)
churn.iter = gbm.perf(churn.gbm,method="cv")
churn.predict = predict(churn.gbm, testset, n.trees = churn.iter)
str(churn.predict)
churn.roc = roc(testset$churn, churn.predict)
plot(churn.roc)
coords(churn.roc, "best")
churn.predict.class = ifelse(churn.predict > coords(churn.roc, "best")["threshold"], "yes", "no")
table( testset$churn,churn.predict.class)
install.packages("mboost")
library(mboost)
trainset$churn = ifelse(trainset$churn == "yes", 1, 0)
trainset$voice_mail_plan = NULL
trainset$international_plan = NULL
churn.mboost = mboost(churn ~ ., data=trainset,  control = boost_control(mstop = 10))
summary(churn.mboost)
par(mfrow=c(1,2))
plot(churn.mboost)
boost.margins = margins(churn.boost, trainset)
boost.pred.margins = margins(churn.boost.pred, testset)
plot(sort(boost.margins[[1]]), (1:length(boost.margins[[1]]))/length(boost.margins[[1]]), type="l",xlim=c(-1,1),main="Boosting: Margin cumulative dis-tribution graph", xlab="margin", ylab="% observations", col = "blue")
lines(sort(boost.pred.margins[[1]]), (1:length(boost.pred.margins[[1]]))/length(boost.pred.margins[[1]]), type="l", col = "green")
abline(v=0, col="red",lty=2)
boosting.training.margin = table(boost.margins[[1]] > 0)
boosting.negative.training = as.numeric(boosting.training.margin[1]/boosting.training.margin[2])
boosting.negative.training
boosting.testing.margin = table(boost.pred.margins[[1]] > 0)
boosting.negative.testing = as.numeric(boosting.testing.margin[1]/boosting.testing.margin[2])
boosting.negative.testing
bagging.margins = margins(churn.bagging, trainset)
bagging.pred.margins = margins(churn.predbagging, testset)
plot(sort(bagging.margins[[1]]), (1:length(bagging.margins[[1]]))/length(bagging.margins[[1]]), type="l",xlim=c(-1,1),main="Bagging: Margin cumulative distribution graph", xlab="margin", ylab="% observations", col = "blue")
lines(sort(bagging.pred.margins[[1]]), (1:length(bagging.pred.margins[[1]]))/length(bagging.pred.margins[[1]]), type="l", col = "green")
abline(v=0, col="red",lty=2)
bagging.training.margin = table(bagging.margins[[1]] > 0)
bagging.negative.training = as.numeric(bagging.training.margin[1]/bagging.training.margin[2])
bagging.negative.training
bagging.testing.margin = table(bagging.pred.margins[[1]] > 0)
bagging.negative.testing = as.numeric(bagging.testing.margin[1]/bagging.testing.margin[2])
bagging.negative.testing
boosting.evol.train = errorevol(churn.boost, trainset)
boosting.evol.test = errorevol(churn.boost, testset)
plot(boosting.evol.test$error, type = "l", ylim = c(0, 1),
       +       main = "Boosting error versus number of trees", xlab = "Iterations",
       +       ylab = "Error", col = "red", lwd = 2)
lines(boosting.evol.train$error, cex = .5, col = "blue", lty = 2, lwd = 2)
legend("topright", c("test", "train"), col = c("red", "blue"), lty = 1:2, lwd = 2)
bagging.evol.train = errorevol(churn.bagging, trainset)
bagging.evol.test = errorevol(churn.bagging, testset)
plot(bagging.evol.test$error, type = "l", ylim = c(0, 1),
       +       main = "Bagging error versus number of trees", xlab = "Iterations",
       +       ylab = "Error", col = "red", lwd = 2)
lines(bagging.evol.train$error, cex = .5, col = "blue", lty = 2, lwd = 2)
legend("topright", c("test", "train"), col = c("red", "blue"), lty = 1:2, lwd = 2)
install.packages("randomForest")
library(randomForest)
churn.rf = randomForest(churn ~ ., data = trainset, im-portance = T)
churn.rf
churn.prediction = predict(churn.rf, testset)
table(churn.prediction, testset$churn)
plot(churn.rf)
importance(churn.rf)
varImpPlot(churn.rf)
margins.rf=margin(churn.rf,trainset)
plot(margins.rf)
hist(margins.rf,main="Margins of Random Forest for churn da-taset")
boxplot(margins.rf~trainset$churn, main="Margins of Random Forest for churn dataset by class")
install.packages("party")
library(party)
churn.cforest = cforest(churn ~ ., data = trainset, con-trols=cforest_unbiased(ntree=1000, mtry=5))
churn.cforest
churn.cforest.prediction = predict(churn.cforest, testset, OOB=TRUE, type = "response")
table(churn.cforest.prediction, testset$churn)
churn.bagging= errorest(churn ~ ., data = trainset, model = bagging)
churn.bagging
install.packages("ada")
library(ada)
churn.boosting= errorest(churn ~ ., data = trainset, model = ada)
churn.boosting
churn.rf= errorest(churn ~ ., data = trainset, model = ran-domForest)
churn.rf
churn.predict = function(object, newdata) {predict(object, newdata = newdata, type = "class")}
churn.tree= errorest(churn ~ ., data = trainset, model = rpart,predict = churn.predict)
churn.tree

