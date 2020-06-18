# Chapter 4 of Zhao's book - Decision Trees and Random Forests

#sink on
sink("console.output.chapter4.txt")
#pdf on
pdf("myPlot.chapter4.pdf")

#split data into test and training set
str(iris)
set.seed(1234)
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7,0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

##load package party and build a decision tree
#install.packages("party")
library(party)
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data = trainData)
#check the prediction
table(predict(iris_ctree), trainData$Species)
#print/plot the tree
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type = "simple")

#predict on test data
testPred <- predict(iris_ctree, newdata =  testData)
table(testPred, testData$Species)

##decision trees with package rpart
#use new dataset "bodyfat"
data("bodyfat", package = "TH.data")
dim(bodyfat)
attributes(bodyfat)
bodyfat[1:5,]

#split into test/training sets
set.seed(1234)
ind <- sample(2, nrow(bodyfat),replace=TRUE, prob=c(0.7, 0.3))
bodyfat.train <- bodyfat[ind==1,]
bodyfat.test <- bodyfat[ind==2,]
#train a decision tree
#install.packages("rpart")
library(rpart)
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat_rpart <- rpart(myFormula, data = bodyfat.train, control = rpart.control(minsplit = 10))
attributes(bodyfat_rpart)
print(bodyfat_rpart$cptable)
print(bodyfat_rpart)
plot(bodyfat_rpart)
text(bodyfat_rpart, use.n = T)


##now we select the tree with the minimum prediction error
opt<- which.min(bodyfat_rpart$cptable[,"xerror"])
cp<- bodyfat_rpart$cptable[opt,"CP"]
bodyfat_prune <- prune(bodyfat_rpart, cp=cp)
print(bodyfat_prune)

plot(bodyfat_prune)
text(bodyfat_prune, use.n=T)

#now we use that model to predict the values on the test data and compare with actual values
DEXfat_pred <- predict(bodyfat_prune, newdata = bodyfat.test)
xlim <- range(bodyfat$DEXfat)
plot(DEXfat_pred ~ DEXfat, data = bodyfat.test, xlab ="Observed", ylab="Predicted", ylim=xlim, xlim =xlim)
abline(a=0, b=1)

##Random Forest
#split the data in train/test subsets
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7,0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

#install.packages("randomForest")
library(randomForest)
rf <- randomForest(Species ~ ., data=trainData, ntree = 100, proximity = TRUE)
table(predict(rf), trainData$Species)
print(rf)
attributes(rf)
plot(rf)
importance(rf)
varImpPlot(rf)

#use on test data
irisPred <- predict(rf, newdata=testData)
table(irisPred, testData$Species)
plot(margin(rf, testData$Species))

#turn graphics off
graphics.off()
#close sink
sink()

