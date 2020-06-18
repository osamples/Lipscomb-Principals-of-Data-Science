#sink
sink("console.output.finalproject.txt")
#pdf on
pdf("myPlot.finalproject.pdf")

rm(list = objects())
#set the seed
set.seed(17)

library(clustMixType)
library(permute)
library(combinat)
library(sets)
library(factoextra)
library(caret)
library(lattice)
library(ggplot2)
library(gclus)
library(gridExtra)
data(wine)
summary(wine)

###exploratory data analysis
#correlation
library(ggcorrplot)
corr <- round(cor(wine[-c(1)]), 1)
ggcorrplot(corr, title = "Figure 1: Wine Correlation Matrix")

#determine if normalization is needed
par(mfrow = c(2, 7))
for (i in 2:14){
        boxplot(wine[i], xlab=colnames(wine)[i])
}
par(mfrow = c(1, 1))

par(mfrow = c(2,1))
boxplot(wine, title="Figure 2: Original Wine Data vs. Normalized Wine Data")

#scale the data
dat_scaled <- as.data.frame(scale(wine[-1]))
summary(dat_scaled)
boxplot(dat_scaled)
wine <- cbind(wine[1],dat_scaled)

par(mfrow = c(1,1))
#compare svd per dimension
mysvd <- svd(wine[-c(1)])
plot(mysvd$d, main="mysvd", type="b", col="blue")
variance.explained = prop.table(mysvd$d^2)
variance.explained

#randomly seperate into test/train data
ind <- sample(2, nrow(wine), replace=TRUE, prob=c(0.7, 0.3))
mdTrain <- wine[ind==1,]
mdTest <- wine[ind==2,]


#remove y component
truelabel<-mdTrain$Class
mydata <- mdTrain[-c(1)]

#set number of clusters you want to explore
num <- 3

###KMEANS 
(kmeans.result <- kmeans(mydata, num))

plot(mydata[c(7, 1)], col = kmeans.result$cluster)
# plot cluster centers
points(kmeans.result$centers[,c(7, 1)], col = 1:3, pch = 8, cex=2)

#evaluate kmeans using silhouette
# Visualize silhouhette information
require("cluster")
sil <- silhouette(kmeans.result$cluster, dist(mydata))
fviz_silhouette(sil)

#confusion matrix : Adjusted in order to remove the labels of the classes
classes <- c(1,2,3)
classes <- levels(factor(classes[kmeans.result$cluster]))
mappings <- permn(classes)

myaccuracy <- matrix(0,1, set_cardinality(mappings))
for(jj in seq(1, set_cardinality(mappings))){
        print(mappings[jj])
        thisorder = mappings[[jj]]
        CM = confusionMatrix(factor(thisorder[kmeans.result$cluster]), factor(truelabel) )
}

plot(1:jj, myaccuracy, type = "b")
jjmax = which.max(myaccuracy)
thisorder = mappings[[jjmax]]
CM = confusionMatrix(factor(thisorder[kmeans.result$cluster]), factor(truelabel))
CM

# numerr = (1- CM$overall[1])*dim(mydata)[1]
# numcor = (CM$overall[1]) * dim(iris)[1]
# 
# mybino = function(N,k){
#         fact(N)/fact(N-k)/fact(k)
# }
# 
# pvalue_est = function(successes, failures){
#         N = successes + failures
#         sum = 0
#         for (jj in seq(0, failures))
# }

# cm1 <- confusionMatrix(factor(truelabel), factor(kmeans.result$cluster))
# cm1
#confusion matrix for num=2
#table(truelabel,kmeans.result$cluster)

#PREDICTION ON TEST DATA
#calculate distance between test data and centroids
myEuclid <- function(points1, points2) {
        distanceMatrix <- matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
        for(i in 1:nrow(points2)) {
                distanceMatrix[,i] <- sqrt(rowSums(t(t(points1)-points2[i,])^2))
        }
        distanceMatrix
}

distances <- myEuclid(mdTest[-c(1)], kmeans.result$centers)

onesigma <- (kmeans.result$withinss/kmeans.result$size)^0.5
sd <- as.matrix(onesigma*2)

#create prediction function
mypredict <- function(distances, sd){
        differences <- matrix(NA, nrow=dim(distances)[1], ncol=dim(sd)[1])
        predict <- matrix(NA, nrow=dim(distances)[1], ncol=1)
        
    for(J in 1:nrow(sd)) {
           for(j in 1:nrow(distances)){
                   differences[j,J] <- distances[j,J]-sd[J]
                   if (differences[j,J] < 0){
                           differences[j,J] <- Inf
                   }
           }
    }
        for(j in 1:nrow(distances)){
                order <- order(differences[j,])
                differences[j,] <- sort(differences[j,])
                predict[j] <- order[1]
}
        return(predict)
}

prediction <- mypredict(distances, sd)
compare <- cbind(prediction, mdTest[c(1)])
accuracy <- sum(compare[,1]==compare[,2])/nrow(compare)
accuracy

#PAM
library(cluster)
pam.result <- pam(mydata, num)

layout(matrix(c(1,2),1,2)) # 2 graphs per page
plot(pam.result)
layout(matrix(1)) # change back to one graph per page

#EVALUATE PAM K-MEDOIDS USING SILHOUETTE
# Visualize silhouette information
fviz_silhouette(pam.result)

#confusion matrix
classes <- c(1,2,3)
classes <- levels(factor(classes[pam.result$cluster]))
mappings <- permn(classes)

myaccuracy <- matrix(0,1, set_cardinality(mappings))
for(jj in seq(1, set_cardinality(mappings))){
        print(mappings[jj])
        thisorder = mappings[[jj]]
        CM = confusionMatrix(factor(thisorder[pam.result$cluster]), factor(truelabel) )
}

plot(1:jj, myaccuracy, type = "b")
jjmax = which.max(myaccuracy)
thisorder = mappings[[jjmax]]
CM2 = confusionMatrix(factor(thisorder[pam.result$cluster]), factor(truelabel))
CM2

# cm2 <- confusionMatrix(factor(truelabel), factor(pam.result$clustering))
# cm2
#confusion matrix for num=2
#table(truelabel, pam.result$cluster)

#predict
distances <- myEuclid(mdTest[-c(1)], pam.result$medoids)
onesigma <- (pam.result$clusinfo[,2]/pam.result$clusinfo[,1])^0.5
sd <- as.matrix(onesigma*2)

prediction <- mypredict(distances, sd)
compare <- cbind(prediction, mdTest[c(1)])
accuracy <- sum(compare[,1]==compare[,2])/nrow(compare)
accuracy

###SPECTRAL CLUSTERING
library(kernlab)
z=as.matrix(mydata)
myclusters=specc(z,num)
plot(mydata, col = myclusters)
print(myclusters)
fviz_silhouette(silhouette(myclusters, dist(mydata)))

#confusion matrix
classes <- c(1,2,3)
classes <- levels(factor(classes[myclusters]))
mappings <- permn(classes)

myaccuracy <- matrix(0,1, set_cardinality(mappings))
for(jj in seq(1, set_cardinality(mappings))){
        print(mappings[jj])
        thisorder = mappings[[jj]]
        CM = confusionMatrix(factor(thisorder[kmeans.result$cluster]), factor(truelabel) )
}

plot(1:jj, myaccuracy, type = "b")
jjmax = which.max(myaccuracy)
thisorder = mappings[[jjmax]]
CM3 = confusionMatrix(factor(thisorder[myclusters]), factor(truelabel))
CM3
# cm3 <- confusionMatrix(factor(truelabel), factor(myclusters))
# cm3
#confusion matrix for num=2
#table(truelabel, myclusters)

#predict
distances <- myEuclid(mdTest[-c(1)], centers(myclusters))
onesigma <- (withinss(myclusters)/size(myclusters))^0.5
sd <- as.matrix(onesigma*2)

prediction <- mypredict(distances, sd)
compare <- cbind(prediction, mdTest[c(1)])
accuracy <- sum(compare[,1]==compare[,2])/nrow(compare)
accuracy

grid.arrange(
tableGrob(CM$table, cols = c("Reference: 1", "Reference: 2", "Reference: 3"), rows = c("Prediction: 1","Prediction: 2", "Prediction: 3")),
tableGrob(CM2$table, cols = c("Reference: 1", "Reference: 2", "Reference: 3"), rows = c("Prediction: 1","Prediction: 2", "Prediction: 3")),
tableGrob(CM3$table, cols = c("Reference: 1", "Reference: 2", "Reference: 3"), rows = c("Prediction: 1","Prediction: 2", "Prediction: 3")), nrow=3)

#turn graphics off
graphics.off()
#close sink
sink()

