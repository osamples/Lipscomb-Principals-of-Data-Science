# Chapter 7 of Zhao's book - Outlier Detection

#sink on
sink("console.output.chapter7.txt")
#pdf on
pdf("myPlot.chapter7.pdf")

##Univariate Outlier Detection
set.seed(3147)
x <- rnorm(100)
summary(x)
# outliers
boxplot.stats(x)$out
boxplot(x)

y <- rnorm(100)
df <- data.frame(x,y)
rm(x,y)
head(df)
attach(df)
#find the index of outliers from x
(a <- which(x %in% boxplot.stats(x)$out))
#find the index of outliers from y
(b <- which(y %in% boxplot.stats(y)$out))
detach (df)

#outliers in both x and y
(outliers.list1 <- intersect(a,b))

plot(df)
points(df[outliers.list1,], col='red', pch='+', cex = 2.5)

#outliers in either x or y
(outliers.list2 <- union(a,b))
plot(df)
points(df[outliers.list2,], col='blue', pch='x', cex=2)

## Outlier detection with LOF
#install.packages("DMwR")
library(DMwR)
#remove "Species", which is a categorical column (LOF only works with numerical variables)
iris2 <- iris[,1:4]
outlier.scores <- lofactor(iris2, k=5)
plot(density(outlier.scores))
#pick top 5 as outliers
outliers <- order(outlier.scores, decreasing = T)[1:5]
#who are outliers
print(outliers)
print(iris2[outliers,])

#show outliers with a biplot of the first two principal components
n <- nrow(iris2)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(iris2), cex = .8, xlabs=labels)

#show outliers with a pairs plot
pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(iris2, pch=pch, col=col)

#install.packages("Rlof")
library(Rlof)
outlier.scores <- lof(iris2, k=5)
#try with different numbers of neighbors (k = 5,6,7,8,9,and 10)
outlier.scores <- lof(iris2, k=c(5:10))

## Outlier detection by clustering
#remove species from the data to cluster
iris2 <- iris[,1:4]
kmeans.result <- kmeans(iris2, centers=3)
#cluster centers
kmeans.result$centers
#cluster IDs
kmeans.result$cluster
#calculate distances between objects and cluster centers
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((iris2 - centers)^2))
#pick top 5 largest distances
outliers <- order(distances, decreasing = T)[1:5]
#who are outliers
print(outliers)
print(iris2[outliers,])
#plot clusters
plot(iris2[,c("Sepal.Length", "Sepal.Width")], pch="o", col=kmeans.result$cluster, cex=0.3)
#plot cluster centers
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=1.5)
#plot outliers
points(iris2[outliers, c("Sepal.Length", "Sepal.Width")], pch="+", col=4, cex = 1.5)

##Outlier Detection from Time Series
# use robust fitting
f <- stl(AirPassengers, "periodic", robust=TRUE)
(outliers <- which(f$weights<1e-8))
#set layout
op <- par(mar=c(0,4,0,3), oma=c(5,0,4,0), mfcol=c(4,1))
plot(f, set.pars=NULL)
sts <- f$time.series
#plot outliers
points(time(sts)[outliers], 0.8*sts[,"remainder"][outliers], pch="x", col="red")
par(op) #reset layout


#turn graphics off
graphics.off()
#close sink
sink()

