#sink
sink("console.output.chapter3.txt")
#save all plots to pdf
pdf("myPlot.chapter3.pdf")

#Chapter 3 - Data Exploration
dim(iris)
names(iris)
str(iris)
attributes(iris)

#display rows
iris[1:5, ]
head(iris)
tail(iris)

## draw a sample of 5 rows
idx <- sample(1:nrow(iris), 5)
idx
iris[idx, ]

## retrive values of single column
iris[1:10, "Sepal.Length"]
iris[1:10, 1]
iris$Sepal.Length[1:10]

## explore individual variables
summary(iris)
quantile(iris$Sepal.Length)
quantile(iris$Sepal.Length, c(0.1, 0.3, 0.65))

var(iris$Sepal.Length)
hist(iris$Sepal.Length)
plot(density(iris$Sepal.Length))

table(iris$Species)
pie(table(iris$Species))
barplot(table(iris$Species))

## explore multiple variables
#covariance and correlation between varaibles
cov(iris$Sepal.Length, iris$Petal.Length)
cov(iris[,1:4])
cor(iris$Sepal.Length, iris$Petal.Length)
cor(iris[,1:4])

aggregate(Sepal.Length ~ Species, summary, data = iris)
boxplot(Sepal.Length ~ Species, data = iris, xlab = "Species", ylab = "Sepal.Length")

#scatter plot
with(iris, plot(Sepal.Length, Sepal.Width, col = Species, pch=as.numeric(Species)))
###same function as above
# plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species, pch=as.numeric(iris$Species))

#jitter plot
plot(jitter(iris$Sepal.Length), jitter(iris$Sepal.Width))

#smooth scatter plot
smoothScatter(iris$Sepal.Length, iris$Sepal.Width)

#matrix of scatter plots
pairs(iris)

##3D plots
#3d scatter plot
#install.packages("scatterplot3d")
library("scatterplot3d")
scatterplot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width)

#had to download XQuartz
#install.packages("rgl")
library(rgl)
plot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width)

#heat map
distMatrix <- as.matrix(dist(iris[,1:4]))
heatmap(distMatrix)

#level plot
#install.packages("lattice")
library(lattice)
levelplot(Petal.Width~Sepal.Length*Sepal.Width, iris, cuts=9, col.regions = grey.colors(10)[10:1])

#contour plot
filled.contour(volcano, color=terrain.colors, asp = 1, plot.axes = contour(volcano, add=T))

#3D surface plot
persp(volcano, theta=25, phi=30, expand = 0.5, col="lightblue")

#parallel coordinates plot
#install.packages("MASS")
library(MASS)
parcoord(iris[1:4], col=iris$Species)
library(lattice)
parallelplot(~iris[1:4] | Species, data = iris)

##ggplot2 : complex graphics
library(ggplot2)
qplot(Sepal.Length, Sepal.Width, data = iris, facets = Species ~.)

##save plots into files

#save as PDF file
#pdf("myPlot.pdf")
x<- 1:50
plot(x, log(x))
#graphics.off()

#save as a postscript file
#postscript("myPlot2.ps")
x <- -20:20
plot(x, x^2)
#graphics.off()

#turn graphics off
graphics.off()
#close sink
sink()

