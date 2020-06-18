## Confusion Matrix
#https://rdrr.io/cran/caret/man/confusionMatrix.html

#sink
sink("console.output.ConfusionMatrix.txt")
#save all plots to pdf
pdf("myPlot.ConfusionMatrix.pdf")

#install.packages("caret")
#install.packages("lattice")
#install.packages("ggplot2")
library(caret)
library(lattice)
library(ggplot2)

lvs <- c("normal", "abnormal")
truth <- factor(rep(lvs, times = c(86, 258)),
                levels = rev(lvs))
pred <- factor(
        c(
                rep(lvs, times = c(54, 32)),
                rep(lvs, times = c(27, 231))),
        levels = rev(lvs))

xtab <- table(pred, truth)

confusionMatrix(xtab)
confusionMatrix(pred, truth)
confusionMatrix(xtab, prevalence = 0.25)

###################
## 3 class example

confusionMatrix(iris$Species, sample(iris$Species))

newPrior <- c(.05, .8, .15)
names(newPrior) <- levels(iris$Species)

confusionMatrix(iris$Species, sample(iris$Species))

#turn graphics off
graphics.off()
#close sink
sink()

