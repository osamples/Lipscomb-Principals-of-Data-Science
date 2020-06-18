## Chapter 5 of Zhao's Book - Linear Regression

#sink on
sink("console.output.chapter5.txt")
#pdf on
pdf("myPlot.chapter5.pdf")

year <- rep(2008:2010, each=4)
quarter <- rep(1:4, 3)
cpi <- c(162.2, 164.6, 166.5, 166.0, 166.2, 167.0, 168.6, 169.5, 171.0, 172.1, 173.3, 174.0)
plot(cpi, xaxt="n", ylab="CPI", xlab="")
#draw x-axis
axis(1, labels=paste(year,quarter,sep="Q"), at=1:12, las=3)
cor(year,cpi)
cor(quarter, cpi)
fit <- lm(cpi~year +quarter)
fit

(cpi2011 <- fit$coefficients[[1]] + fit$coefficients[[2]]*2011 + fit$coefficients[[3]]*(1:4))
attributes(fit)
fit$coefficients
#differences between observed values and fitted values
residuals(fit)
summary(fit)
plot(fit)
library(scatterplot3d)
s3d <- scatterplot3d(year, quarter, cpi, highlight.3d = T, type = "h", lab=c(2,3))
s3d$plane3d(fit)
data2011 <- data.frame(year=2011, quarter=1:4)
cpi2011 <- predict(fit, newdata = data2011)
style <- c(rep(1,12), rep(2,4))
plot(c(cpi, cpi2011), xaxt="n", ylab="CPI", xlab='', pch=style, col=style)
axis(1, at=1:16, las=3, labels=c(paste(year,quarter,sep="Q"), "2011Q1", '2011Q2', "2011Q3", "2011Q4"))

#generalized linear regression
data("bodyfat", package="TH.data")
myFormula <- DEXfat ~ age + waistcirc +hipcirc + elbowbreadth + kneebreadth
bodyfat.glm <- glm(myFormula, family = gaussian("log"), data = bodyfat)
summary(bodyfat.glm)

pred <- predict(bodyfat.glm, type="response")
plot(bodyfat$DEXfat, pred, xlab="Observed Values", ylab="Predicted Values")
abline(a=0, b=1)

#turn graphics off
graphics.off()
#close sink
sink()

