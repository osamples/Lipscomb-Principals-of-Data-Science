## Correlation and Covariance matrix via linear algebra (Revised)

#sink
sink("console.output.corcovmatrix.txt")

#read in data, convert to df
ds <- read.table("http://www.dataminingbook.info/pmwiki.php/Main/NumericDataAnalysis?action=download&upname=magic04.txt", header=F, sep=",")
df <- as.data.frame(ds)
summary(df)
#remove column 11
df <- df[-c(11)]

#calculate multivariate mean vector
n <- nrow(df)
sum <- lapply(df, sum)
sum <- as.integer(sum)
mean <- c()
for(i in sum){
        mean <- c(mean, i/n)
}
mean

#check to see if that is correct
apply(df[,1:10], 2, mean)

#calculate centered data matrix 
mean <- t(mean)
one <- matrix(1, nrow=nrow(df), ncol = ncol(df))
for(i in 1:10) {
        one[,i] <- one[,i]*mean[i]
}
centered <- as.matrix(df-one)

#covariance matrix
covariance_matrix <- (1/n)*t(centered)%*%centered
covariance_matrix

# compare/contrast using R function for covariance
cov(df,df)
#Here we can see the results are practically the same. There does exist some differences, for instance, 
#covarirance_matrix[3,1] = 14.06348989 while cov[3,1] = 14.06422933. In other words, there are slight rounding
#differences for most of the outputs. While there are many differences, they are usually after 4 significant figures, 
#so we can conclude that the two matrices are similar. 

#correlation matrix
#have to use the diagonal function twice in order for it to get back in a 10x10 matrix
diagonal <- diag(diag(covariance_matrix)^(-1/2))
correlation <- diagonal %*% covariance_matrix %*% diagonal
correlation

# compare/contrast using R function for correlation
cor(df)
#It appears that they are the exact same despite the decimal differences with the covariance_matrix. 
#That emphasizes the insignificance of the covariance_matrix differences. 

#close sink
sink()