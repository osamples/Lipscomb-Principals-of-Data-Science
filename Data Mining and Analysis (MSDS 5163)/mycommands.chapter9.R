##Chapter 9 in Zhao's book -  Association Rules
#sink on
sink("console.output.chapter9.txt")
#pdf on
pdf("myPlot.chapter9.pdf")

url <- "http://www.rdatamining.com/data/titanic.raw.rdata"
download.file(url, destfile="/Users/osamples/Documents/Lipscomb/MSDS 5163/Data Exploration & Reading ISL/Chapter 9/titanic.raw.rdata")
load(file = "/Users/osamples/Documents/Lipscomb/MSDS 5163/Data Exploration & Reading ISL/Chapter 9/titanic.raw.rdata")

str(Titanic)
df <- as.data.frame(Titanic)
head(df)
titanic.raw <- NULL
for(i in 1:4){
        titanic.raw <- cbind(titanic.raw, rep(as.character(df[,i]), df$Freq))
}
titanic.raw <- as.data.frame(titanic.raw)
names(titanic.raw) <- names(df)[1:4]
dim(titanic.raw)
str(titanic.raw)
head(titanic.raw)
summary(titanic.raw)

##association rule mining
#install.packages("arules")
library(arules)
#find association rules with default settings
rules.all <- apriori(titanic.raw)
quality(rules.all) <- round(quality(rules.all), digits=3)
rules.all

inspect(rules.all)

#rules with rhs containing "Survived"only
rules <- apriori(titanic.raw, control=list(verbose=F), 
                 parameter = list(minlen=2, supp=0.005, conf=0.8), 
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"),
                                   default="lhs"))
quality(rules) <- round(quality(rules), digits=3)                 
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

##Removing Redundancy
#find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
#I had to change the "NA" in his code to "FALSE"
subset.matrix[lower.tri(subset.matrix, diag=T)] <- FALSE
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

#remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

##interpreting rules
rules <- apriori(titanic.raw,
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(rhs=c("Survived=Yes"),
                                   lhs=c("Class=1st", "Class=2nd", "Class=3rd", "Age=Child", "Age=Adult"),
                                   default="none"),
                 control=list(verbose=F))
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)


##visualizing association rules
#install.packages("arulesViz")
library(arulesViz)
#had to add  jitter=0 to prevent R adding it directly
plot(rules.all, jitter=0)
plot(rules.all, method="grouped")
plot(rules.all, method="graph")
plot(rules.all, method="graph", control=list( type="items"))
plot(rules.all, method="paracoord", control=list(reorder=TRUE))

#turn graphics off
graphics.off()
#close sink
sink()

