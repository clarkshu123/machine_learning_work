### KNN

install.packages("ISLR")
library(ISLR)
head(iris)

str(iris)
var(iris$Sepal.Length)
var(iris$Sepal.Width)
var(iris$Petal.Length)
var(iris$Petal.Width)

standardize.iris <- scale(iris[,-5])

mean(standardize.iris[, 'Sepal.Length'])
var(standardize.iris[, 'Sepal.Length'])

final.data <- cbind(standardize.iris, iris[5])

## Split the data
set.seed(101)
library(caTools)

sample <- sample.split(final.data$Species, SplitRatio = 0.70)
train <- subset(final.data, sample == TRUE)
test <- subset(final.data, sample == FALSE)

library(class)
predicted.species <- knn(train[1:4], test[1:4], train$Species, k=1)
mean(test$Species != predicted.species)  # misclassification rate

predicted.value <- NULL
error.rate <- NULL

for (i in 1:10) {
  set.seed(101)
  predicted.species <- knn(train[1:4], test[1:4], train$Species, k=i)
  error.rate[i] <- mean(test$Species != predicted.species)
}

k.values <- 1:10
error.df <- data.frame(error.rate, k.values)

library(ggplot2)

pl<- ggplot(error.df, aes(x=k.values, y=error.rate)) + geom_point()
pl + geom_line(lty='dotted', color='red')





