### Nerual Nets

library(MASS)
set.seed(101)
data <- Boston
str(data)  # the data type stored
summary(data)
head(data)    
any(is.na(data))  # check if there exists missing values

#install.packages('neuralnet', repos = 'http://cran.us.r-project.org')
library(neuralnet)

### train the model
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
head(scaled)

library(caTools)
split = sample.split(scaled$medv, SplitRatio = 0.7)

train = subset(scaled, split == TRUE)
test = subset(scaled, split == FALSE)

library(neuralnet)
n <- names(train)
f <- as.formula(paste("medv ~ ", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f, data=train, hidden=c(5,3),linear.output = TRUE)
plot(nn)

### Predictions
predicted.nn.values <- compute(nn, test[1:13])
str(predicted.nn.values)

# convert back to non-scaled predictions
true.predictions <- predicted.nn.values$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
# convet the test data
test.r <- (test$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
# check the mean squared error
MSE.nn <- sum((test.r - true.predictions)^2)/nrow(test)
MSE.nn
# visualize error
error.df <- data.frame(test.r, true.predictions)
head(error.df)

library(ggplot2)
ggplot(error.df, aes(x=test.r, y=true.predictions)) + geom_point() + stat_smooth()


