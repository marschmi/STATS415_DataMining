library(e1071)
library(LiblineaR)


set.seed(1)
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[ y== 1, ] <-  x[y == 1,] + 1
plot(x, col = (3-y))

data <- data.frame(x = x, y = as.factor(y)) # must set as a factor!!!
svm_fit_10 <- svm(y~., data = data, kernel = "linear", cost = 10, scale = FALSE)
    # scale = FALSE --> tedo not scale each feature to have mean zero or SD of 1

plot(svm_fit_10, data)
str(svm_fit_10)
svm_fit_10$index
summary(svm_fit_10)


# Run with a smaller cost parameters
svm_fit_0.1 <- svm(y~., data = data, kernel = "linear", cost = 0.1, scale = FALSE)
# scale = FALSE --> tedo not scale each feature to have mean zero or SD of 1

plot(svm_fit_0.1, data)
str(svm_fit_0.1)
svm_fit_0.1$index
summary(svm_fit_0.1)


# Let's compare SVMs with a linear kernel comparing a range of values of the cost parameter
tune_out <- tune(svm, y~., data = data, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune_out)

best_svm <- tune_out$best.model
summary(best_svm)


# Making predictions
xtest <- matrix(rnorm(20*2), ncol = 2)
ytest <- sample(c(-1,1), 20, rep = TRUE)
xtest[ytest == 1, ] <- xtest[ytest == 1,] + 1
test_data <- data.frame(x = xtest, y = as.factor(ytest))
y_prediction_0.1 <- predict(best_svm, test_data)
table(predict = y_prediction_0.1, truth = test_data$y)


#  If we had used a cost of 0.01 instead
svm_fit_0.01 <- svm(y~., data = data, kernel = "linear", cost = 0.01, scale = FALSE)
y_prediction_0.01 <- predict(svm_fit_0.01, test_data)
table(predict = y_prediction_0.01, truth = test_data$y)


# separating hyperplane 
# let's further separate the results
x[ y== 1, ] <-  x[y == 1,] + 0.5
plot(x[,2],x[,1],col=(y+5)/2,pch=19)

data <- data.frame(x = x, y = as.factor(y)) # must set as a factor!!!
svm_fit_large_cost <- svm(y~., data = data, kernel = "linear", cost = 1e5, scale = FALSE)
plot(svm_fit_large_cost, data)
summary(svm_fit_large_cost)

svm_fit_1 <- svm(y~., data=data, kernel ="linear", cost =1, scale = FALSE)
summary(svm_fit_1 )
plot(svm_fit_1, data)



## Support Vector Machine
set.seed(1)
x <- matrix(rnorm (200*2) , ncol =2)
x[1:100, ] <- x[1:100,] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1,150), rep(2 ,50))
data <- data.frame(x = x ,y = as.factor(y))
plot(x, col=y, pch=19)

train <- sample(200 ,100)
# Radial kernel
svm_fit_radial_1 <- svm(y~., data=data[train ,], kernel ="radial", gamma =1, cost =1)
plot(svm_fit_radial_1 , data[train ,])
summary(svm_fit_radial_1)

# increase the cost 
svm_fit_radial_1e5 <- svm(y~., data=data[train ,], kernel ="radial", gamma =1, cost =1e5)
plot(svm_fit_radial_1e5 , data[train ,])
summary(svmfit)

# Perform cross-validation to select the best choice of gamma and cost for a radial kernel
set.seed (1)
tune_out_radial <- tune(svm,y~.,data=data[train ,],kernel ="radial", ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000), gamma=c(0.5,1,2,3,4) ))
summary(tune_out_radial)
#  Error rate = 10%
table(true=data[-train ,"y"], pred=predict(tune_out_radial$best.model,newdata =data[-train,]))



```{r plots}




