# Lab 9
# March 25th, 2016

#install.packages("tree")
library(tree)
library(ISLR)
attach(Carseats)

high <- ifelse(Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, high)
head(Carseats)

tree_carseats <- tree(high ~ .-Sales, Carseats)
summary(tree_carseats)

plot(tree_carseats)
text(tree_carseats, pretty = 0, cex = 0.5)
tree_carseats


set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats_test <- Carseats[-train,]
high_test <- high[-train]
tree_carseats <- tree(high ~ . -Sales, Carseats, subset = train)
tree_pred <- predict(tree_carseats, Carseats_test, type = "class")
table(tree_pred, high_test)

set.seed(3)
cv_carseats <- cv.tree(tree_carseats, FUN = prune.misclass)
names(cv_carseats)
par(mfrow = c(1,2))
plot(cv_carseats$size, cv_carseats$dev, type = "b")
plot(cv_carseats$k, cv_carseats$dev, type = "b")

prune_carseats <- prune.misclass(tree_carseats, best = 9)





set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
prune.boston=prune.tree(tree.boston,best=5)
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)



