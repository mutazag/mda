# AT3 Part B - Q3

library(lars)
library(glmnet)


data("diabetes")
head(diabetes$x)
head(diabetes$y)


Xmatrix <- diabetes$x
yVector <- diabetes$y


# lasso fit 
LassoFit <- glmnet(Xmatrix, yVector, lambda = 1)
LassoFit$beta


LassoFit2 <- glmnet(Xmatrix, yVector, lambda = 2)
LassoFit2$beta

LassoFit3 <- glmnet(Xmatrix, yVector)
LassoFit3$beta
LassoFit3
summary(LassoFit3)
