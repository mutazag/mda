# AT3 Part B - Q3
library(lars)
library(glmnet)

data("diabetes")
head(diabetes$x)
head(diabetes$y)

Xmatrix <- diabetes$x
yVector <- diabetes$y

# lasso fit
LassoFit <- glmnet(Xmatrix, yVector, lambda = 0)
LassoFit$beta

LassoFit1 <- glmnet(Xmatrix, yVector, lambda = 1)
LassoFit1$beta

LassoFit2 <- glmnet(Xmatrix, yVector, lambda = 2)
LassoFit2$beta

summary(lm(yVector ~ Xmatrix))
