# AT 2 Question 4
# Mutaz Abu Ghazaleh 
# 13184383


master.diabetes <- read.delim(url("https://web.stanford.edu/~hastie/Papers/LARS/diabetes.data"))
head(master.diabetes)

dim(master.diabetes)
head(master.diabetes)


CovMat <- cov(master.diabetes)
round(CovMat,2)
round(solve(CovMat),2)
round(cor(master.diabetes,method = 'pearson'),2)

#### part a ####

pairs(master.diabetes[,1:10], col = "dodgerblue")
corrplot::corrplot(
  cor(master.diabetes[,1:10]),
  method = 'number', 
  type = 'lower', 
  diag = FALSE)

#### part b ####
n <- dim(master.diabetes)[1]
Z <- as.matrix(cbind(b0=rep(1, n), 
           master.diabetes[c('AGE', 'SEX', 'BMI', 'BP', 'S1', 'S2', 'S3', 'S4', 'S5')] ))
Y <- as.matrix(master.diabetes['Y'])
Beta <- solve(t(Z) %*% Z) %*% t(Z) %*% Y
round(Beta,4)

# check with R lm() results
X <- master.diabetes[c('AGE', 'SEX', 'BMI', 'BP', 'S1', 'S2', 'S3', 'S4', 'S5')]
lm_fit <- summary(lm(Y ~ ., data= X ))
round(lm_fit$coefficients,4)
#### part c ####

Y_hat <- Z %*% Beta
e <- Y- Y_hat
Y_mean <- mean(Y)

R2 <- 1 - (sum(e^2) / sum( (Y - Y_mean)^2))
R2          
lm_fit$r.squared


#### part d ####
#calculate the number of model parameters - 1
k<-9-1

#calculate sum of squared residuals
SSE<-sum(e**2)

#calculate total observations in dataset
n<-length(e)

#calculate residual standard error
RSE <- sqrt(SSE/(n-2))
RSE
sqrt(sum(lm_fit$residuals**2)/(n-2))
