# redu regression


library(lars)
library(glmnet)
library(car)

data("diabetes")
head(diabetes$x)
head(diabetes$y)


# master.diabetes <- read.delim(url("https://web.stanford.edu/~hastie/Papers/LARS/diabetes.data"))
master.diabetes = diabetes$x
head(master.diabetes)

dim(master.diabetes)
head(master.diabetes)


CovMat <- cov(master.diabetes)
round(CovMat,2)
round(solve(CovMat),2)
round(cor(master.diabetes,method = 'pearson'),1)
corrplot::corrplot(round(cor(master.diabetes,method = 'pearson'),1))

#### part a ####
pairs(master.diabetes[,1:10], col = "dodgerblue")
corrplot::corrplot(
  cor(master.diabetes[,1:10]),
  method = 'number',
  type = 'lower',
  diag = FALSE)

#### part b ####
n <- dim(master.diabetes)[1]
Z <- as.matrix(cbind(rep(1,n),master.diabetes))
Y <- as.matrix(diabetes$y)
Beta <- solve(t(Z) %*% Z) %*% t(Z) %*% Y
round(Beta,4)

# check with R lm() results
X <- master.diabetes
lm_fit <- summary(lm(Y ~ ., data= X ))
round(lm_fit$coefficients,2)


#### part c ####
Y_hat <- Z %*% Beta
e <- Y- Y_hat
Y_mean <- mean(Y)

R2 <- 1 - (sum(e^2) / sum( (Y - Y_mean)^2))
R2
# compare with r lm() result
lm_fit$r.squared


#### part d ####
#calculate the number of model parameters - 1
p<-9
#calculate sum of squared residuals
SSE<-sum(e**2)
#calculate total observations in dataset
n<-length(e)
#calculate residual standard error
RSE <- sqrt(SSE/(n-p-1))
RSE
# compare with r lm() result
lm_fit$sigma


#### part f ####
scatterplot(Y, e)
layout(matrix(c(1,1,2,3),2,2,byrow=T))
#x Residuals Plot
plot(e~Y,
     main=paste0("Respose vs Residuals\n "),
     xlab="Response", ylab="Residuals")
abline(h=0,lty=2)
#Histogram of Residuals
hist(e, main="Histogram of Residuals", breaks = 50,
     ylab="Residuals")
#Q-Q Plot
qqnorm(e)
qqline(e)

# predictors vs residual
for (k in 1:p){
  colName <- colnames(X)[k]
  predictor <- X[,k]
  scatterplot(predictor,e, xlab = colName, boxplots = FALSE)
}

#### part e ####

# simultaneos B confidence interval based on result 7.5
z0 <- matrix(c(1,45,1,32,101,160,95,39,4,5))
Beta
Z
sigmat <- cov(e)
r <- Matrix::rankMatrix(Z)[1]
n <- dim(Z)[1]
s2 <- as.numeric((t(e) %*% e)/(n-r-1))

# sqrt(var Bi) digonal elements of s2 * (Z'Z)^-1
sqrt_var_Bi <- sqrt(diag(s2 * solve(t(Z) %*% (Z))))

# F-distribution term - for simultaneos conf int
F_dist_term <- sqrt((r+1)*qf(.95,(r+1), (n-r-1)))
# t-distribution term for one at a time conf int
t_dist_term <- qt(.95, (n-r-1))

print('simultaneos confidence intervals for Regression Coefficients (Beta)')
for (i in 1:nrow(Beta)){
  bName <- rownames(Beta)
  
  lower_b <- round(Beta[i] - sqrt_var_Bi[i] * F_dist_term,2)
  upper_b <- round(Beta[i] + sqrt_var_Bi[i] * F_dist_term, 2)
  
  
  print(paste0(bName[i], ': ', lower_b, ', ', upper_b))
}

print('one at a time confidence intervals for Regression Coefficients (Beta)')
for (i in 1:nrow(Beta)){
  bName <- rownames(Beta)
  
  lower_b <- round(Beta[i] - sqrt_var_Bi[i] * t_dist_term,2)
  upper_b <- round(Beta[i] + sqrt_var_Bi[i] * t_dist_term, 2)
  
  
  print(paste0(bName[i], ': ', lower_b, ', ', upper_b))
}

# confidence intervals using R
round(confint(lm(Y ~ ., data= X ), levels=.95),2)

