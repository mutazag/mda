# Q4

master.diabetes <- read.delim(url("https://web.stanford.edu/~hastie/Papers/LARS/diabetes.data"))
head(master.diabetes)


inverse_method <- function(A){
  #A <- (1/5630)*sigma1    #Covariance of X
  E <- eigen(A)  #Eigenvectors and -values of A
  Lambda <- diag(E$values^-1)    #Diagonal matrix with inverse of eigenvalues
  Gamma <- E$vectors  #Eigenvectors
  A_Inverse <- Gamma%*%Lambda%*%t(Gamma)  #compute the inverse
  round(A_Inverse%*%A,5)  #check invA * A equals identity   
  
  return(round(A_Inverse,6))
}


# part b -- regression using R and our method .. our method failed. 
Z <- as.matrix(master.diabetes[,1:10])

Z_t_Z <- t(Z) %*% Z
diag(inverse_method(Z_t_Z))
B <- inverse_method(Z_t_Z) %*% t(Z) %*% as.matrix(master.diabetes[,11])

round(cor(master.diabetes), 2)
pairs(master.diabetes)

lm_diabetes <- lm(master.diabetes[,11] ~ . , data = master.diabetes[,1:10])
lm_diabetes$coefficients
lm_diabetes$residuals



# part c -- calculate R2 
e_res <-lm_diabetes$residuals
y_mean <- mean(master.diabetes[,11])
y <- master.diabetes[,11]
1 - (sum(e_res^2) / sum((y-y_mean)^2))

summary(lm_diabetes)

# part d -- examine residuals 

qqnorm(e_res)
qqline(e_res)
plot(density(e_res))
plot(e_res, y) # -> show som corr with error, which means that the model is not properly explaining the output 


#Residual Standard error (Like Standard Deviation)
k=length(lm_diabetes$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(lm_diabetes$residuals**2)
n=length(lm_diabetes$residuals)
sqrt(SSE/(n-(1+k))) #Residual Standard Error
summary(y)

RSE = sqrt(SSE / (n-1))
RSE 
summary(lm_diabetes)


# part f
plot(e_res, y) # -> show som corr with error, which means that the model is not properly explaining the output 
scatterplot(e_res, y)
pairs(master.diabetes[,1:10])
round(cor(master.diabetes[,1:10]))
corrplot::corrplot(cor(master.diabetes[,1:10]))
##### colinear data #######
gen_exact_collin_data = function(num_samples = 100) {
  x1 = rnorm(n = num_samples, mean = 80, sd = 10)
  x2 = rnorm(n = num_samples, mean = 70, sd = 5)
  x3 = 2 * x1 + 4 * x2 + 3
  y = 3 + x1 + x2 + rnorm(n = num_samples, mean = 0, sd = 1)
  data.frame(y, x1, x2, x3)
}

col_data <- gen_exact_collin_data()
round(cor(col_data), 2)
summary(lm(col_data$y ~ col_data$x1 + col_data$x2 + col_data$x3))
Z <- as.matrix(col_data[,2:4])
Z_t_Z <- t(Z) %*% Z
diag(inverse_method(Z_t_Z))
B <-  inverse_method(Z_t_Z) %*% t(Z) %*% as.matrix(col_data$y)
