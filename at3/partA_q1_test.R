
library(MASS)
#### Part a ####

Sigma1 <- (1/5630) * matrix(c(575, -60, 10, -60, 300, -50, 10, -50, 196), nrow=3)
E <- eigen(Sigma1)
Lambda <- diag(E$values^-1)
V <- E$vectors

Sigma1_Inv <- round(V %*% Lambda %*% t(V))
Sigma1_Inv
# [,1] [,2] [,3]
# [1,]   10    2    0
# [2,]    2   20    5
# [3,]    0    5   30

round(Sigma1_Inv %*% Sigma1)
# [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1


#### Part e ####
Sigma1 <- (1/5630) * matrix(c(575, -60, 10, -60, 300, -50, 10, -50, 196), nrow=3)


#### part h ####
# using a big sample to find the linear regressio Y2 = B1 Y1 + e1

sigma_h <- matrix(c(575, -60, -60, 300), nrow=2) * (1/5630)
Mu <- c(0,0)
sigma_h_inv <- solve(sigma_h)
samples <- mvrnorm(n=100000, mu=Mu, Sigma = sigma_h)
Z <- samples[,1]
Y <- samples[,2]

# using result 7.4 formula
Beta <- solve(t(Z) %*% Z) %*% t(Z) %*% Y
round(Beta,4)
# [1,] -0.1063

# using R lm() function

lm_method <- lm(Y~Z)
summary(lm_method)
coefficients(lm_method)['Z']
res2 <- residuals(lm_method)
var(res2)
# Z
# -0.1063087

Y_hat <- Beta %*% Z

res1 <- Y_hat - Y
res1 <- res1[1,]

hist(res1)
qqplot(res1,Y_hat)

## JW pg 381 (7-17)
# var(e) = s^2 (1 - h)
# s^2 = residual mean square ((good enough estimate for residual variance))
# the term (1-h) is likely to be almost 1 so it can be ignored 
# term 1-h can be calcualted based on JW(7-16) 
#  I - Z(tZ Z)^(-1) tZ
# given Z is one variable only i can simply this as 
#   1 - Z  1/(Z^2) * Z

Z * 1/(t(Z) %*% Z) -> zz
# zz * Z is zz^2
zz <- zz[,1]
#zz
1 - sum(zz **2)
1 - sum(zz * Z)
s2<- mean(res1**2)
s2
s2* (1 - sum(zz * Z))
s2 * (1 - sum(zz **2))
var(res1)
var(res2)
# mean(residuals**2)
# Z %*% solve(t(Z) %*% Z) %*% t(Z)
# var(residuals)
#### part i ####
# using a big sample to find the linear regression Y3 = B1 Y1 + B2 Y2 + e3

sigma_i <- (1/5630) * matrix(c(196,10,-50,10,575,-60,-50,-60,300), nrow=3)
sigma_i_inv <- solve(sigma_i)
samples <- mvrnorm(n=100000, mu=c(0,0,0), Sigma = sigma_i)
Z <- samples[,2:3]
Y <- samples[,1]

# using result 7.4 formula
Beta <- solve(t(Z) %*% Z) %*% t(Z) %*% Y
round(Beta,4)
# [1,]  0.0003
# [2,] -0.1705

# using R lm() function

round(coefficients(lm(Y~Z))[c('Z1','Z2')],4)
# Z1           Z2
# 0.0003 -0.1705

