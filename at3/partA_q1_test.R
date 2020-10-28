# AT 3 Part A Question 1
# Mutaz Abu Ghazaleh
# 13184383

library(MASS)

Sigma1 <- (1/5630) * matrix(c(575, -60, 10, -60, 300, -50, 10, -50, 196), nrow=3)
E <- eigen(Sigma1)
Lambda <- diag(E$values^-1)
V <- E$vectors

Sigma1_Inv <- round(V %*% Lambda %*% t(V))
Sigma1_Inv
round(Sigma1_Inv %*% Sigma1)
Sigma1 <- (1/5630) * matrix(c(575, -60, 10, -60, 300, -50, 10, -50, 196), nrow=3)


##### parts a, b, c #####

# using a big sample to find the linear regression Y2 = B2,1 Y1 + e1
sigma_2 <- matrix(c(575, -60, -60, 300), nrow=2) * (1/5630)
Mu2 <- c(0,0)
sigma_2_inv <- solve(sigma_2)
samples2 <- mvrnorm(n=100000, mu=Mu2, Sigma = sigma_2)
Z2 <- samples[,1]
Y2 <- samples[,2]

# using result 7.4 formula
Beta2_1 <- solve(t(Z2) %*% Z2) %*% t(Z2) %*% Y2
round(Beta2_1,4)
# [1,] -0.1042
Y2_hat <- Z2 %*% Beta2_1
res2_1 <- Y2_hat - Y2
res2_1 <- res2_1[,1]
round(var(res2_1),4)
hist(res2_1)
qqplot(res2_1,Y2_hat)

# using R lm() function
lm_method2 <- lm(Y2~Z2)
summary(lm_method2)
round(coefficients(lm_method2)['Z2'],4)
res2_2 <- residuals(lm_method2)
round(var(res2_2),4)

# linear regression Y3 = B3,1 Y1 + B3,2 Y2 + e3
sigma_3 <- (1/5630) * matrix(c(196,10,-50,10,575,-60,-50,-60,300), nrow=3)
sigma_3_inv <- solve(sigma_3)
samples3 <- mvrnorm(n=100000, mu=c(0,0,0), Sigma = sigma_3)
Z3 <- samples3[,2:3]
Y3 <- samples3[,1]

# using result 7.4 formula
Beta3 <- solve(t(Z3) %*% Z3) %*% t(Z3) %*% Y3
round(Beta3,4)
# -0.002, -0.1687
Y3_hat <- Z3 %*% Beta3
res3_1 <- Y3_hat - Y3
res3_1 <- res3_1[,1]
round(var(res3_1),4)
hist(res3_1)

# using R lm() function
round(coefficients(lm(Y3~Z3))[c('Z31','Z32')],4)
lm_method3 <- lm(Y3~Z3)
res3_2 <- residuals(lm_method3)
round(var(res3_1),4)
var(res3_2)
sigma_3[1,1]
# Z1           Z2
# 0.0003 -0.1705



# regression coefficients --- part a
Beta2_1
Beta3

# residuals variance *** part b
var(res2_1)
var(res2_2)
sigma_2[2,2]


# residuals variance *** part c
var(res3_1)
var(res3_2)
sigma_3[1,1]




##### part d - T Matrix

Sigma1
T_mat <- matrix( c(1,0,0,
                   .1042,1,0,
                   .002,.1687,1), byrow=TRUE, nrow=3)
T_mat


#### part e ** for T Sigma T_transpose

e_mat <- T_mat %*% Sigma1 %*% t(T_mat)
round(e_mat,4)



##### part F ** find S inverse

S_inv <- round(t(T_mat) %*% diag(1/c(0.1021, 0.0533, 0.0337)) %*% T_mat,2)
round(S_inv, 2)
# S
round(solve(S_inv) * 5630,2)

