# AT 2 Question 3
# Mutaz Abu Ghazaleh 
# 13184383


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


#### Part c ####
library(MASS)
Sigma1 <- (1/5630) * matrix(c(575, -60, 10, -60, 300, -50, 10, -50, 196), nrow=3)
Mu <- c(0,0,0)

samples <- mvrnorm(n=10, mu=Mu, Sigma = Sigma1, empirical = TRUE)
samples[1:5, ]
# [,1]        [,2]       [,3]
# [1,] -0.43420504  0.39936954  0.1890455
# [2,] -0.43959806 -0.01014538 -0.0760512
# [3,]  0.16364686  0.34253773 -0.3121635
# [4,] -0.09374287 -0.24083186 -0.2279170
# [5,]  0.64858357 -0.01416940  0.1264043



#### part d ####
df <- mvrnorm(n=10000, mu=Mu, Sigma=Sigma1)

library(GGally)
ggpairs(
  as.data.frame(df), 
  lower= list(continuous = ggally_density, combo = ggally_box_no_facet),
  upper = list(continuous = wrap("points", alpha=.1, colour="#00abff") )
  # mapping = aes(alpha=.7, fill= "#00abff")
) 

pairs(as.data.frame(df))

library(car)
scatter3d(x = df[,1], y=df[,2], z=df[,3], main='3d scatter plot of simulated data', 
          xlab='Y1', ylab = 'Y2', zlab = 'Y3', 
          theta = 30, phi = 30, expand = 0.5, col = "lightblue",
          ltheta = 120, shade = 0.75,
          box=TRUE)
#### Part e ####
Sigma1 <- (1/5630) * matrix(c(575, -60, 10, -60, 300, -50, 10, -50, 196), nrow=3)
E <- eigen(Sigma1)
e1 <- E$vectors[,1]
e2 <- E$vectors[,2]
e3 <- E$vectors[,3]

E$values
# [1] 0.10453986 0.05453633 0.03115472

e1
e2
e3
# > e1
# [1]  0.97591370 -0.21190556  0.05185053
# > e2
# [1] -0.2153318 -0.8975461  0.3847638
# > e3
# [1] 0.03499535 0.38666136 0.92155755



#### part g ####

cum_var_explained <- 0
for (k in 1:3){
  var_pc <- E$values[k] / sum(E$values)
  cum_var_explained <- cum_var_explained + var_pc
  
  print(paste0('Variance explained by PC', k, 
               ': ', round(var_pc, 4), ', ',
               'cumulative variance explained: ', 
               round(cum_var_explained, 4)))
}



#### part h ####  
# using a big sample to find the linear regressio Y2 = B1 Y1 + e1

sigma_h <- matrix(c(575, -60, -60, 300), nrow=2) * (1/5630)
sigma_h_inv <- solve(sigma_h)
samples <- mvrnorm(n=100000, mu=Mu, Sigma = sigma_h)
Z <- samples[,1]
Y <- samples[,2]

# using result 7.4 formula
Beta <- solve(t(Z) %*% Z) %*% t(Z) %*% Y
round(Beta,4)
# [1,] -0.1063

# using R lm() function 

coefficients(lm(Y~Z))['Z']
# Z 
# -0.1063087  




#### part i ####  
# using a big sample to find the linear regression Y3 = B1 Y1 + B2 Y2 + e3

sigma_i <- (1/5630) * matrix(c(196,10,-50,10,575,-60,-50,-60,300), nrow=3)
sigma_i_inv <- solve(Sigma_i)
samples <- mvrnorm(n=100000, mu=Mu, Sigma = sigma_i)
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