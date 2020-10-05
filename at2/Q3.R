# At2 Q3 


# part a

# method 1
sigma1 = matrix(c(575,-60,10,-60,300,-50,10,-50,196), nrow=3)
print(sigma1)

round(solve((1/5630)*sigma1))


# method 2
A <- (1/5630)*sigma1    #Covariance of X
E <- eigen(A)  #Eigenvectors and -values of A
Lambda <- diag(E$values^-1)    #Diagonal matrix with inverse of eigenvalues
Gamma <- E$vectors  #Eigenvectors
A_Inverse <- Gamma%*%Lambda%*%t(Gamma)  #compute the inverse
round(A_Inverse%*%A,5)  #check invA * A equals identity 


round(A_Inverse)


## convert covariance matrix to correlation matrix
corr_matrix <- cov2cor(sigma1)
print(corr_matrix)
# part c -- sampling 
library(mnormt)
df_sample <- rmnorm(n=1000, mean = c(0,0,0), varcov = (1/5630)*sigma1)
round(colMeans(df_sample))

library(car)
scatterplotMatrix(df_sample)
scatterplotMatrix(df_sample[,1:2])
scatterplotMatrix(df_sample[,c(1,3)])
scatterplotMatrix(df_sample[,c(2,3)])

colnames(df_sample) <- c('x1','x2','x3')
library(GGally)
  ggpairs(as.data.frame(df_sample), 
    lower = list(continuous = ggally_density, combo = ggally_box_no_facet),
  ) # showing univariate distributions, univariate correlations

  
  # Classic Bivariate Normal Diagram
  library(ellipse)
  rho <- cor(df_sample[,1:2])
  y_on_x <- lm(df_sample[,2] ~ df_sample[,1])    # Regressiion Y ~ X
  x_on_y <- lm(df_sample[,1] ~ df_sample[,2])    # Regression X ~ Y
  plot_legend <- c("99% CI green", "95% CI red","90% CI blue",
                   "Y on X black", "X on Y brown")
  
  plot(df_sample[,1:2], xlab = "X", ylab = "Y",
       col = "dark blue",
       main = "Bivariate Normal with Confidence Intervals", xlim=c(-2,2), ylim=c(-2,2))
  lines(ellipse(rho), col="red")       # ellipse() from ellipse package
  lines(ellipse(rho, level = .05), col="green")
  lines(ellipse(rho, level = .1), col="blue")
  abline(y_on_x)
  abline(x_on_y, col="brown")
  legend(3,1,legend=plot_legend,cex = .5, bty = "n")
  
  

library(MVN)
roystonTest(df_sample,qqplot = TRUE)
mvn(df_sample, mvnTest = 'royston')


df_sample2 <- cbind(
  3*rnorm(1000, mean=1.2, sd=1.2),
  2.1*rnorm(1000),
  1.7*rnorm(1000,sd=.9)
)

scatterplotMatrix(df_sample2)
mvn(df_sample2, mvnTest = 'royston',multivariatePlot = 'qq', univariateTest = 'SF', univariatePlot = 'qq')

persp(x=df_sample2[,1],y=df_sample2[,2],z=df_sample2[,3], axes = TRUE,box = TRUE)

library(MASS)
kde_2vars <- kde2d(x=df_sample[,1],y=df_sample[,2])
contour(kde_2vars)
text(12.2,92,"1",cex=1.5)
points(12.2,89,col="red",pch=18)
text(10.4,47,"2",cex=1.5)
points(10.4,43,col="red",pch=18)
text(21,45,"3",cex=1.5)
points(21,40,col="red",pch=18)
text(21.6,82,"4",cex=1.5)
points(21.6,78,col="red",pch=18)

contour(kde2d(x=df_sample[,1],y=df_sample[,3]))
contour(kde2d(x=df_sample[,2],y=df_sample[,3]))


# part h -- linear regression 

# Y2 = B1 Y1 + e2

Y1 = df_sample[,1]
Y2 = df_sample[,2]
Y3 = df_sample[,3]

#Beta_hat <- inverse of ( Y1transpose %*% Y1) %*% Y1transpose %*% Y2#
# pg 366


solve(t(Y1) %*% Y1) %*% t(Y1) %*% Y2

lm_r <- lm(Y2 ~ Y1)





# inverse method 

# method 2
inverse_method <- function(A){
  #A <- (1/5630)*sigma1    #Covariance of X
  E <- eigen(A)  #Eigenvectors and -values of A
  Lambda <- diag(E$values^-1)    #Diagonal matrix with inverse of eigenvalues
  Gamma <- E$vectors  #Eigenvectors
  A_Inverse <- Gamma%*%Lambda%*%t(Gamma)  #compute the inverse
  round(A_Inverse%*%A,5)  #check invA * A equals identity   
  
  return(round(A_Inverse,6))
}




# part i Y3 = B1 Y1 + B2 Y2 + e3

Y3 = BZ + e3

Z = matrix (0, Y1, Y2)
Beta = matrix (B1 and B2)

Z = cbind(Y1, Y2)

# B = (Z' Z)(-1) Z' Y

Z_t_Z <- t(Z) %*% Z
inverse_method(Z_t_Z)

B <-  inverse_method(Z_t_Z) %*% t(Z) %*% Y3


lm_r_2 <- lm(Y3 ~ Y1+Y2)
lm_r_2
  
Y_hat <-  Z %*% B
e <- Y3 - Y_hat

hist(e,breaks = 50)
scatterplot(Y_hat, e)
plot(e~Y_hat)
qqnorm(e)
qqline(e)
Y3 <- B[1] * Y1 + B[2] * Y2 + (Y3 - Y_hat)




# part h -- using result 7.12

# B = inv (Sigma_Z_z) %*% small_sigma_Z_Y

# Y3 = B1 Y1 + B2 Y2 + e3
full_sigma <- (1/5630)*sigma1

sigma_Z_Z <- as.matrix(full_sigma[1:2,1:2])

small_sigma_z_Y <- as.matrix(full_sigma[1:2,3])

B <- inverse_method(sigma_Z_Z) %*% small_sigma_z_Y
B





