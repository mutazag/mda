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


# part c -- sampling 
library(mnormt)
df_sample <- rmnorm(n=1000, mean = c(0,0,0), varcov = (1/5630)*sigma1)
round(colMeans(df_sample))

library(car)
scatterplotMatrix(df_sample)
scatterplotMatrix(df_sample[,1:2])
scatterplotMatrix(df_sample[,c(1,3)])
scatterplotMatrix(df_sample[,c(2,3)])
