#### generate random variables -- useful when performing simulatiosn 


rnorm(10,3,1) #sample of scalar random normals

# 
pnorm(2,3,1)
qnorm(.9,3,1)


library(MASS)

mv <- c(1,2,2)
vcmat<-matrix(c(1,0,0,0,1,0,0,0,3), nrow=3)
print(mv)
print(vcmat)

mvrnorm(n=10,mv,vcmat) # Simulate from a Multivariate Normal Distribution
