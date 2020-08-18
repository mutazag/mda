library(mnormt)


# mu1 and mu2 
mv <- c(1,2)

# variances v1 =1 v2 =2, covariance v12 = 0
vcmat <- matrix(c(1,0,0,1), nrow=2)
print(vcmat)


# create squences for x and y round the mean by 4 standard dev
 x <- seq(
   from=mv[1]-4*vcmat[1,1], 
   to=mv[1]+4*vcmat[1,1], 
   by=vcmat[1,1]/25 
 )


y <- seq(
  from=mv[2]-4*vcmat[2,2], 
  to=mv[2]+4*vcmat[2,2], 
  by=vcmat[2,2]/25 
)
plot(x,y) 


# calculate the density of x,y which is the z access


z <- matrix(0, nrow=length(x), ncol=length(y))

for (i in 1:length(x)){
  for (j in 1:length(y)){
    # for each i,j in x and y calculate the bivariate density 
    z[i,j] <- dmnorm(  # Function to estimate Multivariate Normal Density Function
      c(x[i],y[j]), 
      mean=mv, 
      varcov = vcmat, 
      log= FALSE)
  }
}
round(z[97:103, 97:103],2)

persp(x[1:100],y[1:100],z[1:100,1:100], box=TRUE)

contour(x,y,z, xlim=c(-1,3), ylim=c(0,4))
