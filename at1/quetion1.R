# constant probability density contour 

# mu1 and mu2 
mv <- c(0,2)

# var-cov matrix
vcmat = matrix(c(2,.707,.707,1), nrow = 2, byrow = 2)
print(vcmat)

eigens =eigen(sigma1)
print(eigens)



# create sequences for x1 and x2 round the mean by 4 standard dev
x1 <- seq(
  from=mv[1]-4*vcmat[1,1], 
  to=mv[1]+4*vcmat[1,1], 
  by=vcmat[1,1]/25 
)


x2 <- seq(
  from=mv[2]-4*vcmat[2,2], 
  to=mv[2]+4*vcmat[2,2], 
  by=vcmat[2,2]/25 
)

# calculate the mv density of x1,x2 which is the z access


z <- matrix(0, nrow=length(x1), ncol=length(x2))

for (i in 1:length(x1)){
  for (j in 1:length(x2)){
    # for each i,j in x1 and y2 calculate the bivariate density 
    z[i,j] <- dmnorm(  # Function to estimate Multivariate Normal Density Function
      c(x1[i],x2[j]), 
      mean=mv, 
      varcov = vcmat, 
      log= FALSE)
  }
}


# chi-squared distribtion 
print(qchisq(.5, df=2))
c = sqrt(qchisq(.5, df=2) )

# calc contour for 50%
tmpz <- sort(as.vector(z), decreasing = TRUE)
tmpz2 <- cumsum(tmpz)
w <- which(tmpz2 > .5*max(tmpz2))[1]

# plot
contour(x1,x2,z, xlim=c(-5,5), ylim=c(-2,5))
contour(x1,x2,z, levels=tmpz[w],
        drawlabels = FALSE, 
        add=TRUE, col='blue', lwd=2)
points(x=mv[1],y=mv[2], col='red')
abline(v=mv[1],h=mv[2], col='grey')



