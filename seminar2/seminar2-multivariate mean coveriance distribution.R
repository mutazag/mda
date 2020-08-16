## visulisaing muti variate normal distribution 

library(mnormt)


mv<-c(1,2)   #mean vector
vcmat<-matrix(c(1,0,0,1), nrow=2)   #variance coveriance matrix 


print(mv)
print(vcmat)


x <- seq(from=mv[1]-4*vcmat[1,1], to=mv[1]+4*vcmat[1,1], by=vcmat[1,1]/25)
y <- seq(from=mv[2]-4*vcmat[2,2], to=mv[2]+4*vcmat[2,2], by=vcmat[2,2]/25)
z <- matrix(0,201,201)

for (i in 1:201){
  for(j in 1:201){
    z[i,j] <- dmnorm(c(x[i],y[j]), mean=mv,  varcov =  vcmat, log=FALSE)
  }
}


persp(x,y,z, axes=TRUE,box=TRUE)
contour(x,y,z, axes=TRUE)


#do it again with a different covariance matrix 


mv<-c(1,2)   #mean vector
vcmat<-matrix(c(1,0,0,2), nrow=2)   #variance coveriance matrix 


print(mv)
print(vcmat)


x <- seq(from=mv[1]-4*vcmat[1,1], to=mv[1]+4*vcmat[1,1], by=vcmat[1,1]/25)
y <- seq(from=mv[2]-4*vcmat[2,2], to=mv[2]+4*vcmat[2,2], by=vcmat[2,2]/25)
z <- matrix(0,201,201)

for (i in 1:201){
  for(j in 1:201){
    z[i,j] <- dmnorm(c(x[i],y[j]), mean=mv,  varcov =  vcmat, log=FALSE)
  }
}


persp(x,y,z, axes=TRUE,box=TRUE)
contour(x,y,z, axes=TRUE)



# introuce coveriance between the two variables of .7


mv<-c(1,2)   #mean vector
vcmat<-matrix(c(1,.7,.7,2), nrow=2)   #variance coveriance matrix 


print(mv)
print(vcmat)


x <- seq(from=mv[1]-4*vcmat[1,1], to=mv[1]+4*vcmat[1,1], by=vcmat[1,1]/25)
y <- seq(from=mv[2]-4*vcmat[2,2], to=mv[2]+4*vcmat[2,2], by=vcmat[2,2]/25)
z <- matrix(0,201,201)

for (i in 1:201){
  for(j in 1:201){
    z[i,j] <- dmnorm(c(x[i],y[j]), mean=mv,  varcov =  vcmat, log=FALSE)
  }
}


persp(x,y,z, axes=TRUE,box=TRUE)
contour(x,y,z, axes=TRUE)


open3d()

