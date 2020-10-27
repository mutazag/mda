# ch4

# 4.3

A = matrix(c(1,2,3,4,5,6,7,8,9), nrow=3)
print(A)


es = eigen(A)
sum(es$values)
sum(diag(A))
es$vectors[1,]
es$values
es$vectors[,1]
?eigen
Matrix::tr

diag(A)
A


x = matrix(c(5,7,9), nrow=3)
x
t(x)
xAx <- t(x) %*% A %*% x
Axx <- A %*% x %*% t(x)
sum(diag(xAx))
sum(diag(Axx))
