#### matrix algebra in R
# https://www.r-bloggers.com/quick-review-of-matrix-algebra-in-r/


m <- matrix( c(1,0,0,1), nrow=2, ncol=2
             , byrow = TRUE)
print(m)
is.matrix(m)

as.matrix(data.frame(c1 = c(1,0), c2 =c(2,3) ))

# diagonal matrix
I <- diag(nrow = 2, ncol = 2,x = c(2,3))
print(I)
# diag can be used for identity matrix gen
I <- diag(nrow = 4, ncol = 4)
print(I)


#### matrix algebra  ####
m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
print(m)
# matrix addition
m + m
# scalar multiplicatio 
4 * m
# matric multiplication 
m %*% m # matrix multiplication is %*% not *


#### transpose and inverse ####
print(m)
m_T = t(m)
print(m_T)
m_inv = solve(m)
print(m_inv)

Im = m %*% m_inv # should equal to idenitiy matrix 

Im == diag(nrow=2, ncol=2)

solve(m) %*% m == diag(nrow = nrow(m), ncol = ncol(m))


#### eigen things ####

b = eigen(m)
print('eigen values')
print(b$values)

print('eigen vectors')
print(b$vectors)



#### determinant of matrix ####
det(m)


#### trace of matrix ####
sum(diag(m)) # sum of diagonal values

#### matrix metadata #### 
dim(m)
nrow(m)
ncol(m)

#### seminar 1 exercise ####

A  = matrix(c(2,-1,0,-1,2,-1,0,-1,2), nrow = 3, ncol = 3, byrow = TRUE)
print(A)

inv_A = solve(A)
print('inverse of A: ')
print(inv_A)


print('verify inverse of A: A %*% inv_A should euqal I') 
A_I = A %*% inv_A

print(A_I)
print(round(A_I,2))

A_I == diag(nrow = 3, ncol = 3)


library(car)
inv(A)

# eignvalues of A 

B = eigen(A)
print(B)

lamb = B$values
v  = B$vectors

#specral decomposition here  https://rdrr.io/r/base/eigen.html
# A * v = lambda * v
# A = lambda * V * V(-1)
round(v %*% diag(lamb) %*% solve(v),2)
round(v %*% diag(lamb) %*% t(v),2)
A
# determinant of A definition 2A.24 pg 131

A_det = det(A)
A_transpose_det = det(round(t(A)))
print(paste0("determinats of A and A_transpose: ", A_det, ", ", A_transpose_det))


# trace of A 
sum(diag(A))
