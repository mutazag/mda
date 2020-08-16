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



#### matrix metadata #### 
dim(m)
nrow(m)
ncol(m)

#### seminar 1 excerise ####

A  = matrix(c(2,-1,0,-1,2,-1,0,-1,2), nrow = 3, ncol = 3, byrow = TRUE)
print(A)
