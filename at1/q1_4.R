# q1 r4.4


a = matrix(c(3,-2,1))
sig = matrix(c(1,1,1,1,3,2,1,2,2), nrow=3, byrow=TRUE)
print(a)
print(sig)

t(a) %*% sig %*% a


#q1 4.3 d

sig43 = matrix(c(1,-2,0,-2,5,0,0,0,2), nrow=3, byrow=TRUE)
print(sig43)
A = matrix(c(.5,.5,0,0,0,1), nrow=2, byrow=TRUE)
print(A)

A %*% sig43 %*% t(A)

#q1 4.3 b

sig43 = matrix(c(1,-2,0,-2,5,0,0,0,2), nrow=3, byrow=TRUE)
print(sig43)
A2 = matrix(c(0,1,0,-5/2,1,-1), nrow=2, byrow=TRUE)
print(A2)

A2 %*% sig43 %*% t(A2)

