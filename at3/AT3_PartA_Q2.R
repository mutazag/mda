# AT 3 - Part A - Q2

stock.data = read.csv(file='stockdata.csv')
head(stock.data)


R_mat = cor(stock.data)
R_mat


# eigens 
egs  = eigen(R_mat)
V = egs$vectors
Lambdas = egs$values
D = diag(egs$values)

# check 
p = dim(R_mat)[1]
sum(Lambdas)
all.equal(p, sum(Lambdas))

# choose m

# create a scree plot to determine a cutoff at 
# which selected lambda maximise explained variance
pp <- c()
for (m in 1:p){
  pp <- append(pp, sum(Lambdas[1:m]) / p)
    #(1 - (sum(Lambdas[1:i]) / sum(Lambdas)))
}
print(pp)
plot(matrix(pp), type='b')

# m = 3 # 87% cumulatie proportion of total sample variance explained 
m = 2
Fs <- c() 
Hs <- c()


Fl_1 <- round(sqrt(Lambdas[1]) * V[,1], 2)
Fl_2 <- round(sqrt(Lambdas[2]) * V[,2], 2)
# Fl_3 <- round(sqrt(Lambdas[3]) * V[,3], 2)
# Fs <- cbind(Fl_1, Fl_2, Fl_3)
Fs <- cbind(Fl_1, Fl_2)

rownames(Fs) <- colnames(stock.data)
Hs <- apply(Fs, 1, function(r) sum(r**2))
Fs <- cbind(Fs, Hs)
# specific var 1 - Hs
Epsilon <- 1 - Hs
Fs <- cbind(Fs, Epsilon)



## Estimate R
Epsilon_mat <- diag(Epsilon)
# L = matrix(cbind(Fl_1, Fl_2, Fl_3), ncol=3)
L = matrix(cbind(Fl_1, Fl_2), ncol=2)

Est_R = round( L %*% t(L) + Epsilon_mat, 2 )
Est_R
round(R_mat,2)

#sum of squared entries of residual matrix 
# is less or equal to sum of left out eigen values

sum((R_mat - Est_R)**2) <= sum(Lambdas[(m+1):p])

Lambdas[1:m]
Fs

# cumulativ propotion if total sample variance explained 
prop_explained <- c()
for (i in 1:5){
  prop_explained<-append(prop_explained, sum(Lambdas[1:i])/p)
}
round(prop_explained,2)



## rotate factors 
L_rotated <- varimax(L)
rownames(L_rotated$loadings) <- colnames(stock.data)
L_rotated



### using R - mle
fact1 <- factanal(stock.data,factors = 2, rotation = 'none', scores = 'Bartlett')
pairs(fact1$scores)
library(car)
scatterpl


fact2 <- psych::principal(stock.data, nfactors = 2, rotate = 'varimax')
fact2
L_rotated
Fs
prop_explained
