# ralted to q3 f


# X = (X1, X2, X3) -- sample only 
sigma123 = matrix(c(1,-2, 0, -2,5,0,0,0,2), nrow=3)

eigens <- eigen(sigma123)

e1 <- eigens$vectors[,1]
e2 <- eigens$vectors[,2]
e3 <- eigens$vectors[,3]


eigens$values

e1
e2
e3


PC1 = e1 * X
PC1 = -0.3826834 * x1  + 0.9238795  * x2 + 0
PC2 = x3
PC3 = 0.9238795 * x1 + 0.3826834 * x2

# variabce explaince is lambda 1 / sum(all lambdas )
PC1_varexplained = eigens$values[1] /  sum(eigens$values)
PC2_varexplained = eigens$values[2] /  sum(eigens$values)
PC3_varexplained = eigens$values[3] /  sum(eigens$values)

PC1_varexplained
PC2_varexplained
PC3_varexplained

PC1_and_PC2 = PC1_varexplained + PC2_varexplained
allPCs = PC1_and_PC2 + PC3_varexplained
