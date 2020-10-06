# AT 2 Question 4
# Mutaz Abu Ghazaleh 
# 13184383


master.diabetes <- read.delim(url("https://web.stanford.edu/~hastie/Papers/LARS/diabetes.data"))
head(master.diabetes)

dim(master.diabetes)
head(master.diabetes)


CovMat <- cov(master.diabetes)
round(CovMat,2)
round(solve(CovMat),2)
round(cor(master.diabetes,method = 'pearson'),2)

#### part a ####

pairs(master.diabetes[,1:10], col = "dodgerblue")
corrplot::corrplot(
  cor(master.diabetes[,1:10]),
  method = 'number', 
  type = 'lower', 
  diag = FALSE)

#### part b ####

Z <- as.matrix(master.diabetes[c('AGE', 'SEX', 'BMI', 'BP', 'S1', 'S2', 'S3', 'S4', 'S5')] )
Y <- as.matrix(master.diabetes['Y'])
Beta <- solve(t(Z) %*% Z) %*% t(Z) %*% Y
round(Beta,4)

# check with R lm() results
summary(lm(Y ~ Z))

#### part c ####

