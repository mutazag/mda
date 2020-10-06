# AT 2 Question 2
# Mutaz Abu Ghazaleh
# 13184383


df <- read.csv('Concinna.csv')
head(df)
dim(df)
mu <- colMeans(df[,1:2])



#### part a ####
scatterplotMatrix(df[,1:2],
                  ellipse = list(levels=c(.95), robust=TRUE, fill=FALSE, fill.alpha=0.2),
                  regLine  = FALSE, smooth = FALSE,
                  diagonal = FALSE
                  )


#  calculating the ellipsoid half lengths using eigen values
p = 2
n = dim(df)[1]
E <- eigen(cov(df[,1:2]))

ci = .95
f_term <- sqrt((p*(n-1)/(n*(n-p))) * qf(.95,p, n-p))
ellipsoid_half_lengths_units <- c(sqrt(E$values[1] * f_term), sqrt(E$vectors[2] * f_term))


#### part b ####
mu2 <- c(125, 14) 

library(ICSNP)

HotellingsT2(X=df[,1:2], mu=mu2)

# Hotelling's one sample T2-test
# 
# data:  df[, 1:2]
# T.2 = 148.72, df1 = 2, df2 = 19, p-value = 2.485e-12
# alternative hypothesis: true location is not equal to c(125,14)