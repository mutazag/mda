# AT 2 Question 2
# Mutaz Abu Ghazaleh
# 13184383


df <- read.csv('Concinna.csv')
head(df)
dim(df)
mu <- colMeans(df[,1:2])



#### part a ####
library(car)
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

#### parts c and d ####
p = 2
n = nrow(df)
ci=.95
S <- cov(df[,1:2])
x_bar <- colMeans(df[,1:2])
f_term <- sqrt((p*(n-1) / (n-p)) * qf(ci, p, n-p))
sd_term <- sqrt(diag(S)/n)
var_names <- colnames(df[,1:2])

# T2-intervals JW Result 5.3 and equation (5-24)
for (i in 1:2){
  
  lower <- round(x_bar[i] - (f_term * sd_term[i]),2)
  upper <- round(x_bar[i] + (f_term * sd_term[i]),2)

  print(paste0('95% CI for ', var_names[i], ': ', lower, ', ', upper))
}

# Bonferroni intervals - JW equation (5-29)
# adjust interval to get t_p(alpha/2p), equal to qt((1-alpha/2p)), alpha = 1-ci
t_term <- qt((1-(1-ci)/4), n-1)
for (i in 1:2){
  
  lower <- round(x_bar[i] - (t_term * sd_term[i]),2)
  upper <- round(x_bar[i] + (t_term * sd_term[i]), 2)
  
  # ci <- round(t.test(df[,i], conf.level = (1-.05/p))$conf.int, 2)
  # print(paste0(ci[1], ci[2]))
  print(paste0('95% CI for ', var_names[i], ': ', lower, ', ', upper))
}


#### part f ####
# marginal uni-variate normality qqplots 
par(mfrow=c(1,2))
for (i in 1:p)
{
  qqnorm(df[,i], sub=var_names[i])
  qqline(df[,i], sub=var_names[i])
}
scatterplotMatrix(df[,1:2], regLine = FALSE, smooth = FALSE)

par(mfrow=c(1,1))
library(MVN)
mvn(df[,1:2], mvnTest = 'royston', multivariatePlot = 'qq')
