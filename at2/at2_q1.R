# AT 2 Question 1
# Mutaz Abu Ghazaleh
# 13184383


df <- read.csv('oxygen.txt')
head(df)
dim(df)
mu <- colMeans(df)



#### part a ####

par(mfrow=c(2,2))
for (v in c('x2','x4')){
  x <- df[,v]
  print(df[,v])
  qqnorm(df[,v], sub=v)
  qqline(df[,v], sub=v)
  
  plot(density(df[,v]), main = v)
}

### part b ####
library(MVN)


par(mfrow=c(1,1))
mvn(df[,c('x2','x4')], 
    multivariatePlot = 'qq', mvnTest = 'royston',
    univariateTest = 'SW')

#### part d ####
# estaimte transformation lambda
trans <- powerTransform(df)
df_transformed <- bcPower(df, lambda = trans$lambda)
colnames(df_transformed) <- colnames(df)

par(mfrow=c(2,2))
for (v in c('x2','x4')){
  x <- df_transformed[,v]
  print(df_transformed[,v])
  qqnorm(df_transformed[,v], sub=paste0(v, ' transformed'))
  qqline(df_transformed[,v], sub=paste0(v, ' transformed'))
  
  plot(density(df_transformed[,v]), main = paste0(v, ' transformed'))
}


par(mfrow=c(1,1))
mvn(df_transformed[,c('x2','x4')], 
    multivariatePlot = 'qq', mvnTest = 'royston',
    univariateTest = 'SW')
