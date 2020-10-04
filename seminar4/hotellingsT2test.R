# from week 4 
# mocking up a hotellings T2 to prove a null hyposis that means difference of two groups is zero 


v1 <- c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,6,4,5,6)
v2 <- c(4,5,6,4,5,6,3,1,2,3,5,6,4,5,6,4,5,6,4,5,6,4,5,6)
v3 <- c(7,8,9,7,8,9,7,8,9,7,3,1,2,3,9,7,8,9,7,8,9,7,8,9)
t <- c(1,1,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,2,1,1,1,2,2,2)

df <- data.frame(v1,v2,v3,t)
df

library(ICSNP)

dummyManova <- manova(cbind(v1,v2,v3) ~ t)

dummyT2Test <- summary(dummyManova)

HotellingsT2(df[1:3])

mean(v1)
mean(v2)
mean(v3)

HotellingsT2(df[,1:3], mu=c(2,5,8))
cov(cbind(v1,v2,v3))
solve(cov(cbind(v1,v2,v3)))
colMeans(df)


library(rrcov)
T2.test(x=df[1:3], mu=c(2.5,4.5,7))
T2.test(x=df[1:3], mu=colMeans(df)[1:3])
T2.test(x=df[1:3], mu=c(0,0,0))
HotellingsT2(df[,1:3], mu=c(2,5,8))
HotellingsT2(df[1:3], mu=c(0,0,0))
T2.test(x=df[1:3])


library(mnormt)
df_sample <- round(rmnorm(24, mean=colMeans(df)[1:3], varcov = cov(df[1:3]) ))
df_sample
colMeans(df_sample)
