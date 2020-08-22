library(tidyverse)

n_samples = 100000

# normally disributied univariates
x = rnorm(n_samples)
y = rnorm(n_samples, mean = 2, sd=.5)
df = data.frame(x=x, y=y)
head(df)

colMeans(df)
df_mean <- df %>% summarise_all(mean)
df_var <-df %>% summarise_all(var)
mv_varcov <- cov(as.matrix(df))

print('mean u')
print(t(as.matrix(df_mean)))

print('var-covar Sigma')
print(mv_varcov)

print('spectral decomposition of a Matrix')
B = eigen(mv_varcov)
print(B)



mv_varcov %*% B$vectors
diag(B$values) %*% B$vectors

# plot distributions 
ggplot(df, aes(x = x)) + 
  geom_histogram(bins = 100) +
  geom_vline(xintercept  = df_mean$x)
