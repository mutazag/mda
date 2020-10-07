# AT 2 Question 2
# Mutaz Abu Ghazaleh
# 13184383



Length<-c(190.5, 172.5, 167., 169.5, 175., 177.5, 179.5, 179.5, 173.5,
          162.5, 178.5, 171.5, 180.5, 183., 169.5, 172., 170., 182.5,
          179.5, 191., 184.5, 181., 173.5, 188.5, 175., 196., 200., 185.,
          174.5, 195.5, 197., 182.5)
Breadth<- c(152.5, 132., 130., 150.5, 138.5, 142.5, 142.5, 138., 135.5,
            139.,135., 148.5, 139., 149., 130., 140., 126.5, 136., 135.,
            140.5, 141.5, 142., 136.5, 130., 153., 142.5, 139.5, 134.5,
            143.5, 144., 131.5, 131.)
Height <- c(145., 125.5, 125.5, 133.5, 126., 142.5, 127.5, 133.5, 130.5,
            131., 136., 132.5, 132., 121.5, 131., 136., 134.5, 138.5,
            128.5, 140.5, 134.5, 132.5, 126., 143., 130., 123.5, 143.5,
            140., 132.5, 138.5, 135., 135.)
Fheight <- c(73.5, 63., 69.5, 64.5, 77.5, 71.5, 70.5, 73.5, 70., 62., 71.,
             65., 74.5, 76.5, 68., 70.5, 66., 76., 74., 72.5, 76.5, 79.,
             71.5, 79.5, 76.5, 76., 82.5, 81.5, 74., 78.5, 80.5, 68.5)
Fbreadth <- c(136.5, 121., 119.5, 128., 135.5, 131., 134.5, 132.5, 133.5,
              126., 124., 146.5, 134.5, 142., 119., 133.5, 118.5, 134.,
              132., 131.5, 141.5, 136.5, 136.5, 136., 142., 134., 146.,
              137., 136.5, 144., 139., 136.)
Type<-c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
Tibet <- data.frame(Length,Breadth,Height,Fheight,Fbreadth,Type)
df <- Tibet[Tibet$Type==1,1:5]
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

#### parts c and d ####
p = ncol(df)
n = nrow(df)
ci=.95
S <- cov(df)
x_bar <- colMeans(df)
f_term <- sqrt((p*(n-1) / (n-p)) * qf(ci, p, n-p))
sd_term <- sqrt(diag(S)/n)
var_names <- colnames(df)

# T2-intervals JW Result 5.3 and equation (5-24)
for (i in 1:p){
  
  lower <- round(x_bar[i] - (f_term * sd_term[i]),2)
  upper <- round(x_bar[i] + (f_term * sd_term[i]),2)

  print(paste0('95% CI for ', var_names[i], ': ', lower, ', ', upper))
}

# Bonferroni intervals - JW equation (5-29)
# adjust interval to get t_p(alpha/2p), equal to qt((1-alpha/2p)), alpha = 1-ci
t_term <- qt((1-(1-ci)/(2*p)), n-1)
for (i in 1:p){
  
  lower <- round(x_bar[i] - (t_term * sd_term[i]),2)
  upper <- round(x_bar[i] + (t_term * sd_term[i]), 2)
  
  # ci <- round(t.test(df[,i], conf.level = (1-.05/p))$conf.int, 2)
  # print(paste0(ci[1], ci[2]))
  print(paste0('95% CI for ', var_names[i], ': ', lower, ', ', upper))
}
