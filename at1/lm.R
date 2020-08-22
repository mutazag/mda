# https://archive.ics.uci.edu/ml/datasets/Airfoil+Self-Noise 

# Data Set Information:
#
# The NASA data set comprises different size NACA 0012 airfoils at various wind
# tunnel speeds and angles of attack. The span of the airfoil and the observer
# position were the same in all of the experiments.
#
#
# Attribute Information:
#
# This problem has the following inputs: 
# 1. Frequency, in Hertzs.
# 2. Angle of # attack, in degrees. 
# 3. Chord length, in meters. 
# 4. Free-stream velocity, in meters per second. 
# 5. Suction side displacement thickness, in meters.
#
# The only output is: 
# 6. Scaled sound pressure level, in decibels.


library(tidyverse)
library(GGally)
df <- read.table(file='https://archive.ics.uci.edu/ml/machine-learning-databases/00291/airfoil_self_noise.dat', sep ='\t')
colnames(df) <- c('freq', 'angle', 'chord_length', 'free_stream_velocity', 'suc_disp_thick', 'sound_db')
head(df)

# https://www.researchgate.net/publication/282988800_Airfoil_Self_Noise_Prediction_Using_Linear_Regression_Approach


df %>% 
  ggpairs()


# test for normality 

par(mfrow = c(3,2))
for (i in 1:ncol(df)){ 
  print(i)
  qqnorm(df[,i], sub=colnames(df)[i])
}

# univariate normality test
ks_test_before =apply(df, 2, function(x) ks.test(scale(x), y=pnorm))

df %>% ggplot(aes(x=log(freq))) + geom_density()
mean(log(df$freq))

df_normal <- df
df_normal$freq_log <- log(df$freq)
df_normal$angle_log <- log(df$angle)
df_normal$chord_length_log <- log(df$chord_length)
df_normal$free_stream_velocity_log <- log(df$free_stream_velocity)
df_normal$suc_disp_thick_log <- log(df$suc_disp_thick)
df_normal$sound_db_log <- log(df$sound_db)

df_normal %>% select(ends_with("_log")) %>% 
  ggpairs()

df_subset <- df_normal %>% select(ends_with("_log"))
ks_test_after =apply(df_subset, 2, function(x) ks.test(x, y=pnorm))
par(mfrow = c(3,2))
for (i in 1:ncol(df)){ 
  print(i)
  qqnorm(df[,i], sub=colnames(df_subset)[i])
}

# multivariate normality test 

library(MVN)
mvtest_before <- mvn(df, mvnTest = "royston", univariatePlot = TRUE, multivariatePlot = TRUE)
mvtest_before
mvtest_after <- mvn(df_subset, mvnTest = "royston")
mvtest_after
mvn(normal, mvnTest = "royston")

library(car)
trans <- powerTransform(df$freq)
df$freq_bc <- bcPower(df$freq,trans$lambda )
mvtest_bc <- mvn(df[c("sound_db","freq_bc")], mvnTest = "royston")
mvtest_bc
par(mfrow = c(1,2))
for (i in 1:ncol(df[c("sound_db","freq_bc")])){ 
  print(i)
  qqnorm(df[c("freq","freq_bc")][,i], sub=colnames(df[c("freq","freq_bc")])[i])
}


# split data
simple.fit <- lm(sound_db ~ freq + angle + chord_length + free_stream_velocity + suc_disp_thick, data=df)
summary(simple.fit)
plot(simple.fit)


layout(matrix(c(1,1,2,3),2,2,byrow=T))
#Spend x Residuals Plot
plot(simple.fit$resid~df$sound_db[order(df$sound_db)],
     main="Spend x Residuals\nfor Simple Regression",
     xlab="Sound dB Spend", ylab="Residuals")
abline(h=0,lty=2)
#Histogram of Residuals
hist(simple.fit$resid, main="Histogram of Residuals",
     ylab="Residuals")
#Q-Q Plot
qqnorm(simple.fit$resid)
qqline(simple.fit$resid)

library(lmtest) #dwtest
dwtest(simple.fit) #Test for independence of residuals
#Null Hypothesis: Errors are serially UNcorrelated
#Results: DW = 1.1347, p-value = 0.03062
lmtest::coefci(simple.fit)

# http://www.learnbymarketing.com/tutorials/linear-regression-in-r/#:~:text=S%20ummary%3A%20R%20linear%20regression%20uses%20the%20lm,out%20the%20%24resid%20variable%20from%20your%20new%20model.


# R example on same daaset
#http://srisai85.github.io/airfoil_noise/Airfoil_Noise_Prediction.html
