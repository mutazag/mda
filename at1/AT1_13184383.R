# Multivariate Data Analysis , Assignment 1 
# Mutaz Abu Ghazaleh 
# 

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
library(Amelia)
library(corrplot)
library(MVN)



df <- read.table(file='https://archive.ics.uci.edu/ml/machine-learning-databases/00291/airfoil_self_noise.dat', sep ='\t')
colnames(df) <- c('freq', 'angle', 'chord_length', 'free_stream_velocity', 'suc_disp_thick', 'sound_db')
head(df)

# check for missing values in variables
missmap(df)

#dimensions
dim(df)

df %>% 
  ggpairs(
    lower = list(continuous = ggally_density, combo = ggally_box_no_facet),
  ) # showing univariate distributions, univariate correlations



# inspecting the correlation in the inputs 
variablesCorr <-  cor(df)
corrplot(variablesCorr,method = "number", diag = FALSE)
round(variablesCorr,2)

# highest correlation between angle and suc_disp_thick




# test for normality 

par(mfrow = c(2,3))
for (i in 1:ncol(df)){ 
  print(i)
  qqnorm(df[,i], sub=colnames(df)[i])
}

ks_test_before =apply(df, 2, function(x) ks.test(df, y=pnorm))
ks_test_before
mvn(df, mvnTest = "royston")



library(car)
trans <- powerTransform(df$freq)
df$freq_bc <- bcPower(df$freq,trans$lambda )
df$suc_disp_thick_bc <- bcPower(df$suc_disp_thick,powerTransform(df$suc_disp_thick)$lambda )
df$free_stream_velocity <- bcPower(df$free_stream_velocity,powerTransform(df$free_stream_velocity)$lambda )


# mvtest_bc <- mvn(df[c("sound_db","freq_bc")], mvnTest = "royston")
# mvtest_bc
# par(mfrow = c(1,2))
# for (i in 1:ncol(df[c("sound_db","freq_bc")])){ 
#   print(i)
#   qqnorm(df[c("freq","freq_bc")][,i], sub=colnames(df[c("freq","freq_bc")])[i])
# }
# mvn(df[c("freq_bc", "freq","sound_db")])



plotFit <- function(fit1,subTitle="Simple Regression"){ 
  layout(matrix(c(1,1,2,3),2,2,byrow=T))
  #Spend x Residuals Plot
  plot(fit1$resid~df$sound_db[order(df$sound_db)],
       main=paste0("Sound db Residuals\n ", subTitle),
       xlab="Sound dB", ylab="Residuals")
  abline(h=0,lty=2)
  #Histogram of Residuals
  hist(fit1$resid, main="Histogram of Residuals", breaks = 50,
       ylab="Residuals")
  #Q-Q Plot
  qqnorm(fit1$resid)
  qqline(fit1$resid)
}

fitandPlot <- function(df, formula = sound_db ~ ., subTitle="Simple Regression" ){
  xfit <- lm(formula = formula , data=df)
  print(summary(xfit))
  plotFit(xfit, subTitle = subTitle)
}

# simple fit
simple.fit <- lm(sound_db ~ . , data=df)
summary(simple.fit)
plot(simple.fit)
plotFit(simple.fit)

freq.fit <- lm(sound_db ~ freq , data=df)
summary(freq.fit)
plot(freq.fit)
plotFit(freq.fit)


fitandPlot(df)
fitandPlot(df, sound_db ~ freq)
fitandPlot(df, sound_db ~ angle)
fitandPlot(df, sound_db ~ chord_length)
fitandPlot(df, sound_db ~ free_stream_velocity)
fitandPlot(df, sound_db ~ suc_disp_thick)

fitandPlot(df, sound_db ~ freq + chord_length + free_stream_velocity + suc_disp_thick + angle)


