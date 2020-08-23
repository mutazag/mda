
#install.packages("MVN","VGAM","car")
library(MVN)
library(VGAM)
library(car)


### MANOVA Example
filmdata<-read.csv("C:/Documents/Film.csv")

filmdata$Rate_Ext<-as.factor(filmdata$Rate_Ext)
filmdata$Additive<-as.factor(filmdata$Additive)

interaction.plot(filmdata$Rate_Ext,filmdata$Additive,filmdata$Resistance)
interaction.plot(filmdata$Rate_Ext,filmdata$Additive,filmdata$Gloss)
interaction.plot(filmdata$Rate_Ext,filmdata$Additive,filmdata$Opacity)

filmmodel<-manova(cbind(Resistance,Gloss,Opacity)~Rate_Ext*Additive,data=filmdata)
print(filmmodel)

summary.aov(filmmodel)
summary(filmmodel,intercept=TRUE,test="Wilks")
summary(filmmodel,intercept=TRUE, test="Roy")
summary(filmmodel,intercept=TRUE, test="Hotelling-Lawley")


fitIII <- lm(cbind(Resistance,Gloss,Opacity) ~ Rate_Ext*Additive, data=filmdata,
             contrasts=list(Rate_Ext=contr.sum, Additive=contr.sum))
filmmodel <- Manova(fitIII, type="III")
summary(filmmodel, multivariate=TRUE)

## Multivariate Regression Example

y1<-c(3389, 1101, 1131, 596, 896, 1767, 807, 1111, 645, 628, 1360, 652, 860, 500, 781, 1070, 1754)
y2<-c(3149, 653, 810, 448, 844, 1450, 493, 941, 547, 392, 1283, 458, 722, 384, 501, 405, 1520)
x1<-c(1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1)
x2<-c(7500, 1975, 3600, 675, 750, 2500, 350, 1500, 375, 1050, 3000, 450, 1750, 2000, 4500, 1500, 3000)
x3<-c(220, 200, 205, 160, 185, 180, 154, 200, 137, 167, 180, 160, 135, 160, 180, 170, 180)
x4<-c(0, 0, 60, 60, 70, 60, 80, 70, 60, 60, 60, 64, 90, 60, 0, 90, 0)
x5<-c(140, 100, 111, 120, 83, 80, 98, 93, 105, 74, 80, 60, 79, 80, 100, 120, 129)

ami_model<-lm(cbind(y1,y2)~x1+x2+x3+x4+x5)
summary(ami_model)
Manova(ami_model)


roystonTest(residuals(ami_model),qqplot=TRUE)

newdata<-data.frame(x1=1,x2=1200,x3=140,x4=70,x5=85)
predict(ami_model,newdata,interval="prediction")

beta<-coefficients(ami_model)
z0<-matrix(c(1,1,1200,140,70,85),ncol=1)
Zmat<-cbind(c(rep(1,17)),x1,x2,x3,x4,x5)
sigmat<-cov(residuals(ami_model))
m<-2
n<-17
r<-6

for(i in 1:2){
  lower<-t(z0)%*%beta[,i]-sqrt((m*(n-r-1))/(n-r-m)*qf(0.95,m,n-r-m))*sqrt((1+t(z0)%*%solve(t(Zmat)%*%Zmat)%*%z0)*(n/(n-r-1)*sigmat[i,i]))
  upper<-t(z0)%*%beta[,i]+sqrt((m*(n-r-1))/(n-r-m)*qf(0.95,m,n-r-m))*sqrt((1+t(z0)%*%solve(t(Zmat)%*%Zmat)%*%z0)*(n/(n-r-1)*sigmat[i,i]))
  cat(paste("The 95% CI for y",i," is: (",lower,", ",upper,")\n"))
}

### Multivariate GLM Example

GenningsRaw<-read.csv("GenningsData.csv")
GenningsFrame<-data.frame(GenningsRaw)

linmod<-vglm(formula = cbind(Pain.and.Side.effect, Pain.and.No.side.effect, No.Pain.and.Side.effect, No.Pain.and.No.side.effect) ~ x1 + x2, family = multinomial, data = GenningsFrame, weights = m)

summary(linmod)


intmod<-vglm(formula = cbind(Pain.and.Side.effect, Pain.and.No.side.effect, No.Pain.and.Side.effect, No.Pain.and.No.side.effect) ~ x1 * x2, family = multinomial, data = GenningsFrame, weights = m)
summary(intmod)

fullmod<-vglm(formula = cbind(Pain.and.Side.effect, Pain.and.No.side.effect, No.Pain.and.Side.effect, No.Pain.and.No.side.effect) ~ (x1 + I(x1^2)) * (x2 + I(x2^2)), family = multinomial, data = GenningsFrame, weights = m)


lrtest(intmod,linmod)


