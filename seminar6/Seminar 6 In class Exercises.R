
## MANOVA
progdata<-read.csv("C:/Documents/Shorthand.csv")

progdata$Method<-as.factor(progdata$Method)
progdata$Strategy<-as.factor(progdata$Strategy)

interaction.plot(progdata$Method,progdata$Strategy,progdata$Speed)
interaction.plot(progdata$Method,progdata$Strategy,progdata$Accuracy)

progmodel<-manova(cbind(Speed,Accuracy)~Method*Strategy,data=progdata)
print(progmodel)

summary.aov(progmodel)
summary(progmodel,intercept=TRUE,test="Wilks")
summary(progmodel,intercept=TRUE, test="Roy")
summary(progmodel,intercept=TRUE, test="Hotelling-Lawley")


fitIII <- lm(cbind(Speed,Accuracy) ~ Method*Strategy, data=progdata,
             contrasts=list(Method=contr.sum, Strategy=contr.sum))
progmodel <- Manova(fitIII, type="III")
summary(progmodel, multivariate=TRUE)

### Reg

liverdata<-read.csv("C:/Documents/Liver.csv")

liver_model<-lm(cbind(Diploid,Tetraploid,Octaploid)~Age+Sex,data=liverdata)
summary(liver_model)


roystonTest(residuals(liver_model),qqplot=TRUE)

newdata<-data.frame(Age=40,Sex=0)
predict(liver_model,newdata,interval="prediction")

beta<-coefficients(liver_model)
z0<-matrix(c(1,40,0),ncol=1)
Zmat<-cbind(c(rep(1,10)),liverdata$Age,liverdata$Sex)
sigmat<-cov(residuals(liver_model))
m<-3
n<-10
r<-3

for(i in 1:3){
  lower<-t(z0)%*%beta[,i]-sqrt((m*(n-r-1))/(n-r-m)*qf(0.95,m,n-r-m))*sqrt((1+t(z0)%*%solve(t(Zmat)%*%Zmat)%*%z0)*(n/(n-r-1)*sigmat[i,i]))
  upper<-t(z0)%*%beta[,i]+sqrt((m*(n-r-1))/(n-r-m)*qf(0.95,m,n-r-m))*sqrt((1+t(z0)%*%solve(t(Zmat)%*%Zmat)%*%z0)*(n/(n-r-1)*sigmat[i,i]))
  cat(paste("The 95% CI for y",i," is: (",lower,", ",upper,")\n"))
}

## MGLM

caesardata<-read.csv("C:/Users/Steve/Documents/Caesarian.csv")

linmod<-vglm(formula = cbind(TypeI,TypeII,NoInfection) ~ Planned + Risk + Antibiotics, family = multinomial, data = caesardata, weights = m)
summary(linmod)

intmod<-vglm(formula = cbind(TypeI,TypeII,NoInfection) ~ Planned + Risk + Antibiotics +Planned * Risk + Antibiotics * Risk + Planned * Antibiotics, family = multinomial, data = caesardata, weights = m)
summary(intmod)

lrtest(intmod,linmod)
